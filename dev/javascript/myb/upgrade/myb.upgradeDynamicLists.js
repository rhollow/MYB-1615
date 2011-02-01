/**
 * utility to upgrade dynamic lists from the old flat structure to the
 * new nested structure that has a different major set for each standing
 *  old list query:
 *	context:[g-ced-students"]
 *  major:["ARCHITECTURE", "LANDSCAPE ARCH"]
 *  standing:["undergrad", "grad"]
 *  
 *  new list query:
 *   context: ["g-ced-students"]
 *	 standing: [{'undergrad':{'major':["ARCHITECTURE", "LANDSCAPE ARCH"]}},
 *              {'grad':{'major':["ARCHITECTURE", "LANDSCAPE ARCH"]}}]
 */

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core"], function($, sakai, myb) {

    sakai_global.upgradeDynamicLists = function() {
        var GET_LIST_URL = "/private/dynamic_lists.infinity.json";
        var SAVE_LIST_URL = "/private/dynamic_lists";
        var DELETE_LIST_URL = "/private/dynamic_lists/lists";

        var UNDERGRAD_MAJORS = [ "ARCHITECTURE", "INDIVIDUAL", "LIMITED",
                "LANDSCAPE ARCH", "URBAN STUDIES" ];
        var GRAD_MAJORS = [ "ARCHITECTURE", "CITY REGIONAL PLAN", "DESIGN",
                "LIMITED", "LAND ARCH AND ENV PLAN", "URBAN DESIGN" ];

        var dryRun = true;
        var advisors = [];

        var logData = function(doit, data) {
            console.log("g-ced-advisors are: ")
            console.log(data);
            advisors = data;
        };

        // get the advisors who may have old dynamic lists
        var loadAdvisors = function() {
            sakai.api.Groups.getMembers("g-ced-advisors", logData);
        };

        // save the new transformed lists
        var saveLists = function(lists, advisorId) {
            var submitData = {};
            if (dryRun === false) {
                console.log("saving new lists for advisor " + advisorId);
                var url = "/~" + advisorId + SAVE_LIST_URL;
                submitData.lists = lists
                sakai.api.Server.saveJSON(url, submitData);
            }
            else {
                console.log("This is a dry run, not saving new lists");
            }
        }

        // delete all lists for advisor
        var deleteLists = function(advisorId) {
            if (dryRun === false) {
                console.log("deleting all lists for advisor " + advisorId);
                var url = "/~" + advisorId + DELETE_LIST_URL;
                sakai.api.Server.removeJSON(url);
            }
            else {
                console.log("This is a dry run, not removing any lists");
            }
        }

        // the callback from loadJSON, transform list structure if
        // the advisor has any lists and log if advisor has no lists
        var handleLists = function(doit, data, advisorId) {
            var list, lists;
            if (data && data.lists) {
                lists = data.lists;
                console.log(data.lists);
                for ( var i = 0, len = lists.length; i < len; i++) {
                    list = lists[i];
                    // this is the old structure, the new one has a nested major
                    // node so list.query.major will only be true for old lists
                    if (list.query.major) {
                        list = updateList(list);
                    }
                    lists[i] = list;
                }

                // Skipping all non-upgradable lists
                // No supported in IE
                var listsToSave = lists.filter(function(x) {
                   return x !== null;
                });

                // Do we have any upgraded lists at all?
                if(listsToSave.length > 0) {
                    saveLists(listsToSave, advisorId);
                } else {
                    // Deleting all lists for this advisor
                    deleteLists(advisorId);
                    console.warn("Advisor " + advisorId + " has lists, but none of then can be upgraded. All existing non-upgradable lists for this advisor were deleted");
                }

            } else {
                console.log("advisor " + advisorId + " has no lists to upgrade");
            }
        };

        // change the old list structure to the new
        var updateList = function(list) {
            // old list query
            // context:[g-ced-students"]
            // major:["ARCHITECTURE", "LANDSCAPE ARCH"]
            // standing:["undergrad", "grad"]
            console.log("old list: ");
            console.log(list);
            var standing, newStanding, filteredMajors;
            var newStandings = [];
            var majors = list.query.major;
            var standings = list.query.standing;
            for ( var i = 0; i < standings.length; i++) {
                standing = standings[i];
                newStanding = {};
                filteredMajors = filterMajors(standing, majors);
                if(filteredMajors.length === 0) {
                    console.warn("List with id '" + list["sakai:id"] + "' will not contain any majors after conversion, skipping...");
                    return null;
                }
                newStanding[standing] = {
                    'major' : filteredMajors
                };
                newStandings[i] = newStanding;
            }
            // new list query
            // context: ["g-ced-students"]
            // standing: [{'undergrad':{'major':["ARCHITECTURE", "LANDSCAPE
            // ARCH"]}},{'grad':{'major':["ARCHITECTURE", "LANDSCAPE ARCH"]}}]
            delete list.query.major;
            list.query.standing = newStandings;
            console.log("new list: ");
            console.log(list);
            return list;
        };

        // make sure the new major arrays have the specific majors for each standing
        var filterMajors = function(standing, majors) {
            var filteredMajors = [];
            for ( var i = 0; i < majors.length; i++) {
                if (containsMajor(standing, majors[i])) {
                    filteredMajors.push(majors[i]);
                }
            }
            return filteredMajors;
        }

        // check the arrays of defined majors
        var containsMajor = function(standing, major) {
            var containsMajor = false;
            var targetArr;
            if (standing === "undergrad") {
                targetArr = UNDERGRAD_MAJORS;
            } else if (standing === "grad") {
                targetArr = GRAD_MAJORS;
            } else {
                console.log("cannot filter majors for standing " + standing)
            }
            if (targetArr) {
                for ( var i = 0; i < targetArr.length; i++) {
                    if (targetArr[i] === major) {
                        containsMajor = true;
                    }
                }
            }
            return containsMajor;
        }

        // retrieve any lists for each advisor
        var loadListsForAdvisor = function(advisorId) {
            var url = "/~" + advisorId + GET_LIST_URL;
            console.log("loading lists for advisor " + advisorId);
            sakai.api.Server.loadJSON(url, function(doit, data) {
                handleLists(doit, data, advisorId)
            });
        };

        // load the advisors on page load and
        // load and transform the lists when clicking on the anchor "button"
        var doInit = function() {
            var querystring = new Querystring();
            if (querystring.contains("dryRun")
                    && querystring.get("dryRun") === "false") {
                dryRun = false;
            }
            loadAdvisors();
            $("#upgrade_dynamiclists_button").live("click", function() {
                console.log("Running upgrade; dryRun=" + dryRun);
                for ( var i = 0; i < advisors.length; i++) {
                    var advisorId = advisors[i]["rep:userId"];
                    loadListsForAdvisor(advisorId);
                }
            });
        };

        doInit();
    };
    sakai.api.Widgets.Container.registerForLoad("upgradeDynamicLists");
});
