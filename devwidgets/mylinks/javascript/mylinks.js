/*
 * Licensed to the Sakai Foundation (SF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The SF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

/*
    global $, Config, jQuery, sakai, sdata, fluid
*/

var sakai = sakai || {};


/**
 * Initialize the My Links widget
 * @param {String} tuid unique id of the widget
 * @param {Boolean} showSettings show the settings of the widget or not
 */

sakai.mylinks = function (tuid, showSettings) {

    // page elements
    var $elm_container = $("#" + tuid);
    var $draggableList = $(".movable_list", $elm_container);

    // selectors
    var draggableElmSelector = "li";

    // Templates
    var mylinksListTemplate = "mylinks_list_template";

    // Data files and paths
    var userLinks = "my_links"
    var linksDataNode = "/_user" + sakai.data.me.profile.path + "/private/" + userLinks;

    /**
     * write the users links to JCR
     * @param {object} the current state of the users list
    */
    var saveLinkList = function (updatedList) {
        sakai.api.Server.saveJSON(linksDataNode, updatedList);
        //sakai.api.Widgets.saveWidgetData(tuid, updatedList);
    };

    /**
     * saves the new list (or list order) to the server
     * @param {Object} a jQuery object containing the moved item
     * @param {Number} an index of the new position
     * @param {Object} a jQuery object containing the current list in the new order
    */
    var saveNewOrder = function (item, requestedPosition, movables) {
        // retrieve objects
        var updatedList = currentListObj(movables);
        // push data to server
        saveLinkList(updatedList);
    };

    /**
     * return the proper js Obj to store on the server
     * @param {Object} a jQuery object containing the current list
    */
    var currentListObj = function (movables) {
        //var listObj = {};
        var list = [];
        var listObj = getDefaultList();

        // loop through movables, retrieve data and return it as an array
        movables.each(function(idx, movable){                         
            var $link = $("a", movable);
            var record = {
                id :   $link.attr("id"),
                name : $link.text(),
                url :  $link.attr("href")
            }
            list.push(record);
        });
        listObj.widget = list;
        return listObj;
    };

    var initDraggables = function () {
        fluid.reorderList($draggableList, {
            selectors: {
                movables: draggableElmSelector
            },
            listeners: {
                afterMove: saveNewOrder
            }
        });
    };

    var createLinkList = function (data, isUserList) {
        $draggableList.html($.TemplateRenderer(mylinksListTemplate, data));
        initDraggables();
    };

    var defaultLinks = {
    colset : [1,2,3,4], 
    featured : ["calmail", "bear_facts", "bspace"],
    widget : [
        {   id : "asuc",
            name : "ASUC",
            url : "http://www.asuc.org"
        },
        {
            id : "atoz_sites",
            name : "Berkeley Sites (A-Z)",
            url : "http://www.berkeley.edu/a-z/a.shtml"
        },
        {
            id : "bear_facts",
            name : "Bear Facts",
            url : "https://bearfacts.berkeley.edu/bearfacts/student/studentMain.do?bfaction=welcome"
        },
        {
            id : "student_services",
            name : "Student Services",
            url : "http://www.berkeley.edu/students"
        },
        {
            id : "bspace",
            name : "bSpace",
            url : "http://bspace.berkeley.edu",
            popup_description : "Homework assignments, lecture slides, syllabi, and class resources."
        },
        {
            id : "calmail",
            name : "CalMail",
            url : "http://calmail.berkeley.edu",
            popup_description : "Read and manage your university email."
        },
        {
            id : "campus_bookstore",
            name : "Campus Bookstore",
            url : "http://www.bkstr.com/CategoryDisplay/10001-9604-10433-1"
        },
        {
            id : "dars",
            name : "DARS",
            url : "https://marin.berkeley.edu/darsweb/servlet/ListAuditsServlet"
        },
        {
            id : "decal",
            name : "DeCal Courses",
            url : "http://www.decal.org"
        },
        {
            id : "public_service",
            name : "Public Service",
            url : "http://calcorps.berkeley.edu"
        },
        {
            id : "class_schedule",
            name : "Schedule of Classes",
            url : "http://schedule.berkeley.edu"
        },
        {
            id : "schedule_planning",
            name : "Schedule Planning Tools",
            url : "http://asuc.org/newsite/scheduleplanning"
        },
        {
            id : "resource_guide",
            name : "Resource Guide for Students",
            url : "http://resource.berkeley.edu"
        },
        {
            id: "tele-bears",
            "name": "Tele-BEARS",
            url: "http://telebears.berkeley.edu"
        }  
    ],
    links : [
        {
            id : "asuc",
            name : "ASUC",
            url : "http://www.asuc.org",
            popup_description : "UC Berkeley's student government home.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "bear_facts",
            name : "Bear Facts",
            url : "https://bearfacts.berkeley.edu/bearfacts/student/studentMain.do?bfaction=welcome",
            popup_description : "See your registration and financial info, grades, online bill, and personal data.",
            featured_description : "Needs featured_description.",
            selected : null,
            audience : [
                {
                    name : "student",
                    required : true
                }
            ]
        },
        {
            id : "atoz_sites",
            name : "Berkeley Sites (A-Z)",
            url : "http://www.berkeley.edu/a-z/a.shtml",
            popup_description : "Comprehensive list of official campus websites.",
            featured_description : "Needs featured_description.",
            selected : null,
            audience : [
                {
                    name : "student",
                    required : true
                }
            ]
        },
        {
            id : "student_services",
            name : "Student Services",
            url : "http://www.berkeley.edu/students",
            popup_description : "Several student sites grouped by function.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "bspace",
            name : "bSpace",
            url : "http://bspace.berkeley.edu",
            popup_description : "Homework assignments, lecture slides, syllabi, and class resources.",
            featured_description : "Needs featured_description.",
            selected : null,
            audience : [
                {
                    name : "student",
                    required : true
                }
            ]
        },
        {
            id : "cal1card",
            name : "Cal-1-Card",
            url : "http://services.housing.berkeley.edu/c1c/static/aboutc1c.htm",
            popup_description : "Manage and learn about your Cal 1 Card.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "caldining",
            name : "CalDining",
            url : "http://caldining.berkeley.edu",
            popup_description : "View daily menus and check your meal points.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "calmail",
            name : "CalMail",
            url : "http://calmail.berkeley.edu",
            popup_description : "Read and manage your university email.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "campus_calendar",
            name : "Campus Events Calendar",
            url : "http://events.berkeley.edu",
            popup_description : "View upcoming events on campus.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "campus_map",
            name : "Campus Map",
            url : "http://berkeley.edu/map",
            popup_description : "View campus maps and find buildings using the interactive map.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "campus_safety",
            name : "Campus Safety / Police",
            url : "http://police.berkeley.edu",
            popup_description : "Learn about UCPD services.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "campus_bookstore",
            name : "Campus Bookstore",
            url : "http://www.bkstr.com/CategoryDisplay/10001-9604-10433-1",
            popup_description : "Find and purchase textbooks by course.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },       
        {
            id : "course_catalog",
            name : "Course Catalog",
            url : "http://sis.berkeley.edu/catalog/gcc_search_menu",
            popup_description : "Detailed course descriptions.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "dars",
            name : "DARS",
            url : "https://marin.berkeley.edu/darsweb/servlet/ListAuditsServlet",
            popup_description : "Track progress toward a major.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "decal",
            name : "DeCal Courses",
            url : "http://www.decal.org",
            popup_description : "Explore student-taught courses.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "finaid",
            name : "Financial Aid",
            url : "http://students.berkeley.edu/finaid",
            popup_description : "Learn about financial aid and scholarships.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },        
        {
            id : "future_campus_calendars",
            name : "Future Campus Calendars",
            url : "http://opa.berkeley.edu/AcademicCalendar",
            popup_description : "Look at campus calendars for upcoming semesters.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "uhs",
            name : "University Health Services",
            url : "http://uhs.berkeley.edu",
            popup_description : "Health and medical services for students.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },        
        {
            id : "career_center",
            name : "Career Center",
            url : "http://career.berkeley.edu",
            popup_description : "Find jobs, internships, and learn about career paths.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "library",
            name : "Library",
            url : "http://www.lib.berkeley.edu",
            popup_description : "Search for materials in the UC library system.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },        
        {
            id : "parking",
            name : "Parking",
            url : "http://pt.berkeley.edu/park",
            popup_description : "Find and pay for parking.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },                
        {
            id : "public_service",
            name : "Public Service",
            url : "http://calcorps.berkeley.edu",
            popup_description : "Get involved in public service.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "rsf",
            name : "Recreational Sports Facility",
            url : "http://www.recsports.berkeley.edu",
            popup_description : "Explore RSF programs and intramural sports.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },        
        {
            id : "rescomp",
            name : "Residential Computing",
            url : "http://rescomp.berkeley.edu",
            popup_description : "Computing help for the residence halls.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },                                                 
        {
            id : "class_schedule",
            name : "Schedule of Classes",
            url : "http://schedule.berkeley.edu",
            popup_description : "See current and upcoming courses.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "schedule_planning",
            name : "Schedule Planning Tools",
            url : "http://asuc.org/newsite/scheduleplanning",
            popup_description : "Tools to help you plan your class schedule.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "student_groups",
            name : "Student Groups and Programs",
            url : "http://students.berkeley.edu/osl",
            popup_description : "Student organizations, leadership programs, and GenEq.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },
        {
            id : "resource_guide",
            name : "Resource Guide for Students",
            url : "http://resource.berkeley.edu",
            popup_description : "Information about campus resources.",
            featured_description : "Needs featured_description.",
            selected : false,
            audience : [
                {
                    name : "student",
                    required : false
                }
            ]
        },                   
        {
            id: "tele-bears",
            "name": "Tele-BEARS",
            url: "http://telebears.berkeley.edu",
            "popup_description": "Register for classes.",
            featured_description: "Needs featured_description.",
            selected : null,
            "audience": [{
                "name": "student",
                required: true
            }]
        }
    ]
};

    var getDefaultList = function () {
         return defaultLinks;
    };

    /**
     * Retreives the current list of links for the current user
     * if the user does have any links, returns the default list
     * @param {Boolean} whether the loadJSON got it's data
     * @param {Object} contains the users links record
     */

    var loadLinksList = function (success, data) {
        if (success) {
            // load the users link list from data
            createLinkList(data, true);
         } else {
            // load the default link list and use that
            var defaultLinksList = getDefaultList();
            createLinkList(defaultLinksList, false);
            // save the default link list back to the server
            saveLinkList(defaultLinks);
        }
    };

    var doInit = function() {
        sakai.api.Server.loadJSON(linksDataNode, loadLinksList);
    };

    doInit();

};

sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");