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
     * @param {object} the current state of the users links list
    */
    var saveLinkList = function (updatedList) {
        sakai.api.Server.saveJSON(linksDataNode, updatedList);
    };

    /**
     * saves the new list (or list order) to the server
     * @param {Object} a jQuery object containing the moved item
     * @param {Number} an index of the new position
     * @param {Object} a jQuery object containing the current list in the new order
    */
    var saveNewOrder = function (item, requestedPosition, movables) {

        // retreave objects
        var updatedList = currentListObj(movables);

        // push data to server
        saveLinkList(updatedList);
    };

    /**
     * return the proper js Obj to store on the server
     * @param {Object} a jQuery object containing the current list
    */
    var currentListObj = function (movables) {
        var listObj = {};
        var list = [];

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
        listObj.links = list;

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
        links : [
            {
                id :   "bear_facts",
                name : "Bear Facts",
                url :  "https://bearfacts.berkeley.edu/bearfacts/student/studentMain.do?bfaction=welcome"
            },
            {
                id :   "bspace",
                name : "bSpace",
                url :  "http://bspace.berkeley.edu"
            },
            {
                id :   "calmail",
                name : "CalMail",
                url :  "http://calmail.berkeley.edu"
            },
            {
                id :   "schedule_of_classes",
                name : "Schedule of Classes",
                url :  "http://schedule.berkeley.edu"
            },
            {
                id :   "tele-bears",
                name : "Tele-BEARS",
                url :  "http://telebears.berkeley.edu"
            },
            {
                id :   "campus_textbook_store",
                name : "Campus Textbook Store",
                url :  "http://www.bkstr.com/CategoryDisplay/10001-9604-10433-1"
            },
            {
                id :   "how_to_pick_classes",
                name : "How to pick classes / plan your schedule",
                url :  "http://asuc.org/newsite/scheduleplanning"
            },
            {
                id :   "public_service",
                name : "Public Service",
                url :  "http://calcorps.berkeley.edu/"
            },
            {
                id :   "asuc",
                name : "ASUC (Student Government)",
                url :  "http://www.asuc.org"
            },
            {
                id :   "decal",
                name : "DeCal courses",
                url :  "http://www.decal.org/"
            }
        ]
    };

    var getDefaultList = function () {
         return defaultLinks;
    };

    /**
     * initializes a list of links for the user
     * if the user does have any links, uses the default list and saves it back to the users data
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

    /**
     * Set up the widget
     * grab the users link data, then fire callback loadLinksList
     * @param {Boolean} whether the loadJSON got it's data
     * @param {Object} contains the users links record
     */

    var doInit = function() {
        sakai.api.Server.loadJSON(linksDataNode, loadLinksList);
    };

    doInit();

};

sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");