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

    // templates
    var mylinksListTemplate = "mylinks_list_template";

    // data files and paths
    var userLinks = "my_links";
    var linksDataNode = "/~" + sakai.data.me.user.userid + "/private/" + userLinks;

    // path for the default list of links to display in the widget
    var defaultLinksPath = "/var/defaults/mylinks/mylinks-defaults.json";

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
        var listObj = {};
        var list = [];

        // loop through movables, retrieve data and return it as an array
        movables.each(function(idx, movable){
            var $link = $("a", movable);
            var record = {
                id :   $link.attr("id"),
                name : $link.text(),
                url :  $link.attr("href")
            };
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

    var createLinkList = function (data) {
        $draggableList.html($.TemplateRenderer(mylinksListTemplate, data));
		
		// Add Google Analytics outbound links tracking
		$("div.mylinks_widget li.link a").click(function () {
			sakai.myb.api.google.recordOutboundLink(this, 'Outbound Links', $(this).attr('href'));
			return false;			
		});		
		
        initDraggables();
    };

    /**
     * retrieve default data, use that and save it back
     */
    var loadDefaultList = function () {
        $.ajax({
            url: defaultLinksPath,
            cache: false,
            dataType: "json",
            success: function(data){
                var parsedData = data;
                // create the link list from the default list and then save it back
                createLinkList(parsedData);
                // save the default link list back to the server
                saveLinkList(parsedData);
            },
            error: function(xhr, textStatus, thrownError) {
                    //alert("An error has occured");
            }
        });
    };


    /**
     * Set up the widget
     * grab the users link data, then fire callback loadLinksList
     * @param {Boolean} whether the loadJSON got it's data
     * @param {Object} contains the users links record
     */

    var doInit = function() {
        sakai.api.Server.loadJSON(linksDataNode, function (success, data) {
            if (success) {
                // load the users link list from their saved link data
                createLinkList(data);
            } else {
                // else retrieve default data, use that and save it back
                loadDefaultList();
            }

        });
    };

    doInit();

};

sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");