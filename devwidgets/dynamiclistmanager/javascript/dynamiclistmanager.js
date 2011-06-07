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
/* global $, Config, jQuery, sakai, sdata */

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core", "/dev/javascript/myb/myb.securepage.js"], function($, sakai, myb) {
    /**
     * @name sakai_global.dynamiclistmanager
     *
     * @class dynamiclistmanager
     *
     * @description
     * Dynamic list manager widget
     *
     * @version 0.0.1
     * @param {String} tuid Unique id of the widget
     * @param {Boolean} showSettings Show the settings of the widget or not
     */
    sakai_global.dynamiclistmanager = function(tuid, showSettings) {

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////

        /**
         * Hashtable of all the lists
         */
        var allLists = {};

        /**
         * Dynamic lists base URL (parent node in Sparse), delete this node to remove all dynamic lists for current user
         */
        var dynamicListsBaseUrl;

        //////////////////////
        // jQuery selectors //
        //////////////////////

        /**
         * Widget's root element
         */
        var $rootElement = $("#" + tuid);

        /**
         * Dynamic list 'Edit' button
         */
        var $dynListsEditButton = $("#dyn_lists_edit_button");

        /**
         * Dynamic list 'Copy' button
         */
        var $dynListsCopyButton = $("#dyn_lists_copy_button");

        /**
         * Dynamic list 'delete' button
         */
        var $dynListsDeleteButton = $("#dyn_lists_delete_button");

        /**
         * Dynamic list 'Create' button
         */
        var $dynListsCreateButton = $("#dynamic_lists_create_new");

        var $tableOfLists = $("#list_table");

        //////////////////////////
        // UI related functions //
        //////////////////////////

        /**
         * Shows a general message on the top screen
         * @param {String} msg    the message you want to display
         * @param {Boolean} isError    true for error (red block)/false for normal message(green block)
         */
        var showGeneralMessage = function(msg, isError){

            // Check whether to show an error type message or an information one
            var type = isError ? sakai.api.Util.notification.type.ERROR : sakai.api.Util.notification.type.INFORMATION;

            // Show the message to the user
            sakai.api.Util.notification.show("", msg, type);

        };

        /**
         * Check or uncheck all messages depending on the top checkbox.
         */
        var tickMessages = function(){
            $(".list_list_check_list").attr("checked", ($("#list_list_checkAll").is(":checked") ? "checked" : ''));
            updateEditCopyDeleteButtonsStatus();
        };

        /**
         * Removes all the messages out of the DOM.
         * It will also remove the preloader in the table.
         */
        var removeAllListsOutDOM = function(){
            $(".list_list").remove();
        };


        /**
         * sorts an array of dynamic lists on the date of last modification, this can be used with the JavaScript sort function.
         *
         * @param {Object} a  a dynamic list item
         * @param {Object} b  another dynamic list item
         *
         * @return {Number} Less than 0: Sort "a" to be a lower index than "b";
         *                   Zero: "a" and "b" should be considered equal, and no sorting performed;
         *                   Greater than 0: Sort "b" to be a lower index than "a".
         */
        var sortByDateFunction = function(a, b) {

            return a['_lastModified'] - b['_lastModified'];
        };

        /**
         * Renders loaded lists to the lists table
         */
        var displayLoadedLists = function() {

            //Sort by date
            var listsArray = [];
            for(var key in allLists){
                if(allLists.hasOwnProperty(key)) {
                    listsArray.push(allLists[key]);
                }
            }
            listsArray.sort(sortByDateFunction);



            var data = {
                links: listsArray, //allLists,
                sakai: sakai
            };

            // remove previous lists
            removeAllListsOutDOM();

            // Add them to the DOM
            $tableOfLists.children("tbody").append(sakai.api.Util.TemplateRenderer("#list_list_lists_template", data));

            // do checkboxes
            tickMessages();
        };

        var getNumberOfSelectedLists = function() {
            return $(".list_list_check_list:checked").length;
           };

        var updateEditCopyDeleteButtonsStatus = function() {
          var num = getNumberOfSelectedLists();
          if(num === 0) {
              $dynListsEditButton.attr('disabled', 'disabled');
              $dynListsCopyButton.attr('disabled', 'disabled');
              $dynListsDeleteButton.attr('disabled', 'disabled');
          } else if(num === 1){
              $dynListsEditButton.removeAttr('disabled');
              $dynListsCopyButton.removeAttr('disabled');
              $dynListsDeleteButton.removeAttr('disabled');
          } else if(num > 1){
              $dynListsEditButton.attr('disabled', 'disabled');
              $dynListsCopyButton.attr('disabled', 'disabled');
              $dynListsDeleteButton.removeAttr('disabled');
          }
        };

        var loadDynamicListsFromServer = function() {
            sakai.api.Server.loadJSON(dynamicListsBaseUrl, function(success, data) {
                if (success) {
                    allLists = data;
                    displayLoadedLists();
                } else {
                    allLists = [];
                    displayLoadedLists();
                }

                // TODO: HACK: To prevent flickering this widget was made invisible in HTML code, need to undo this
                $("div.dynamiclistmanager_widget", $rootElement).show();

            });
        };


        ////////////////////////////
        // Dynamic lists deletion //
        ////////////////////////////

         /**
         * This will do a DELETE request to the specified paths and delete each list.
         * @param {String[]} paths The array of dynamic lists that you want to delete.
         */
        var batchDeleteLists = function(paths) {
            var requests = [];
            $(paths).each(function(i,val) {
                var req = {
                    url: val,
                    method: "POST",
                    parameters: {
                        ":operation": "delete"
                    }
                };
                requests.push(req);
            });
            $.ajax({
                url: sakai.config.URL.BATCH,
                traditional: true,
                type: "POST",
                data: {
                    requests: $.toJSON(requests)
                },
                error: function() {
                   showGeneralMessage("Error deleting lists.", true);
                }
            });
        };

        /**
         * Removes lists with provided IDs from DOM and sends batch delete AJAX request.
         *
         *  @param listIds {Array} IDs of lists to delete
         */
        var deleteLists = function(listIds) {

            var paths = []; // paths to nodes to delete
            for (var i = 0, j = listIds.length; i < j; i++) {

                var currentId = listIds[i];

                if(allLists.hasOwnProperty(currentId)) {

                    $("#list_table_list_" + currentId).empty();
                    $("#list_table_list_" + currentId).remove();

                    // store path to delete
                    paths.push(dynamicListsBaseUrl + "/" + currentId);

                    // delete list from memory
                    delete allLists[currentId];
                } else {
                    alert("Error: list \"" + currentId + "\" not found");
                }
            }


            // batch delete nodes selected for deletion
            batchDeleteLists(paths);
        };


        ////////////////////
        // Click handlers //
        ////////////////////

        $dynListsCreateButton.click(function() {
            $.bbq.pushState({"new": true});
        });


        $(".list_list_check_list").live("click", function(){
            updateEditCopyDeleteButtonsStatus();
        });

        // Button click events
        $dynListsDeleteButton.live("click", function(){
            var listId = [];
            $(".list_list_check_list:checked").each(function(){
                var id = $(this).val();
                listId.push(id);
            });

            $("#list_list_checkAll").removeAttr("checked");
            tickMessages();

            if (listId.length < 1) {
                showGeneralMessage($("#list_generalmessages_none_selected").text(), true);
            } else {
                deleteLists(listId);
                loadDynamicListsFromServer();
            }
        });

        $dynListsCopyButton.live("click", function(){
            var listIds = [];
            $(".list_list_check_list:checked").each(function(){
                var id = $(this).val();
                listIds.push(id);
            });

            if (listIds.length === 0) {
                return;
            }

            $("#list_list_checkAll").removeAttr("checked");
            tickMessages();

            // Display new list
            $.bbq.pushState({"copy": listIds[0]});

        });

        $dynListsEditButton.live("click", function(){

            var listIds = [];
            $(".list_list_check_list:checked", $rootElement).each(function(){
                var id = $(this).val();
                listIds.push(id);
            });

            if (listIds.length === 0) {
                return;
            }

            // Display edit list
            $.bbq.pushState({"edit": listIds[0]});
        });

        $(".editLink").live("click", function(evt){

            var id = evt.target.id;

            // Display edit list
            $.bbq.pushState({"edit": id});
        });

        // Check all messages
        $("#list_list_checkAll").change(function(){
            tickMessages();
        });


        ////////////////////////////////
        // Hashchange events handling //
        ////////////////////////////////

        /**
         * Sets current state of this component (list mode or edit mode).
         * This function is called when hashchange event fires.
         */
         var setTabState = function(){

            var state = $.bbq.getState();

            if (state.hasOwnProperty("new") || state.hasOwnProperty("edit") || state.hasOwnProperty("copy")) {
                 $rootElement.hide();
            } else {
                loadDynamicListsFromServer();
                $rootElement.show();
            }
            

        };

         $(window).bind('hashchange', function() {
            setTabState();
        });


        /////////////////////////////
        // Initialization function //
        /////////////////////////////

        /**
         * Initialization function that is run when the widget is loaded. Determines
         * which mode the widget is in (settings or main), loads the necessary data
         * and shows the correct view.
         */
        var doInit = function() {
            var security = sakai.api.Security;

            // if the user is not a member of the advisers group then bail
            if (!myb.api.security.isUserAnAdviser()) {
                security.send403();
                return;
            }

            dynamicListsBaseUrl = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists";

            setTabState();
            loadDynamicListsFromServer();
        };

        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("dynamiclistmanager");

});