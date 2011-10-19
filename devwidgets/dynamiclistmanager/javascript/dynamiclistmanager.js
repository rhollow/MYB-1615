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

        var LISTS_PER_PAGE = 100;

        var currentPage = 0;
        
        var savedRowID = "";


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
         * Dynamic list Action buttons
         */
        var $dynListsEditButton = $(".dyn_lists_edit_button", $rootElement);
        var $dynListsCopyButton = $(".dyn_lists_copy_button", $rootElement);
        var $dynListsDeleteButton = $(".dyn_lists_delete_button", $rootElement);

        /**
         * Dynamic list 'Create' button
         */
        var $dynListsCreateButton = $("#dynamic_lists_create_new");

        /**
         * Dynamic list dropdown
         */
        var $dynListsContextSelect = $("#dlm_context_choice_select");
        
        var contexts = sakai.data.me.dynamiclistcontexts;
        var multipleContexts = contexts.length > 1;


        var $tableOfLists = $("#list_table");

        var $listPager = $("#list_pager", $rootElement);
        var $listPagerContainer = $(".bottom_decor", $rootElement);

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
        * uses OEA applyThreeDots function to force the width of the object used for truncation
        * not currently used but left here in case we want to bring back the description or truncate something else
        *
        * @param {String} containerSelector a valid jQuery select to find all the text elements
        * @param {Integer} widthAdjustment  adjustment usually down to make the truncation a little more aggressive, -10 is a nice number
        * @param {Integer} numRows          how many rows to show in the container
        * @param {Bool}    wholeWordBool    true or false, break on whole words or no
        * 
        */
        var truncateElemText = function(containerSelector, widthAdjustment, numRows, wholeWordBool) {
            widthAdjustment = (widthAdjustment) ? Number(widthAdjustment) : 0;
            numRows = (numRows) ? Number(numRows) : 1;
            var jQElements = $(containerSelector, $rootElement);
            var theWidth = jQElements.eq(0).innerWidth() + widthAdjustment;
            var currElem = {};
            $(jQElements).each(function () {
                currElem = $(this);
                currElem.text(sakai.api.Util.applyThreeDots(currElem.text(), theWidth, {max_rows: numRows, whole_word: wholeWordBool}));
            });
        };
   

        /**
         * Renders loaded lists to the lists table
         */
        var displayLoadedLists = function() {

            //Sort by date
            var listsArray = [];
            for(var key in allLists){
                if(allLists.hasOwnProperty(key)) {
                    if ( key.substr(0, 3) === "dl-") {
                        listsArray.push(allLists[key]);
                    }
                }
            }

            // TODO: commented out till the issues with paging and sorting are resolved
            // See MYB-1022
            // listsArray.sort(sortByDateFunction);

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
            
            hiliteRow();
        };

        var getNumberOfSelectedLists = function() {
            return $(".list_list_check_list:checked").length;
        };

        var updateEditCopyDeleteButtonsStatus = function() {
          var num = getNumberOfSelectedLists();
          if(num === 0) {
              $dynListsEditButton.attr('disabled', 'disabled');
              $dynListsEditButton.addClass("disabled");
              $dynListsCopyButton.attr('disabled', 'disabled');
              $dynListsCopyButton.addClass("disabled");
              $dynListsDeleteButton.attr('disabled', 'disabled');
              $dynListsDeleteButton.addClass("disabled");
          } else if(num === 1){
              $dynListsEditButton.removeAttr('disabled');
              $dynListsEditButton.removeClass("disabled");
              $dynListsCopyButton.removeAttr('disabled');
              $dynListsCopyButton.removeClass("disabled");
              $dynListsDeleteButton.removeAttr('disabled');
              $dynListsDeleteButton.removeClass("disabled");
          } else if(num > 1){
              $dynListsEditButton.attr('disabled', 'disabled');
              $dynListsEditButton.addClass("disabled");
              $dynListsCopyButton.attr('disabled', 'disabled');
              $dynListsCopyButton.addClass("disabled");
              $dynListsDeleteButton.removeAttr('disabled');
              $dynListsDeleteButton.removeClass("disabled");
          }
        };

        var loadDynamicListsFromServer = function() {
            var url = dynamicListsBaseUrl + ".1.json?items=" + LISTS_PER_PAGE + "&page=" + currentPage;
            sakai.api.Server.loadJSON(url, function(success, data) {
                if (success) {
                    allLists = data;
                    displayLoadedLists();
                } else {
                    allLists = [];
                    displayLoadedLists();
                }

                // only show pager if needed
                if (data.total > LISTS_PER_PAGE) {
                    // pagenumber is 1-indexed, currentPage is 0-indexed
                    $listPager.pager({ pagenumber: currentPage+1, pagecount: Math.ceil(data.total/LISTS_PER_PAGE), buttonClickCallback: handlePageClick });
                    $listPagerContainer.show();
                } else {
                    currentPage = 0;
                    $listPagerContainer.hide();
                }

                // TODO: HACK: To prevent flickering this widget was made invisible in HTML code, need to undo this
                $("div.dynamiclistmanager_widget", $rootElement).show();
                
            });
        };

        /**
         * Handle click on paging controls, the pager callback function
         */
        var handlePageClick = function(pageNum) {
            currentPage = pageNum - 1;
            $.bbq.pushState({"currentPage": currentPage});
            loadDynamicListsFromServer();
        };

        ////////////////////////////
        // Dynamic lists deletion //
        ////////////////////////////

         /**
         * This will do a DELETE request to the specified paths and delete each list.
         * @param {String[]} paths The array of dynamic lists that you want to delete.
         * @param successCallback Function to call if deletion was successful.
         */
        var batchDeleteLists = function(paths, successCallback) {
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
                success: function(){
                    if ($.isFunction(successCallback)){
                        successCallback();
                    }
                },
                error: function() {
                   showGeneralMessage("Error deleting lists.", true);
                }
            });
        };

        /**
         * Removes lists with provided IDs from DOM and sends batch delete AJAX request.
         *
         *  @param listIds {Array} IDs of lists to delete.
         *  @param successCallback Function to call if deletion was successful.
         */
        var deleteLists = function(listIds, successCallback) {

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
            batchDeleteLists(paths, successCallback);
        };


        ////////////////////
        // Click handlers //
        ////////////////////

        $dynListsCreateButton.click(function() {
            var context = (multipleContexts) ? $dynListsContextSelect.val() : sakai.data.me.dynamiclistcontexts[0].name;
            $.bbq.pushState({"new": true, "context": context});
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
                // TODO: Temporary fix for MYB-1016
                currentPage = 0;
                $.bbq.removeState("currentPage");

                deleteLists(listId, loadDynamicListsFromServer);
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
            $.bbq.pushState({"cp": listIds[0]});

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
        
                
        var removeRowHilite = function (jqRowObj) {
            jqRowObj.removeClass("saved-elm-hilite");
        };
        
        
        /**
         * highlights the last saved message in the table if the id is set in the namespace.
         */
        var hiliteRow = function () {
            if (savedRowID) {
                var jqRowObj = $("tr#list_table_list_" + savedRowID, $rootElement);
                jqRowObj.addClass("saved-elm-hilite");
                setTimeout(function () {
                    removeRowHilite(jqRowObj);
                }, 1500);
            }
            savedRowID = "";
        };



        ////////////////////////////////
        // Hashchange events handling //
        ////////////////////////////////

        /**
         * Sets current state of this component.
         * This function is called when hashchange event fires.
         */
         var setState = function(){

            var state = $.bbq.getState();
            
            savedRowID = (state.hasOwnProperty("saved")) ? state.saved : "";

            if (!(state.hasOwnProperty("l") && state.l === "dynlists")) {
                return;
            }

            if ( state.hasOwnProperty("currentPage")) {
                currentPage = parseInt(state.currentPage);
            }

            if (state.hasOwnProperty("new") || state.hasOwnProperty("edit") || state.hasOwnProperty("cp")) {
                 $rootElement.hide();
            } else {
                loadDynamicListsFromServer();
                $rootElement.show();
            }
            

        };

         $(window).bind('hashchange', function() {
            setState();
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

            setState();
            
            /* Initialize context menu */

            if (multipleContexts) {
                // Display multiple choice drop-down menu
                
                for (var key in contexts) {
                    var c = contexts[key];
                    $dynListsContextSelect.append('<option value="' + c.name + '">'+c.name+'</option>');
                }

                $("#dlm_multiple_choice_container", $rootElement).show();
            }
        };

        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("dynamiclistmanager");

});