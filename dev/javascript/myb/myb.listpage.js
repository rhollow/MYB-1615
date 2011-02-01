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

/* global $, Config, opensocial */

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core", "/dev/lib/myb/jquery/jquery-ui-tabs-min.js"], function($, sakai, myb) {

    sakai_global.listpage = function(){
        /**
         *
         * Configuration
         *
         */
        var submitData = {}; // Data to be passed to saveJSON
        var allLists = []; // Array of all the lists
        var userUrl;
        var generalMessageFadeOutTime = 3000; // The amount of time it takes till the general message box fades out
        var sortBy = "name";
        var sortOrder = "descending";
        var editExisting = false;
        var currList;


        /**
         *
         * CSS IDs
         *
         */
        var inboxID = "#inbox";
        var inboxClass = ".inbox";
        var inbox = "inbox";
        var inboxArrow = inboxClass + "_arrow";
        var inboxTable = inboxID + "_table";
        var inboxTablePreloader = inboxTable + "_preloader";
        var inboxGeneralMessage = inboxID + "_general_message";
        var inboxMessageError = inbox + "_error_message";

        /**
         * This will show the preloader.
         */
        var showLoader = function(){
            $(inboxTable).append(sakai.api.Util.TemplateRenderer(inboxTablePreloader.substring(1), {}));
        };

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
            $(".inbox_inbox_check_list").attr("checked", ($("#inbox_inbox_checkAll").is(":checked") ? "checked" : ''));
        };

        var getDataFromInput = function() {
            var result = {};

            result.context = "g-ced-students";
            result.listName = $.trim($("#list_name").val());
            if(result.listName == null) {
                $("#invalid_name").show();
                return -1;
            }
            result.desc = $.trim($("#description").val());

            // NOT SUPPORTED FOR POC
            // result.size = $("#list_size").val();

            // Gathering the data on standing
            var standingArray = [];
            if($("#undergrad:checked").val() == null && $("#grad:checked").val() == null) {
                $("#invalid_major").show();
                    return -1;
            }

            // Gathering the data on majors
            var selectedUndergradMajors = [];
            if ($("#undergrad:checked").val() && $("#byMajor:checked").val()) {
                $("#ugrad_majors .major_checkbox:checked").each(function() {
                    var major = $.trim($(this).val());
                    selectedUndergradMajors.push(major);
                });
                if(selectedUndergradMajors.length == 0) {
                    $("#invalid_major").show();
                    return -1;
                }
            } else if($("#undergrad:checked").val() && $("#allUgrads:checked").val()) {
                $("#ugrad_majors .major_checkbox").each(function() {
                    var major = $.trim($(this).val());
                    selectedUndergradMajors.push(major);
                });
            }
            // Append majors only if something was selected
            if(selectedUndergradMajors.length > 0) {
                standingArray.push({
                    "undergrad": {
                        "major": selectedUndergradMajors
                    }
                });
            }

            var selectedGradMajors = [];
            if ($("#grad:checked").val() && $("#byProgram:checked").val()) {
                $("#grad_majors .major_checkbox:checked").each(function(){
                    var major = $.trim($(this).val());
                    selectedGradMajors.push(major);
                });
                if(selectedGradMajors.length == 0) {
                    $("#invalid_major").show();
                    return -1;
                }
            } else if($("#grad:checked").val() && $("#allGrads:checked").val()) {
                $("#grad_majors .major_checkbox").each(function() {
                    var major = $.trim($(this).val());
                    selectedGradMajors.push(major);
                });
            }
            // Append majors only if something was selected
            if(selectedGradMajors.length > 0) {
                standingArray.push({
                    "grad": {
                        "major": selectedGradMajors
                    }
                });
            }

            result.standing = standingArray;
            return result;
        }

        // NOT SUPPORTED FOR POC
        /**
         * Updates the input field that displays the current size of the list being created
         */
        sakai_global.listpage.updateListSize = function(){
            var data = getDataFromInput();
            if(data < 0) {
                alert("Error");
            }

            // STILL NEEDS TO BE IMPLEMENTED
            // var size = query(selectedContext, selectedMajor, selectedStanding);
            // document.createListForm.list_size.value = size;
        }

        /**
         * Clears input fields
         */
        var clearInputFields = function() {
            document.getElementById("createListForm").reset();
        };

        // TODO: Document properties.
        /**
         * Used for the date formatter.
         */
        var replaceChars = {
            date: new Date(),
            shortMonths: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'],
            longMonths: ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'],
            shortDays: ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'],
            longDays: ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'],

            // Day
            d: function() {
                return (replaceChars.date.getDate() < 10 ? '0' : '') + replaceChars.date.getDate();
            },
            D: function() {
                return replaceChars.shortDays[replaceChars.date.getDay()];
            },
            j: function() {
                return replaceChars.date.getDate();
            },
            l: function() {
                return replaceChars.longDays[replaceChars.date.getDay()];
            },
            N: function() {
                return replaceChars.date.getDay() + 1;
            },
            S: function() {
                return (replaceChars.date.getDate() % 10 === 1 && replaceChars.date.getDate() !== 11 ? 'st' : (replaceChars.date.getDate() % 10 === 2 && replaceChars.date.getDate() !== 12 ? 'nd' : (replaceChars.date.getDate() % 10 === 3 && replaceChars.date.getDate() !== 13 ? 'rd' : 'th')));
            },
            w: function() {
                return replaceChars.date.getDay();
            },
            z: function() {
                return "Not Yet Supported";
            },
            // Week
            W: function() {
                return "Not Yet Supported";
            },
            // Month
            F: function() {
                return replaceChars.longMonths[this.getMonth()];
            },
            m: function() {
                return (replaceChars.date.getMonth() < 11 ? '0' : '') + (replaceChars.date.getMonth() + 1);
            },
            M: function() {
                return replaceChars.shortMonths[replaceChars.date.getMonth()];
            },
            n: function() {
                return replaceChars.date.getMonth() + 1;
            },
            t: function() {
                return "Not Yet Supported";
            },
            // Year
            L: function() {
                return "Not Yet Supported";
            },
            o: function() {
                return "Not Supported";
            },
            Y: function() {
                return replaceChars.date.getFullYear();
            },
            y: function() {
                return ('' + replaceChars.date.getFullYear()).substr(2);
            },
            // Time
            a: function() {
                return replaceChars.date.getHours() < 12 ? 'am' : 'pm';
            },
            A: function() {
                return replaceChars.date.getHours() < 12 ? 'AM' : 'PM';
            },
            B: function() {
                return "Not Yet Supported";
            },
            g: function() {
                return replaceChars.date.getHours() % 12 || 12;
            },
            G: function() {
                return replaceChars.date.getHours();
            },
            h: function() {
                return ((replaceChars.date.getHours() % 12 || 12) < 10 ? '0' : '') + (replaceChars.date.getHours() % 12 || 12);
            },
            H: function() {
                return (replaceChars.date.getHours() < 10 ? '0' : '') + replaceChars.date.getHours();
            },
            i: function() {
                return (replaceChars.date.getMinutes() < 10 ? '0' : '') + replaceChars.date.getMinutes();
            },
            s: function() {
                return (replaceChars.date.getSeconds() < 10 ? '0' : '') + replaceChars.date.getSeconds();
            },
            // Timezone
            e: function() {
                return "Not Yet Supported";
            },
            I: function() {
                return "Not Supported";
            },
            O: function() {
                return (replaceChars.date.getTimezoneOffset() < 0 ? '-' : '+') + (replaceChars.date.getTimezoneOffset() / 60 < 10 ? '0' : '') + (replaceChars.date.getTimezoneOffset() / 60) + '00';
            },
            T: function() {
                return "Not Yet Supported";
            },
            Z: function() {
                return replaceChars.date.getTimezoneOffset() * 60;
            },
            // Full Date/Time
            c: function() {
                return "Not Yet Supported";
            },
            r: function() {
                return replaceChars.date.toString();
            },
            U: function() {
                return replaceChars.date.getTime() / 1000;
            }
        };

        /**
         * Format a date to a string.
         * See replaceChars for the specific options.
         * @param {Date} d
         * @param {String} format
         */
        var formatDate = function(d, format){
            var returnStr = '';
            replaceChars.date = d;
            var replace = replaceChars;
            for (var i = 0; i < format.length; i++) {
                var curChar = format.charAt(i);
                if (replace[curChar]) {
                    returnStr += replace[curChar].call(d);
                }
                else {
                    returnStr += curChar;
                }
            }
            return returnStr;
        };

        sakai_global.listpage.getDate = function(dateString){
            var d = sakai.api.Util.parseSakaiDate(dateString);
            d = formatDate(d, "M j, Y g:i A");
            return d;
        };

        /**
         * Removes all the messages out of the DOM.
         * It will also remove the preloader in the table.
         */
        var removeAllListsOutDOM = function(){
            $(".inbox_list").remove();
        };

        var renderLists = function(response){
            allLists = response.lists || [];

            var data = {
                "links": allLists
            }

            // remove previous lists
            removeAllListsOutDOM();

            // Add them to the DOM
            $(inboxTable).children("tbody").append(sakai.api.Util.TemplateRenderer("#inbox_inbox_lists_template", data));

            // do checkboxes
            tickMessages();
        }

        /**
         * Get the dynamic list out of the list with the specific id.
         * @param {String} id    The id of a dynamic list
         */
        var getListWithId = function(id){
            for (var i = 0, j = allLists.length; i < j; i++) {
                if (allLists[i]["sakai:id"] === id) {
                    return allLists[i];
                }
            }
            return null;
        };

        /**
         * Returns the index of the list with the specific id.
         * @param {Object} id
         */
        var getIndexFromId = function(id) {
            for (var i = 0, j = allLists.length; i < j; i++) {
                if (allLists[i]["sakai:id"] === id) {
                    return i;
                }
            }
            return -1;
        }

        /**
         * Displays only the list with that id.
         * @param {String} id    The id of a list
         */
        var displayList = function(id){
            // Display edit list tab
            $.bbq.pushState({"tab": "new"},2);
            clearInputFields();

            // Fill in input fields with list data
            var list = getListWithId(id);
            document.createListForm.list_name.value = list["sakai:name"];
            document.createListForm.description.value = list["sakai:description"];

            // NOT SUPPORTED FOR POC
            // document.createListForm.list_size.value = list["sakai:size"];

            var ugradMajorArray = [];
            var gradMajorArray = [];

            var listStanding = list.query.standing;
            for(var i = 0, j = listStanding.length; i < j; i++) {
                if(listStanding[i].hasOwnProperty("undergrad")) {
                    ugradMajorArray = listStanding[i].undergrad.major;
                } else if(listStanding[i].hasOwnProperty("grad")) {
                    gradMajorArray = listStanding[i].grad.major;
                }
            }

            if(ugradMajorArray != null && ugradMajorArray.length > 0) {
                $("#undergrad").click();
                enableAll(document.getElementById("choose_ugrad"));

                for (var i = 0, j = ugradMajorArray.length; i < j; i++) {
                    var major = ugradMajorArray[i].replace(/ /g, "_");
                    $("#ugrad_majors #" + major + "_M").attr("checked", true);
                }

                var allChecked = true;
                $("#ugrad_majors .major_checkbox").each(function(){
                    if (!this.checked) {
                        allChecked = false;
                    }
                })
                if (allChecked) {
                    document.getElementById("allUgrads").checked = true;
                    $("#ugrad_majors .major_checkbox").removeAttr("checked");
                } else {
                    document.getElementById("byMajor").checked = true;
                    enableAll(document.getElementById("ugrad_majors"));
                }
            }
            if(gradMajorArray != null && gradMajorArray.length > 0) {
                $("#grad").click();
                enableAll(document.getElementById("choose_grad"));

                for (var i = 0, j = gradMajorArray.length; i < j; i++) {
                    var major = gradMajorArray[i].replace(/ /g, "_");
                    $("#grad_majors #" + major + "_P").attr("checked", true);
                }

                var allChecked = true;
                $("#grad_majors .major_checkbox").each(function(){
                    if (!this.checked) {
                        allChecked = false;
                    }
                })
                if (allChecked) {
                    document.getElementById("allGrads").checked = true;
                    $("#grad_majors .major_checkbox").removeAttr("checked");
                } else if ($("#grad:checked").val() != null) {
                    document.getElementById("byProgram").checked = true;
                    enableAll(document.getElementById("grad_majors"));
                }
            }
        }

        // Check all messages
        $("#inbox_inbox_checkAll").change(function(){
            tickMessages();
        });

        // Sorters for the inbox.
        /* Commented out for the myBerkeley POC since sorting is broken - eli
        $(".inbox_inbox_table_header_sort").bind("mouseenter", function() {
            if (sortOrder === 'descending') {
                $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortUp).html()));
            }
            else {
                $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortDown).html()));
            }
        });

        $(".inbox_inbox_table_header_sort").bind("mouseout", function() {
            $(inboxTable + " " + inboxArrow).remove();
        });

        $(".inbox_inbox_table_header_sort").bind("click", function() {
            sortBy = $(this).attr("id").replace(/inbox_table_header_/gi, "");
            sortOrder = (sortOrder === "descending") ? "ascending" : "descending";

            getAllMessages(); // no such function defined, but the function should sort the data and re-populate the inbox
        });
        */

        var deleteLists = function(listsArray) {
            var listId = listsArray;

            for (var i = 0, j = listId.length; i < j; i++) {
                var index = getIndexFromId(listId[i]);
                if(index >= 0) {
                    $("#inbox_table_list_" + listId[i]).empty();
                    $("#inbox_table_list_" + listId[i]).remove();

                    if (allLists.length == 1) {
                        sakai.api.Server.removeJSON(userUrl, loadData);
                        return;
                    } else {
                        allLists.splice(index, 1);
                    }
                } else {
                    alert("Error: list not found");
                }
            }

            submitData.lists = allLists;
            sakai.api.Server.saveJSON(userUrl, submitData, loadData);
        };

        /**
         * Returns a value indicating whether the specified list object already exists.
         *
         * @param {Object} listToCheck The list to seek
         * @return {Boolean} true if the value parameter occurs within the existing lists; otherwise, false
         */
        var listAlreadyExists = function(listToCheck) {

            for (var i = 0, j = allLists.length; i < j; i++) {
                var exsistingList = allLists[i];
                var exsistingListObj = {
                    "context" : exsistingList.query.context[0],
                    "listName": exsistingList["sakai:name"],
                    "desc": exsistingList["sakai:description"],
                    "standing": exsistingList.query.standing
                }

                if(listEquals(exsistingListObj, listToCheck)) return true;
            }

            return false;
        };

        /**
         * Compares two specified list objects.
         *
         * @param {Object} list1 The first list
         * @param {Object} list2 The The second list
         * @return {Boolean} true if the lists are equal; otherwise false
         */
        var listEquals = function(list1, list2) {

            if(list1.context !== list2.context) return false;
            if(list1.listName !== list2.listName) return false;
            if(list1.desc !== list2.desc) return false;

            var list1Standing = list1.standing;
            var list2Standing = list2.standing;

            var list1StandingLength = list1Standing.length;
            var list2StandingLength = list2Standing.length;

            if(list1StandingLength !== list2StandingLength) return false;

            // Comparing lists of majors

            var list1UndergradMajors = [];
            var list2UndergradMajors = [];
            var list1GradMajors = [];
            var list2GradMajors = [];

            // Assigning udergraduate and graduate majors to appropriate variables
            // Assuming that list1StandingLength == list2StandingLength
            for (var i = 0; i < list1StandingLength; i++) {

                if(list1Standing[i].hasOwnProperty("undergrad")) {
                    list1UndergradMajors = list1Standing[i].undergrad.major;
                } else if(list1Standing[i].hasOwnProperty("grad")) {
                    list1GradMajors = list1Standing[i].grad.major;
                }

                if(list2Standing[i].hasOwnProperty("undergrad")) {
                    list2UndergradMajors = list2Standing[i].undergrad.major;
                } else if(list2Standing[i].hasOwnProperty("grad")) {
                    list2GradMajors = list2Standing[i].grad.major;
                }
            }

            if(list1UndergradMajors.length !== list2UndergradMajors.length) return false;
            if(list1GradMajors.length !== list2GradMajors.length) return false;

            // Comparing undergraduate majors lists (assuming that both arrays have the same length)
            for(var i = 0, j = list1UndergradMajors.length; i < j; i++) {
                if(list2UndergradMajors.indexOf(list1UndergradMajors[i]) === -1) return false;
            }

            // Comparing graduate majors lists (assuming that both arrays have the same length)
            for(var i = 0, j = list1GradMajors.length; i < j; i++) {
                if(list2GradMajors.indexOf(list1GradMajors[i]) === -1) return false;
            }

            return true;
        }

        var finishSaveAndLoad = function() {
            clearInputFields();
            $.bbq.pushState({"tab": "existing"},2);
            loadData();
        };

        var generateId = function() {
            var id = "dl-" + sakai.data.me.user.userid + "-" + new Date().getTime();
            return id;
        }

        var saveList = function(data, index) {
            if (listAlreadyExists(data)) {
                showGeneralMessage($("#inbox_generalmessages_already_exists").text());
                return;
            }

            if(index != null && index >= 0) { // we are editing an existing list
                allLists[index]["sakai:name"] = data.listName;
                allLists[index]["sakai:description"] = data.desc;
                allLists[index]["sakai:dateModified"] = new Date();
                allLists[index]["sakai:dateModified@TypeHint"] = "date";
                allLists[index]["sakai:modifiedBy"] = sakai.data.me.user.userid;
                allLists[index].query.context = [data.context];
                allLists[index].query.standing = data.standing;
            } else { // we are creating a new list
                var id = generateId();

                var list = {
                    "sakai:id": id,
                    "sakai:name": data.listName,
                    "sakai:description": data.desc,
                    "sakai:dateModified": new Date(),
                    "sakai:dateModified@TypeHint": "date",
                    "sakai:modifiedBy": sakai.data.me.user.userid,
                    "query": {
                        "context": [data.context],
                        "standing": data.standing
                    }
                }
                allLists.push(list);
            }
            submitData.lists = allLists;
            sakai.api.Server.saveJSON(userUrl, submitData, finishSaveAndLoad);
        };

        // List creation events
        $("#undergrad").live("click", function() {
            if(this.checked) {
                enableAll(document.getElementById("choose_ugrad"));
            } else {
                disableAll(document.getElementById("choose_ugrad"));
            }
        });

        $("#grad").live("click", function() {
            if (this.checked) {
                enableAll(document.getElementById("choose_grad"));
            } else {
                disableAll(document.getElementById("choose_grad"));
            }
        });

        $("#byMajor").live("click", function() {
            if(this.checked) {
                enableAll(document.getElementById("ugrad_majors"));
            } else {
                disableAll(document.getElementById("ugrad_majors"));
            }
        });

        $("#allUgrads").live("click", function() {
            if(this.checked) {
                disableAll(document.getElementById("ugrad_majors"));
             }
        });

        $("#byProgram").live("click", function() {
             if(this.checked) {
                enableAll(document.getElementById("grad_majors"));
            } else {
                disableAll(document.getElementById("grad_majors"));
            }
        });

        $("#allGrads").live("click", function() {
            if(this.checked) {
                disableAll(document.getElementById("grad_majors"));
             }
        });

        var enableAll = function(el) {
            try {
                el.disabled = false;
            } catch (E) {}

            if (el && el.childNodes && el.childNodes.length > 0) {
                for (var x = 0; x < el.childNodes.length; x++) {
                    el.childNodes[x].disabled = false;
                }
            }
        }

        var disableAll = function(el) {
            try {
                el.disabled = true;
                el.checked = false;
            } catch (E) {}

            if (el && el.childNodes && el.childNodes.length > 0) {
                for (var x = 0; x < el.childNodes.length; x++) {
                    disableAll(el.childNodes[x]);
                }
            }
        }

        // Button click events
        $("#inbox_inbox_delete_button").live("click", function(){
            var listId = [];
            $(".inbox_inbox_check_list:checked").each(function(){
                var id = $(this).val();
                listId.push(id);
            });

            $("#inbox_inbox_checkAll").attr("checked", "");
            tickMessages();

            if (listId.length < 1) {
                showGeneralMessage($("#inbox_generalmessages_none_selected").text());
            } else {
                deleteLists(listId);
            }
        });

        $("#inbox_inbox_duplicate_button").live("click", function(){
            var listId = [];
            $(".inbox_inbox_check_list:checked").each(function(){
                var id = $(this).val();
                listId.push(id);
            });

            $("#inbox_inbox_checkAll").attr("checked", "");
            tickMessages();

            if (listId.length < 1) {
                showGeneralMessage($("#inbox_generalmessages_none_selected").text());
            } else if (listId.length > 1) {
                showGeneralMessage($("#inbox_generalmessages_duplicate_multiple").text());
            } else {
                displayList(listId[0]);
            }
        });

        $(".editLink").live("click", function(evt){
            editExisting = true;
            var id = evt.target.id;
            currList = id;
            displayList(id);
        });

        $("#inbox_inbox_back_button").live("click", function(){
            window.location = "/dev/inboxnotifier.html";
        });

        $("#inbox_inbox_cancel_button").live("click", function(){
            editExisting = false;
            clearInputFields();
            $.bbq.pushState({"tab": "existing"},2);
        });

        $("#inbox_inbox_save_button").live("click", function(){
            $("#invalid_name").hide();
            $("#invalid_major").hide();
            // NOT SUPPORTED FOR POC
            //$("#invalid_size").hide();

            var data = getDataFromInput();
            if(data < 0) {
                return;
            }

            // NOT SUPPORTED FOR POC
            // var size = $("#list_size").val();

            // In IE browser jQuery.trim() function doesn't work this way $('#selector').text().trim()
            // It should be called like this $.trim($('#selector').text())
            // See http://bit.ly/d8mDx2
            if (editExisting) {
                saveList(data, getIndexFromId(currList));
                editExisting = false;
            } else {
                saveList(data, null);
            }
        });

        // NOT SUPPORTED FOR POC
        // On selecting checkbox events
        /*
        $(".major_checkbox").click(function() {
            sakai_global.listpage.updateListSize();
        });
        */

        // Tab click events
        $("#existing_link").click(function() {
            $.bbq.pushState({"tab": "existing"},2);

            // Set headers and tab styling
            $("#inbox_new").hide();
            $("#inbox_existing").show();
            $("#new_list_tab").removeClass("fl-tabs-active");
            $("#existing_lists_tab").addClass("fl-tabs-active");

            // Show/hide appropriate buttons
            $("#inbox_inbox_cancel_button").hide();
            $("#inbox_inbox_save_button").hide();
            $("#inbox_inbox_delete_button").show();
            $("#inbox_inbox_duplicate_button").show();
            $("#inbox_inbox_back_button").show();
        });

        $("#create_new_link").click(function() {
            $.bbq.pushState({"tab": "new"},2);

            // NOT SUPPORTED FOR POC
            // sakai_global.listpage.updateListSize();

            // Set headers and tab styling
            $("#inbox_existing").hide();
            $("#inbox_new").show();
            $("#existing_lists_tab").removeClass("fl-tabs-active");
            $("#new_list_tab").addClass("fl-tabs-active");

            // Show/hide appropriate buttons
            $("#inbox_inbox_delete_button").hide();
            $("#inbox_inbox_duplicate_button").hide();
            $("#inbox_inbox_back_button").hide();
            $("#inbox_inbox_cancel_button").show();
            $("#inbox_inbox_save_button").show();
        });

        var setTabState = function(){
            var tab = $.bbq.getState("tab");
            if (tab) {
                switch (tab) {
                    case "existing":
                        $("#existing_link").click();
                        break;
                    case "new":
                        $("#create_new_link").click();
                        break;
                }
            }
        };

        $(window).bind('hashchange', function(e) {
            setTabState();
        });

        var createDefaultList = function() {
            allLists = [];
            var emptyData = {
                "links": []
            }

            $(inboxTable).children("tbody").append(sakai.api.Util.TemplateRenderer("#inbox_inbox_lists_template", emptyData));
        };

        /**
         * Takes the data returned by the server and formats it to get it ready for posting to the server
         * @param {Object} data
         */
        var formatData = function(data) {
            var result =  {
                "_MODIFIERS": {},
                "lists": data.lists
            };
            return result;
        };

        var loadData = function() {
            sakai.api.Server.loadJSON(userUrl, function(success, data){
                $("#tabs").tabs();
                setTabState();

                if (success) {
                    submitData = formatData(data);
                    renderLists(submitData);
                } else {
                    createDefaultList();
                }
            });
        };

        var doInit = function() {
            // if the user is not a member of the advisors group then bail
            if (!myb.api.security.isUserAnAdvisor()) {
                sakai.api.Security.send403();
                return;
            }

            userUrl = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists";
            loadData();
        };

        doInit();
    };

    sakai.api.Widgets.Container.registerForLoad("listpage");
});
