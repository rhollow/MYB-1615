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

var sakai = sakai || {};
sakai.listpage = function(){
    /**
     *
     * Configuration
     *
     */
    var allLists = []; // Array that will hold all the dynamic lists.
    var userUrl;
    var generalMessageFadeOutTime = 3000; // The amount of time it takes till the general message box fades out
    var sortBy = "name";
    var sortOrder = "descending";


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
     * This function will redirect the user to the login page.
     */
    var redirectToLoginPage = function(){
        document.location = sakai.config.URL.GATEWAY_URL;
    };
    
    /**
     * This will show the preloader.
     */
    var showLoader = function(){
        $(inboxTable).append($.TemplateRenderer(inboxTablePreloader.substring(1), {}));
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
    
    /**
     * Updates the input field that displays the current size of the list being created
     */
    sakai.listpage.updateListSize = function(){
        var index = document.createListForm.context.selectedIndex;
        var selectedContext = document.createListForm.context.options[index].text;
        index = document.createListForm.major.selectedIndex;
        var selectedMajor = document.createListForm.major.options[index].text;
        index = document.createListForm.standing.selectedIndex;
        var selectedStanding = document.createListForm.standing.options[index].text;

        // DEBUGGING:
        // var size = query(selectedContext, selectedMajor, selectedStanding);
        // document.createListForm.list_size.value = size;
    }
    
    /**
     * Clears input fields
     */
    var clearInputFields = function() {
        document.getElementById("createListForm").reset();
        sakai.listpage.updateListSize();
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
    
    var formatList = function(list){
        var dateString = list.dateModified;
        
        var d = new Date();
        d.setFullYear(parseInt(dateString.substring(0, 4), 10));
        d.setMonth(parseInt(dateString.substring(5, 7), 10) - 1);
        d.setDate(parseInt(dateString.substring(8, 10), 10));
        d.setHours(parseInt(dateString.substring(11, 13), 10));
        d.setMinutes(parseInt(dateString.substring(14, 16), 10));
        d.setSeconds(parseInt(dateString.substring(17, 19), 10));
        //format Jan 22, 2009 10:25 PM
        list.date = formatDate(d, "M j, Y g:i A");
        
        return list;
    }
    
    /**
     * Removes all the messages out of the DOM.
     * It will also remove the preloader in the table.
     */
    var removeAllListsOutDOM = function(){
        $(".inbox_list").remove();
    };
    
    var renderLists = function(response){
        for (var j = 0, l = response.lists.length; j < l; j++) {
            response.lists[j] = formatList(response.lists[j]);
        }
        
        allLists = response.lists;
        
        // remove previous lists
        removeAllListsOutDOM();
        
        // Add them to the DOM
        $(inboxTable).children("tbody").append($.TemplateRenderer("#inbox_inbox_lists_template", allLists));
        
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
        return {};
    };
    
    /**
     * Returns the index of the list wtih the specific id.
     * @param {Object} id
     */
    var getIndexOfList = function(id) {
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
        $.bbq.pushState({"tab": "existing"},2);
        
        // Fill in input fields with list data
        var list = getListWithId(id);
        $("#list_name").value = list["sakai:name"];
        $("#description").value = list["sakai:description"];
        $("#context").val(list.query.contextreplace(/ /gi, "_"));
        $("#major").val(list.query.majorreplace(/ /gi, "_"));
        $("#standing").val(list.query.standingreplace(/ /gi, "_"));
    }
    
    /**
     * Gets all the messages from the JCR.
     */
    getAllMessages = function(callback) { // DEBUGGING: will this function even work? since we're using ajax indirectly now

        switch(sortBy) {
            case "date_modified":
                sortBy = "sakai:dateModified";
                break;
            case "name":
                sortBy = "sakai:name";
                break;
            case "description":
                sortBy = "sakai:description";
                break;
            case "size":
                sortBy = "sakai:size";
                break;
        }

        url += "&sortOn=" + sortBy + "&sortOrder=" + sortOrder;
        $.ajax({
            url: url,
            cache: false,
            success: function(data) {
                if (data.results) {
                    // Render the messages
                    renderLists(data);
                }
                if (typeof callback !== "undefined") {
                    callback();
                }

            },
            error: function(xhr, textStatus, thrownError) {
                showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
                $(inboxResults).html(sakai.api.Security.saneHTML($(inboxGeneralMessagesErrorGeneral).text()));
            }
        });
    };

    // Check all messages
    $("#inbox_inbox_checkAll").change(function(){
        tickMessages();
    });
    
    // Sorters for the inbox.
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

        getAllMessages();
    });
    
    var deleteLists = function() {
        var listId = [];
        $("#inbox_inbox_check_list:checked").each(function(){
            var id = $(this).val();
            listId.push(id);
        });
        
        for (var i = 0, j = listId.length; i < j; i++) {
            var index = getIndexOfList(listId[i]);
            if(index > 0) {
                allLists.remove(index);
            } else {
                alert("List not found");
            }
        }
        
        sakai.api.Server.saveJSON(userUrl, allLists);
    };
    
    var saveList = function() {
        // DEBUGGING
        // ajax to save list with listName and desc and the three selected states
        // need some way of checking if saving new list or overwriting existing one
        
        // on success:
        showGeneralMessage($("#inbox_generalmessages_list_saved").text());
        clearInputFields();
    };
    
    // Button click events
    $("#inbox_inbox_delete_button").live("click", function(){
        var listId = [];
        $("#inbox_inbox_check_list:checked").each(function(){
            var id = $(this).val();
            listId.push(id);
        });
        
        if (listId.length < 1) {
            showGeneralMessage($("#inbox_generalmessages_none_selected").text());
        } else {
            $("#delete_check_dialog").jqmShow();
        }
    });
    
    $("#delete_check_dialog").css("position", "absolute");
    $("#delete_check_dialog").css("top", "250px");
    
    $("#dlc-delete").click(function(){
        $("#save_reminder_dialog").jqmHide();
        deleteLists();
    });
    
    $("#inbox_inbox_duplicate_button").live("click", function(){
        var listId = [];
        $("#inbox_inbox_check_list:checked").each(function(){
            var id = $(this).val();
            listId.push(id);
        });
        
        if(listId.length < 1) {
            showGeneralMessage($("#inbox_generalmessages_none_selected").text());
        } else {
            for(var i = 0, j = listId.length; i < j; i++) {
                // DEBUGGING
                // need ajax call to create new lists with same data as the selected lists   
            }
            // on success:
            // somehow reload the inbox pane to display new lists - need to force refresh page?
        }
    });
    
    $("#inbox_inbox_edit_button").live("click", function(){
        var listId = [];
        $("#inbox_inbox_check_list:checked").each(function(){
            var id = $(this).val();
            listId.push(id);
        });
        
        if(listId.length < 1) {
            showGeneralMessage($("#inbox_generalmessages_none_selected").text());
        } else if(listId.length > 1) {
            showGeneralMessage($("#inbox_generalmessages_edit_multiple").text());
        } else {
            displayList(listId[0]);   
        }
    });
    
    $("#inbox_inbox_back_button").live("click", function(){
        window.location = "http://localhost:8080/dev/inboxnotifier.html";
    });
    
    $("#inbox_inbox_reset_button").live("click", function(){
        clearInputFields();
    });
    
    $("#inbox_inbox_save_button").live("click", function(){
        $("#invalid_name").hide();
        $("#invalid_desc").hide();
        $("#invalid_size").hide();
        var listName = $("#list_name").val();
        var desc = $("#description").val();
        var size = $("#list_size").val();
        
        if(listName.trim() && desc.trim() && size != 0) {
            saveList();
        } else {
            if(!listName.trim()) {
                $("#invalid_name").show();
            }
            if(!desc.trim()) {
                $("#invalid_desc").show();
            }
            if(size == 0) {
                $("#invalid_size").show();
            }
        }
    });
    
    // Tab click events
    $("#existing_link").click(function() {
        $.bbq.pushState({"tab": "existing"},2);
        
        // Set headers and tab styling
        $("#inbox_new").hide();
        $("#inbox_existing").show();
        $("#new_list_tab").removeClass("selected_tab");
        $("#existing_lists_tab").addClass("selected_tab");
        
        // Show/hide appropriate buttons
        $("#inbox_inbox_reset_button").hide();
        $("#inbox_inbox_save_button").hide();
        $("#inbox_inbox_delete_button").show();
        $("#inbox_inbox_duplicate_button").show();
        $("#inbox_inbox_edit_button").show();
        $("#inbox_inbox_back_button").show();
    });
    
    $("#create_new_link").click(function() {
        $.bbq.pushState({"tab": "new"},2);
        sakai.listpage.updateListSize();
        
        // Set headers and tab styling
        $("#inbox_existing").hide();
        $("#inbox_new").show();
        $("#existing_lists_tab").removeClass("selected_tab");
        $("#new_list_tab").addClass("selected_tab");
        
        // Show/hide appropriate buttons
        $("#inbox_inbox_delete_button").hide();
        $("#inbox_inbox_duplicate_button").hide();
        $("#inbox_inbox_edit_button").hide();
        $("#inbox_inbox_back_button").hide();
        $("#inbox_inbox_reset_button").show();
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
        } else {
            $("#existing_link").click();
        }
    };
    
    $(window).bind('hashchange', function(e) {
        setTabState();
    });
    
    var doInit = function() {
        // Check if we are logged in or out.
        var person = sakai.data.me;
        var uuid = person.user.userid;
        if (!uuid || person.user.anon) {
            redirectToLoginPage();
        } else {
            userUrl = "/~" + uuid + "/private/dynamic_lists";
            sakai.api.Server.loadJSON(url, function(success, data){
                if (success) {
                    $("#tabs").tabs();
                    setTabState();
                    renderLists(data);
                } else {
                    alert("error");
                }
            });
        }
    };

    doInit();
}
sakai.api.Widgets.Container.registerForLoad("sakai.listpage");
