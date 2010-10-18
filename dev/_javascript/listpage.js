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
    var submitData = {}; // Data to be passed to saveJSON
    var allLists = []; // Array of all the lists
    var userUrl;
    var generalMessageFadeOutTime = 3000; // The amount of time it takes till the general message box fades out
    var sortBy = "name";
    var sortOrder = "descending";
    var editExisting = false;
    var currList;
    var hashMap = new Object(); // hash map to keep track of unique dynamic lists


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
    
    var getDataFromInput = function() {
        var result = {};
        
        result.listName = $("#list_name").val().trim();
        result.desc = $("#description").val().trim();
        
        // NOT SUPPORTED FOR POC
        // result.size = $("#list_size").val();
        
        var index = document.createListForm.context.selectedIndex;
        var selectedContext = document.createListForm.context.options[index].value;
        result.context = selectedContext.trim();
        
        var selectedMajors = [];
        $(".major_checkbox:checked").each(function() {
            var major = $(this).val().trim();
            selectedMajors.push(major);
        });
        result.major = selectedMajors;
        
        var standingArray = [];
        index = document.createListForm.standing.selectedIndex;
        var selectedStanding = document.createListForm.standing.options[index].value;
        if(selectedStanding === "all" || selectedStanding === "All") {
            standingArray.push("undergrad");
            standingArray.push("grad");
        } else {
            standingArray.push(selectedStanding)
        }
        result.standing = standingArray;
        
        return result;
    }
    
    // NOT SUPPORTED FOR POC
    /**
     * Updates the input field that displays the current size of the list being created
     */
    sakai.listpage.updateListSize = function(){
        var data = getDataFromInput();
        
        // STILL NEEDS TO BE IMPLEMENTED
        // var size = query(selectedContext, selectedMajor, selectedStanding);
        // document.createListForm.list_size.value = size;
    }
    
    /**
     * Clears input fields
     */
    var clearInputFields = function() {
        document.getElementById("createListForm").reset();
        
        // NOT SUPPORTED FOR POC
        // sakai.listpage.updateListSize();
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
    
    sakai.listpage.getDate = function(dateString){
        var d = sakai.api.Util.parseSakaiDate(dateString);
        d = formatDate(d, "M j, Y g:i A");
        return d;
    }
    
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
        $(inboxTable).children("tbody").append($.TemplateRenderer("#inbox_inbox_lists_template", data));
        
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
        
        // Fill in input fields with list data
        var list = getListWithId(id);
        document.createListForm.list_name.value = list["sakai:name"];
        document.createListForm.description.value = list["sakai:description"];
        
        // NOT SUPPORTED FOR POC
        // document.createListForm.list_size.value = list["sakai:size"];
        
        // $("#context").val(list.query.context);
        // HARD-CODING FOR POC
        $("#context").val("ced");
        
        var majorArray = list.query.major;
        for(var i = 0, j = majorArray.length; i < j; i++) {
            var major = majorArray[i].replace(/ /g, "_");
            $("#" + major).attr("checked", true);
        }
        
        var standingArray = list.query.standing;
        if(standingArray.length != 1) {
            $("#standing").val("all");
        } else {
            $("#standing").val(standingArray[0]);
        }
    }

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
                alert("List not found");
            }
        }
        
        submitData.lists = allLists;
        sakai.api.Server.saveJSON(userUrl, submitData, loadData);
    };
    
    var listAlreadyExists = function(id){
        return hashMap[id] != null;
    };
    
    var getHashId = function(data) {
        var id = "dl-" + data.context;
        var majorArray = data.major;
        var standingArray = data.standing;
        
        for(var i = 0, j = majorArray.length; i < j; i++) {
            id += majorArray[i];
        }
         
        for(var i = 0, j = standingArray.length; i < j; i++) {
            id += standingArray[i];
        } 
        
        id = id.replace(/ /gi, "");
        
        return id;
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
        var hashId = getHashId(data);        
        if (listAlreadyExists(hashId)) {
            showGeneralMessage($("#inbox_generalmessages_already_exists").text());
            return;
        }
        
        if(index != null) { // we are editing an existing list
            allLists[index]["sakai:name"] = data.listName;
            allLists[index]["sakai:description"] = data.desc;
            allLists[index]["sakai:dateModified"] = new Date();
            allLists[index]["sakai:dateModified@TypeHint"] = "date";
            allLists[index]["sakai:modifiedBy"] = sakai.data.me.user.userid;
            allLists[index].query.context = [data.context];
            allLists[index].query.standing = data.standing;
            allLists[index].query.major = data.major;
        } else { // we are creating a new list
            hashMap[hashId] = hashId;
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
                    "standing": data.standing,
                    "major": data.major
                }
            }
            allLists.push(list);
        }
        
        submitData.lists = allLists;
        sakai.api.Server.saveJSON(userUrl, submitData, finishSaveAndLoad);
    };
    
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
        $("#invalid_desc").hide();
        $("#invalid_major").hide();
        
        // NOT SUPPORTED FOR POC
        //$("#invalid_size").hide();
        var data = getDataFromInput();
        
        // NOT SUPPORTED FOR POC
        // var size = $("#list_size").val();
        
        if(data.listName.trim() && data.desc.trim() && (data.major.length != 0)) {
            if(editExisting) {
                saveList(data, getIndexFromId(currList));
            } else {
                saveList(data, null);   
            }
        } else {
            if(!data.listName.trim()) {
                $("#invalid_name").show();
            }
            if(!data.desc.trim()) {
                $("#invalid_desc").show();
            }
            if(data.major.length == 0) {
                $("#invalid_major").show();
            }
            
            // NOT SUPPORTED FOR POC
            /*
            if(size == 0) {
                $("#invalid_size").show();
            }
            */
        }
    });
    
    // NOT SUPPORTED FOR POC
    // On selecting checkbox events
    /*
    $(".major_checkbox").click(function() {
        sakai.listpage.updateListSize();
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
        // sakai.listpage.updateListSize();
        
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
        } else {
            $("#existing_link").click();
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
        
        $(inboxTable).children("tbody").append($.TemplateRenderer("#inbox_inbox_lists_template", emptyData));
    }
    
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
    }
    
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
    }
    
    var doInit = function() {
        // Check if we are logged in or out.
        var person = sakai.data.me;
        var uuid = person.user.userid;
        if (!uuid || person.user.anon) {
            redirectToLoginPage();
        } else {
            userUrl = "/~" + uuid + "/private/dynamic_lists";
            loadData();
        }
    };

    doInit();
}
sakai.api.Widgets.Container.registerForLoad("sakai.listpage");
