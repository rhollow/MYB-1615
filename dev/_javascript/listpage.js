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
    var removeAllMessagesOutDOM = function(){
        $(".inbox_list").remove();
    };
    
    var renderLists = function(response){
        for (var j = 0, l = response.lists.length; j < l; j++) {
            // style notice according to whether it's a message or reminder
            response.lists[j] = formatList(response.lists[j]);
        }
        
        allLists = response.lists;
        
        // Show messages
        var tplData = {
            "messages": response.lists
        };
        
        // remove previous messages
        removeAllMessagesOutDOM();
        
        // Add them to the DOM
        $(inboxTable).children("tbody").append($.TemplateRenderer("#inbox_inbox_lists_template", tplData));
        
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
     * Displays only the list with that id.
     * @param {String} id    The id of a list
     */
    var displayList = function(id){
        
        // Hide/show relevant buttons
        $(".different").hide();
        $("#inbox_inbox_delete_button").hide();
        $("#inbox_inbox_duplicate_button").hide();
        $("#inbox_inbox_edit_button").hide();
        $("#inbox_inbox_back_button").hide();
        $("#inbox_inbox_cancel_button").show();
        $("#inbox_inbox_save_button").show();
        
        var list = getListWithId(id);

        if (typeof list !== "undefined") {
            // Fill in this message values.
            $(inboxSpecificMessageSubject).text(sakai.api.Security.saneHTML(message["sakai:subject"]));
            var messageBody = "" + message["sakai:body"]; // coerce to string in case the body is all numbers
            $(inboxSpecificMessageBody).html(sakai.api.Security.saneHTML(messageBody.replace(/\n/gi, "<br />")));
            $(inboxSpecificMessageDate).text(sakai.api.Security.saneHTML(message.date));
        }
    }
    
    /**
     * Gets all the messages from the JCR.
     */
    getAllMessages = function(callback) {

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
    
    $("#existing_link").click(function() {
        // Set headers and tab styling
        $("#inbox_new").hide();
        $("#inbox_existing").show();
        $("#new_list_tab").removeClass("selected_tab");
        $("#existing_lists_tab").addClass("selected_tab");
        
        // Show/hide appropriate buttons
        $("#inbox_inbox_cancel_button").hide();
        $("#inbox_inbox_save_button").hide();
        $("#inbox_inbox_delete_button").show();
        $("#inbox_inbox_duplicate_button").show();
        $("#inbox_inbox_edit_button").show();
        $("#inbox_inbox_back_button").show();
    });
    
    $("#create_new_link").click(function() {
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
        $("#inbox_inbox_cancel_button").show();
        $("#inbox_inbox_save_button").show();
    });
    
    var doInit = function() {
        // Check if we are logged in or out.
        var person = sakai.data.me;
        var uuid = person.user.userid;
        if (!uuid || person.user.anon) {
            redirectToLoginPage();
        } else {
            $("#tabs").tabs();
            $("#existing_link").click();
        }
    }

    doInit();
}
sakai.api.Widgets.Container.registerForLoad("sakai.listpage");


// document.createListForm.list_size.value = 5; // TO SET SIZE FIELD


