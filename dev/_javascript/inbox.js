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
/*global $, Config, opensocial */

var sakai = sakai || {};
sakai.inbox = function(){

    /**
     *
     * Configuration
     *
     */
    var messagesPerPage = 12; // The number of messages per page.
    var allMessages = []; // Array that will hold all the messages.
    var me = sakai.data.me;
    var generalMessageFadeOutTime = 3000; // The amount of time it takes till the general message box fades out
    var selectedMessage = {}; // The current message
    var selectedType = 'inbox';
    var selectedCategory = "";
    var sortOrder = "descending";
    var sortBy = "date";
    var currentPage = 0;
    var messagesForTypeCat; // The number of messages for this type/cat.
    var box = "";
    var cats = "";
    var inboxComposeNewPanelOpen = false;
    
    
    /**
     *
     * CSS IDs
     *
     */
    var inbox = "inbox";
    var inboxID = "#inbox";
    var inboxClass = ".inbox";
    
    // global vars
    var inboxGeneralMessage = inboxID + "_general_message";
    var inboxMessageError = inbox + "_error_message";
    var inboxMessageNormal = inbox + "_normal_message";
    var inboxPager = inboxID + "_pager";
    var inboxResults = inboxID + "_results";
    var inboxArrow = inboxClass + "_arrow";
    var inboxFolders = inboxID + "_folders";
    
    // Filters on the left side
    var inboxFilter = inboxID + "_filter";
    var inboxFilterClass = inboxClass + "_filter";
    var inboxFilterInbox = inboxFilter + "_inbox";
    var inboxFilterMessages = inboxFilter + "_messages";
    var inboxFilterAnnouncements = inboxFilter + "_announcements";
    var inboxFilterChats = inboxFilter + "_chats";
    var inboxFilterInvitations = inboxFilter + "_invitations";
    var inboxFilterSent = inboxFilter + "_sent";
    var inboxFilterReminders = inboxFilter + "_reminders";
    var inboxFilterArchive = inboxFilter + "_archive";
    var inboxFilterTrash = inboxFilter + "_trash";
    var inboxFilterNrMessages = inboxFilterClass + "_nrMessages";
    var inboxFilterNrReminders = inboxFilterClass + "_nrReminders";
    var inboxBold = inbox + "_bold";
    var currentFilter;
    
    // Different panes (inbox, send message, view message, ..)
    var inboxPane = inboxID + "_pane";
    var inboxPaneClass = inboxClass + "_pane";
    var inboxPaneInbox = inboxPane + "_inbox";
    var inboxPaneCompose = inboxPane + "_compose";
    var inboxPaneMessage = inboxPane + "_message";
    var inboxPaneCreateNotification = inboxPane + "_createNotification";
    
    // Main inbox
    var inboxTable = inboxID + "_table";
    var inboxTablePreloader = inboxTable + "_preloader";
    var inboxTableHeader = inboxTable + "_header";
    var inboxTableHeaderFrom = inboxTableHeader + "_from";
    var inboxTableHeaderFromContent = inboxTableHeaderFrom + " span";
    var inboxTableHeaderDate = inboxTableHeader + "_date";
    var inboxTableHeaderDateContent = inboxTableHeaderDate + " span";
    var inboxTableMessage = inboxClass + "_message"; //    A row in the table
    var inboxTableMessageID = inboxTable + "_message_";
    var inboxTableMessagesTemplate = inbox + "_" + inbox + "_messages_template";
    var inboxTableSubject = inboxTable + "_subject_";
    var inboxTablesubjectReadClass = 'inbox-subject-read';
    var inboxTablesubjectUnreadClass = 'inbox-subject-unread';
    
    var inboxInbox = inboxID + "_inbox";
    var inboxInboxClass = inboxClass + "_inbox";
    
    var inboxInboxSortUp = inboxInbox + "_sort_up";
    var inboxInboxSortDown = inboxInbox + "_sort_down";
    
    var inboxInboxCheckAll = inboxInbox + "_checkAll";
    var inboxInboxDelete = inboxInbox + "_delete";
    var inboxInboxEmptyTrash = inboxInbox + "_empty_trash";
    var inboxInboxBackToList = inboxInbox + "_back_to_list";
    
    var inboxInboxMessage = inboxInboxClass + "_message";
    var inboxInboxHeader = inboxInboxClass + "_header";
    var inboxInboxCheckMessage = inboxInboxClass + '_check_message';
    
    var inboxTableHeaderSort = inboxInboxClass + "_table_header_sort";
    
    // Specific message
    var inboxSpecificMessage = inboxID + "_message";
    var inboxSpecificMessageDelete = inboxSpecificMessage + "_delete";
    var inboxSpecificMessageBackToInbox = inboxSpecificMessage + "_back_to_inbox";
    var inboxSpecificMessagePreviousMessages = inboxSpecificMessage + "_previous_messages";
    var inboxSpecificMessageOption = inboxSpecificMessage + "_option";
    var inboxSpecificMessageOptionReply = inboxSpecificMessageOption + "_reply";
    var inboxSpecificMessageOptionDelete = inboxSpecificMessageOption + "_delete";
    var inboxSpecificMessageBody = inboxSpecificMessage + "_body";
    var inboxSpecificMessageDate = inboxSpecificMessage + "_date";
    var inboxSpecificMessageFrom = inboxSpecificMessage + "_from";
    var inboxSpecificMessageSubject = inboxSpecificMessage + "_subject";
    var inboxSpecificMessagePicture = inboxSpecificMessage + "_picture";
    
    // Reply on a message
    var inboxSpecificMessageReplies = inboxSpecificMessage + "_replies";
    var inboxSpecificMessageRepliesTemplate = inbox + "_message_replies_template";
    var inboxSpecificMessageRepliesTemplateChats = "inbox_compose_replies_template_chats";
    
    var inboxSpecificMessageCompose = inboxSpecificMessage + "_compose";
    var inboxSpecificMessageComposeSubject = inboxSpecificMessageCompose + "_subject";
    var inboxSpecificMessageComposeBody = inboxSpecificMessageCompose + "_body";
    var inboxSpecificMessageComposeSend = inboxSpecificMessageCompose + "_send";
    var inboxSpecificMessageComposeCancel = inboxSpecificMessageCompose + "_cancel";
    
    
    // New message
    var inboxCompose = inboxID + "_compose";
    var inboxComposeCancel = "#send_message_cancel";
    var inboxComposeMessage = inboxCompose + "_message";
    var inboxComposeNew = inboxCompose + "_new";
    var inboxComposeNewContainer = inboxComposeNew + "_container";
    
    var inboxComposeNewPanel = inboxComposeNew + "_panel";
    
    // Viewing reminders
    var inboxReminder = inboxID + "_reminder";
    
    // Errors and messages
    var inboxGeneralMessages = inboxID + "_generalmessages";
    var inboxGeneralMessagesError = inboxGeneralMessages + "_error";
    var inboxGeneralMessagesErrorGeneral = inboxGeneralMessagesError + "_general";
    var inboxGeneralMessagesErrorReadFail = inboxGeneralMessagesError + "_read_fail";
    var inboxGeneralMessagesNrNewMessages = inboxGeneralMessages + "_nr_new_messages";
    var inboxGeneralMessagesNoneSelected = inboxGeneralMessages + "_none_selected";
    var inboxGeneralMessagesDeleted = inboxGeneralMessages + "_deleted";
    var inboxGeneralMessagesDeleted_1 = inboxGeneralMessagesDeleted + "_1";
    var inboxGeneralMessagesDeleted_x = inboxGeneralMessagesDeleted + "_x";
    var inboxGeneralMessagesSent = inboxGeneralMessages + "_sent";
    var inboxGeneralMessagesCompleted = inboxGeneralMessages + "_completed";
    var inboxGeneralMessagesDeletedFailed = inboxGeneralMessagesDeleted + "_failed";
    var inboxGeneralMessagesSendFailed = inboxGeneralMessages + "_send_fail";
    
    // other IDs
    var chatUnreadMessages = "#chat_unreadMessages";
    var selectedFilterDiv = "";
    
    
    // Keep JSLint.com happy...
    var pageMessages = function(){
    };
    var getCount = function(){
    };
    var getAllMessages = function(){
    };
    
    /**
     *
     * Aid Functions
     *
     */
    var unreadMessages = 0;
    var unreadInvitations = 0;
    var unreadAnnouncements = 0;
    var unreadReminders = 0;
    var unreadChats = 0;
    
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
     * Scroll to a specific element in a page
     * @param {Object} element The element you want to scroll to
     */
    var scrollTo = function(element){
        $("html, body").animate({
            scrollTop: element.offset().top
        }, 1);
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
     * This will hide all the panes (the inbox, new reply, view message, etc..)
     */
    var hideAllPanes = function(){
        $(inboxPaneClass).hide();
    };
    
    /**
     * Will show the required pane and hide all the others.
     * @param {String} the Id of the pane you want to show
     */
    var showPane = function(pane){
        //    We do a check to see if the pane isn't already visible
        //    Otherwise we get an annoying flicker.
        if (!$(pane).is(":visible")) {
            hideAllPanes();
            $(pane).show();
        }
    };
    
    /**
     * Check or uncheck all messages depending on the top checkbox.
     */
    var tickMessages = function(){
        $(inboxInboxCheckMessage).attr("checked", ($(inboxInboxCheckAll).is(":checked") ? "checked" : ''));
    };
    
    /**
     * This will display the first page of the specified messages
     * @param {String} type The type of the messages (inbox, sent or trash or * for all of them)
     * @param {String} category The category of the messages (chat, invitation, ... or * for all of them)
     * @param {String} read Whether we should fetch messages that are read, unread or all of them. Option: true, false, all
     * @param {String} id The id of the filter that got clicked in the side panel.
     */
    var filterMessages = function(type, category, read, id){
        // Reset headers
        $(inboxTableHeaderFromContent).text("From");
        $(inboxTableHeaderDateContent).text("Date");
        
        // Show/hide appropriate buttons
        $(inboxInboxDelete).show();
        $(inboxSpecificMessageDelete).hide();
        $(inboxInboxEmptyTrash).hide();
        $(inboxInboxBackToList).hide();
        
        // The small header above the webpage
        $(inboxInboxHeader).hide();
        $(inboxID + "_" + type + category).show();
        
        // Remember the type and category we want to see.
        selectedType = type;
        selectedCategory = category;
        
        // Display first page.
        //getCount(read);
        //getAllMessages();
        showPage(1);
        
        // Show the inbox pane
        showPane(inboxPaneInbox);
        
        // Set the title bold
        $(inboxFilterClass).removeClass(inboxBold);
        $(id).addClass(inboxBold);
    };
    
    /**
     * Removes all the messages out of the DOM.
     * It will also remove the preloader in the table.
     */
    var removeAllMessagesOutDOM = function(){
        $(inboxTableMessage).remove();
    };
    
    /**
     * This method will clear the input fields for the reply part of a specific message.
     * It will also hide the form again.
     */
    var clearInputFields = function(){
        // Clear all the input fields.
        $(inboxSpecificMessageComposeSubject + ", " + inboxSpecificMessageComposeBody).val('');
        
        // Hide the reply form.
        $(inboxSpecificMessageCompose).hide();
    };
    
    
    /**
     *
     * Render messages
     *
     */
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
        d: function(){
            return (replaceChars.date.getDate() < 10 ? '0' : '') + replaceChars.date.getDate();
        },
        D: function(){
            return replaceChars.shortDays[replaceChars.date.getDay()];
        },
        j: function(){
            return replaceChars.date.getDate();
        },
        l: function(){
            return replaceChars.longDays[replaceChars.date.getDay()];
        },
        N: function(){
            return replaceChars.date.getDay() + 1;
        },
        S: function(){
            return (replaceChars.date.getDate() % 10 === 1 && replaceChars.date.getDate() !== 11 ? 'st' : (replaceChars.date.getDate() % 10 === 2 && replaceChars.date.getDate() !== 12 ? 'nd' : (replaceChars.date.getDate() % 10 === 3 && replaceChars.date.getDate() !== 13 ? 'rd' : 'th')));
        },
        w: function(){
            return replaceChars.date.getDay();
        },
        z: function(){
            return "Not Yet Supported";
        },
        // Week
        W: function(){
            return "Not Yet Supported";
        },
        // Month
        F: function(){
            return replaceChars.longMonths[this.getMonth()];
        },
        m: function(){
            return (replaceChars.date.getMonth() < 11 ? '0' : '') + (replaceChars.date.getMonth() + 1);
        },
        M: function(){
            return replaceChars.shortMonths[replaceChars.date.getMonth()];
        },
        n: function(){
            return replaceChars.date.getMonth() + 1;
        },
        t: function(){
            return "Not Yet Supported";
        },
        // Year
        L: function(){
            return "Not Yet Supported";
        },
        o: function(){
            return "Not Supported";
        },
        Y: function(){
            return replaceChars.date.getFullYear();
        },
        y: function(){
            return ('' + replaceChars.date.getFullYear()).substr(2);
        },
        // Time
        a: function(){
            return replaceChars.date.getHours() < 12 ? 'am' : 'pm';
        },
        A: function(){
            return replaceChars.date.getHours() < 12 ? 'AM' : 'PM';
        },
        B: function(){
            return "Not Yet Supported";
        },
        g: function(){
            return replaceChars.date.getHours() % 12 || 12;
        },
        G: function(){
            return replaceChars.date.getHours();
        },
        h: function(){
            return ((replaceChars.date.getHours() % 12 || 12) < 10 ? '0' : '') + (replaceChars.date.getHours() % 12 || 12);
        },
        H: function(){
            return (replaceChars.date.getHours() < 10 ? '0' : '') + replaceChars.date.getHours();
        },
        i: function(){
            return (replaceChars.date.getMinutes() < 10 ? '0' : '') + replaceChars.date.getMinutes();
        },
        s: function(){
            return (replaceChars.date.getSeconds() < 10 ? '0' : '') + replaceChars.date.getSeconds();
        },
        // Timezone
        e: function(){
            return "Not Yet Supported";
        },
        I: function(){
            return "Not Supported";
        },
        O: function(){
            return (replaceChars.date.getTimezoneOffset() < 0 ? '-' : '+') + (replaceChars.date.getTimezoneOffset() / 60 < 10 ? '0' : '') + (replaceChars.date.getTimezoneOffset() / 60) + '00';
        },
        T: function(){
            return "Not Yet Supported";
        },
        Z: function(){
            return replaceChars.date.getTimezoneOffset() * 60;
        },
        // Full Date/Time
        c: function(){
            return "Not Yet Supported";
        },
        r: function(){
            return replaceChars.date.toString();
        },
        U: function(){
            return replaceChars.date.getTime() / 1000;
        }
    };
    
    
    /**
     * Format a date to a string.
     * See replaceChars for the specific options.
     * @param {Date} d
     * @param {String} format
     */
    sakai.inbox.formatDate = function(d, format){
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
    
    /**
     * Detects whether a given date is within 24 of today, and returns the appropriate class name
     * @param {Object} date The date being compared to today
     */
    var compareDates = function(date){
        var dueDate = new Date(date);
        var today = new Date();
        var tomorrow = today.setDate(today.getDate() + 1);
        tomorrow = new Date(tomorrow);
        return (tomorrow > dueDate) ? "inbox-subject-critical" : "";
    }
    
    /**
     * Updates specified property of a reminder with the specified value
     * @param {Object} url jcr:path of the reminder being updated
     * @param {Object} JSON obj containing name/value pairs to be updated, ex. {"taskState":"completed"}
     * @param {Object} callback Function to be called on completion
     */
    var updateReminder = function(url, props, callback){
        $.ajax({
            type: 'POST',
            url: url,
            data: props,
            success: function(data, textStatus, xhr){
                if (typeof callback !== "undefined") {
                    callback();
                }
            },
            error: function(xhr, textStatus, thrownError){
                alert("Updating " + url + " failed for " + propname + " = " + propvalue + " with status =" + textStatus +
                " and thrownError = " +
                thrownError +
                "\n" +
                xhr.responseText);
            },
            dataType: 'json'
        });
    };
    
    /**
     * Formats the due date property for reminder
     * @param {Object} reminder A reminder object
     */
    var formatReminder = function(reminder){
        var dateString = reminder["sakai:dueDate"];
        var d = new Date();
        d.setFullYear(parseInt(dateString.substring(0, 4), 10));
        d.setMonth(parseInt(dateString.substring(5, 7), 10) - 1);
        d.setDate(parseInt(dateString.substring(8, 10), 10));
        d.setHours(parseInt(dateString.substring(11, 13), 10));
        d.setMinutes(parseInt(dateString.substring(14, 16), 10));
        d.setSeconds(parseInt(dateString.substring(17, 19), 10));
        //Jan 22, 2009 10:25 PM
        reminder.date = sakai.inbox.formatDate(d, "M j, Y g:i A");
        
        return reminder;
    }
    
    /**
     * Adds the correct format to a message.
     * ex: parsing the date
     * @param {Object} message
     */
    var formatMessage = function(message){
        var dateString = message["sakai:created"];
        var d = new Date();
        d.setFullYear(parseInt(dateString.substring(0, 4), 10));
        d.setMonth(parseInt(dateString.substring(5, 7), 10) - 1);
        d.setDate(parseInt(dateString.substring(8, 10), 10));
        d.setHours(parseInt(dateString.substring(11, 13), 10));
        d.setMinutes(parseInt(dateString.substring(14, 16), 10));
        d.setSeconds(parseInt(dateString.substring(17, 19), 10));
        //Jan 22, 2009 10:25 PM
        message.date = sakai.inbox.formatDate(d, "M j, Y g:i A");
        
        if (message["sakai:read"] === "true" || message["sakai:read"] === true) {
            message.read = true;
        }
        else {
            message.read = false;
        }
        
        if (message["sakai:category"] === "message" || message["sakai:category"] === undefined) {
            message.category = "Message";
        }
        else 
            if (message["sakai:category"] === "announcement") {
                message.category = "Announcement";
            }
            else 
                if (message["sakai:category"] === "invitation") {
                    message.category = "Invitation";
                }
                else 
                    if (message["sakai:category"] === "chat") {
                        message.category = "Chat";
                    }
        
        if (message.previousMessage) {
            message.previousMessage = formatMessage(message.previousMessage);
        }
        
        // A chat message doesn't really have subject, only a body.
        if (message["sakai:type"] === "chat") {
            message.subject = "Chat message";
        }
        
        // pictures
        if (message.userFrom && $.isArray(message.userFrom)) {
            for (var i = 0, il = message.userFrom.length; i < il; i++) {
                if (message.userFrom[i].picture && $.parseJSON(message.userFrom[i].picture).name) {
                    message.userFrom[i].photo = $.parseJSON(message.userFrom[i].picture).name;
                }
            }
        }
        
        if (message.userTo && $.isArray(message.To)) {
            for (var j = 0, jl = message.userTo.length; j < jl; j++) {
                if (message.userTo[j].picture && $.parseJSON(message.userTo[j].picture).name) {
                    message.userTo[j].photo = $.parseJSON(message.userTo[j].picture).name;
                }
            }
        }
        
        return message;
    };
    
    /**
     * Renders the messages and reminders.
     * @param {Object} The JSON response from the server. Make sure it has a .message array in it.
     */
    var renderMessages = function(response){
        for (var i = 0, k = response.results.length; i < k; i++) {
        
            if (box === "inbox" && cats === "" && response.results[i]["sakai:category"] === "chat") {
                response.results.splice(i, 1);
                // We are modifying the array we are iterating. We need to adjust the length otherwise we end up with undefined array elements
                k--;
            }
        }
        
        for (var j = 0, l = response.results.length; j < l; j++) {
            // temporary internal id.
            // Use the name for the id.
            response.results[j].nr = j;
            response.results[j].subject = response.results[j]["sakai:subject"];
            response.results[j].body = response.results[j]["sakai:body"];
            response.results[j].messagebox = response.results[j]["sakai:messagebox"];
            
            // style notice according to whether it's a message or reminder
            if (response.results[j]["sakai:category"] === "reminder") {
                response.results[j] = formatReminder(response.results[j]);
            }
            else {
                response.results[j] = formatMessage(response.results[j]);
            }
        }
        
        allMessages = response.results;
        
        // Show messages
        var tplData = {
            "messages": response.results
        };
        
        // remove previous messages
        removeAllMessagesOutDOM();
        
        // Add them to the DOM
        $(inboxTable).children("tbody").append($.TemplateRenderer(inboxTableMessagesTemplate, tplData));
        
        // do checkboxes
        tickMessages();
        
        // if the notice is a reminder style accordingly
        if (currentFilter == inboxFilterReminders) {
            for (var p = 0, q = response.results.length; p < q; p++) {
                var tableRow = $(inboxTableMessageID + response.results[p]["jcr:name"]);
                var rowCheckbox = $("#inbox_checkbox_" + response.results[p]["jcr:name"], tableRow);
                
                var critical = compareDates(response.results[p]["sakai:dueDate"]);
                tableRow.addClass(critical);
                tableRow.data("data", response.results[p]);
                $(".inbox_inbox_check_message").attr("title", "Completed");
                
                rowCheckbox.one("click", function(evt){
                    var id = evt.target.id;
                    id = id.split("_");
                    id = id[id.length - 1];
                    
                    var reminderData = $(inboxTableMessageID + id).data("data");
                    var path = reminderData["jcr:path"];
                    
                    var propertyToUpdate = {
                        "sakai:taskState": "completed",
                        "sakai:messagebox": "archive"
                    };
                    
                    updateReminder(path, propertyToUpdate, function(){
                        $(inboxTableMessageID + id).empty();
                        $(inboxTableMessageID + id).remove();
                        showGeneralMessage($(inboxGeneralMessagesCompleted).text());
                    });
                });
            }
        }
        else {
            $(".inbox_inbox_check_message").attr("title", "Delete");
        }
    };
    
    /**
     *
     * Pager
     *
     */
    /**
     * Show a certain page of messages.
     * @param {int} pageNumber The page number you want to display.
     */
    var showPage = function(pageNumber){
        // Remove all messages
        // remove previous messages
        removeAllMessagesOutDOM();
        // Set the pager
        pageMessages(pageNumber);
        // Remember which page we're on.
        currentPage = pageNumber - 1;
        // Show set of messages
        getAllMessages();
    };
    
    /**
     * Draw up the pager at the bottom of the page.
     * @param {int} pageNumber The number of the current page
     */
    pageMessages = function(pageNumber){
        $(inboxPager).pager({
            pagenumber: pageNumber,
            pagecount: Math.ceil(messagesForTypeCat / messagesPerPage),
            buttonClickCallback: showPage
        });
        curentPage = pageNumber;
    };
    
    
    /**
     *
     * Server functions
     *
     */
    /**
     * Gets all the messages from the JCR.
     */
    getAllMessages = function(callback){
        box = "inbox";
        if (selectedType === "sent") {
            box = "outbox";
        }
        else 
            if (selectedType === "trash") {
                box = "trash";
            }
            else 
                if (selectedType === "archive") {
                    box = "archive";
                }
        
        var url = sakai.config.URL.MESSAGE_BOX_SERVICE + "?box=" + box + "&items=" + messagesPerPage + "&page=" + currentPage;
        
        var types = "&types=" + selectedType;
        if (typeof selectedType === "undefined" || selectedType === "") {
            types = "";
        }
        else 
            if (typeof selectedType === "Array") {
                types = "&types=" + selectedType.join(",");
            }
        
        cats = selectedCategory;
        if (selectedCategory) {
            if (selectedCategory === "Message") {
                cats = "message";
            }
            else 
                if (selectedCategory === "Announcement") {
                    cats = "announcement";
                }
                else 
                    if (selectedCategory === "Invitation") {
                        cats = "invitation";
                    }
                    else 
                        if (selectedCategory === "Chat") {
                            cats = "chat";
                        }
                        else 
                            if (selectedCategory === "Reminder") {
                                cats = "reminder";
                            }
            url = sakai.config.URL.MESSAGE_BOXCATEGORY_SERVICE + "?box=" + box + "&category=" + cats + "&items=" + messagesPerPage + "&page=" + currentPage;
        }
        $.ajax({
            url: url,
            cache: false,
            success: function(data){
                if (data.results) {
                    // Render the messages
                    renderMessages(data);
                }
                if (typeof callback !== "undefined") {
                    callback();
                }
                
            },
            error: function(xhr, textStatus, thrownError){
                showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
                $(inboxResults).html(sakai.api.Security.saneHTML($(inboxGeneralMessagesErrorGeneral).text()));
            }
        });
    };
    
    /**
     * Updates the number of unread messages/reminders/etc.; this number is displayed next to each filter
     */
    var updateUnreadNumbers = function(){
    
        if (unreadMessages > 0) {
            $("#inbox_unread_nr_messages").text(sakai.api.Security.saneHTML("(" + unreadMessages + ")"));
        }
        else {
            $("#inbox_unread_nr_messages").text("");
        }
        
        if (unreadAnnouncements > 0) {
            $("#inbox_unread_nr_announcements").text(sakai.api.Security.saneHTML("(" + unreadAnnouncements + ")"));
        }
        else {
            $("#inbox_unread_nr_announcements").text("");
        }
        
        if (unreadInvitations > 0) {
            $("#inbox_unread_nr_invitations").text(sakai.api.Security.saneHTML("(" + unreadInvitations + ")"));
        }
        else {
            $("#inbox_unread_nr_invitations").text("");
        }
        
        if (unreadReminders > 0) {
            $("#inbox_unread_nr_reminders").text(sakai.api.Security.saneHTML("(" + unreadReminders + ")"));
        }
        else {
            $("#inbox_unread_nr_reminders").text("");
        }
        
    };
    
    /**
     * Will do a count of all the unread messages and change the values in the DOM.
     * Note: This function will only check the nr of messages there are. It will not fetch them!
     */
    var showUnreadMessages = function(){
        $.ajax({
            url: "/~" + sakai.data.me.user.userid + "/message.count.json?filters=sakai:messagebox,sakai:read&values=inbox,false&groupedby=sakai:category",
            cache: false,
            success: function(data){
                var totalcount = 0;
                for (var i = 0, j = data.count.length; i < j; i++) {
                    if (data.count[i].group === "message") {
                        unreadMessages = data.count[i].count;
                    }
                    else 
                        if (data.count[i].group === "announcement") {
                            unreadAnnouncements = data.count[i].count;
                        }
                        else 
                            if (data.count[i].group === "invitation") {
                                unreadInvitations = data.count[i].count;
                            }
                            else 
                                if (data.count[i].group === "chat") {
                                    $(inboxFilterChats).append(sakai.api.Security.saneHTML(tpl.replace(/__NR__/gi, data.count[i].count)));
                                }
                                else 
                                    if (data.count[i].group === "reminder") {
                                        unreadReminders = data.count[i].count;
                                    }
                    totalcount += data.count[i].count;
                }
                
                updateUnreadNumbers();
            },
            error: function(xhr, textStatus, thrownError){
                showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
            }
        });
    };
    
    /**
     *
     * Display specific message
     *
     */
    /**
     * Get the message out of the list with the specific id.
     * @param {String} id    The id of a message
     */
    var getMessageWithId = function(id){
        for (var i = 0, j = allMessages.length; i < j; i++) {
            if (allMessages[i]["jcr:name"] === id) {
                return allMessages[i];
            }
        }
        
        return {};
    };
    
    /**
     * Mark a message as read.
     * @param {Object} message The JSON object that represents the message.
     * @param {String} id The id for this message.
     */
    var markMessageRead = function(message, id){
        var postParameters = {
            "sakai:read": "true"
        };
        
        $.ajax({
            type: "POST",
            url: message["jcr:path"] + ".json",
            data: postParameters,
            success: function(userdata){
                for (var i = 0, j = allMessages.length; i < j; i++) {
                    if (allMessages[i].id === message.id) {
                        allMessages[i]["sakai:read"] = true;
                        break;
                    }
                }
                // Mark the message in the inbox table as read.         
                var tableRow = $(inboxTableMessageID + id);
                tableRow.addClass(inboxTablesubjectReadClass);
                tableRow.removeClass(inboxTablesubjectUnreadClass);
                
                if (currentFilter != inboxFilterTrash) {
                    if (message["sakai:category"] === "message") {
                        unreadMessages -= 1;
                    }
                    else 
                        if (message["sakai:category"] === "invitation") {
                            unreadInvitations -= 1;
                        }
                        else 
                            if (message["sakai:category"] === "announcement") {
                                unreadAnnouncements -= 1;
                            }
                            else 
                                if (message["sakai:category"] === "reminder") {
                                    unreadReminders -= 1;
                                }
                    
                    updateUnreadNumbers();
                }
                
                // Mark message in navigationchat as read
                if (currentFilter != inboxFilterTrash) {
                    $(chatUnreadMessages).html(sakai.api.Security.saneHTML((parseInt($(chatUnreadMessages).text(), 10) - 1)));
                }
                
            },
            error: function(xhr, textStatus, thrownError){
                showGeneralMessage($(inboxGeneralMessagesErrorReadFail).text());
            }
        });
    };
    
    /**
     * Displays only the message with that id.
     * @param {String} id    The id of a message
     */
    var displayMessage = function(id){
        $(".message-options").show();
        $("#inbox_message_previous_messages").hide();
        $("#inbox_message_replies").html("");
        
        // Hide/show relevant buttons
        $(inboxInboxBackToList).show();
        $(inboxInboxEmptyTrash).hide();
        $(inboxInboxDelete).hide();
        if (currentFilter === inboxFilterMessages || currentFilter === inboxFilterTrash) {
            $(inboxSpecificMessageDelete).show();
        }
        else {
            $(inboxSpecificMessageDelete).hide();
        }
        
        // Hide invitation links
        $("#inbox-invitation-accept").hide();
        $("#inbox-invitation-already").hide();
        $("#inbox-sitejoin-accept").hide();
        $("#inbox-sitejoin-deny").hide();
        $("#inbox-sitejoin-already").hide();
        
        showPane(inboxPaneMessage);
        
        var message = getMessageWithId(id);
        
        selectedMessage = message;
        if (typeof message !== "undefined") {
        
            // Fill in this message values.
            $(inboxSpecificMessageSubject).text(sakai.api.Security.saneHTML(message["sakai:subject"]));
            $(inboxSpecificMessageBody).html(sakai.api.Security.saneHTML(message["sakai:body"].replace(/\n/gi, "<br />")));
            $(inboxSpecificMessageDate).text(sakai.api.Security.saneHTML(message.date));
            
            if (message.userFrom) {
                for (var i = 0, j = message.userFrom.length; i < j; i++) {
                    $(inboxSpecificMessageFrom).text(sakai.api.User.getDisplayName(message.userFrom[i]));
                    if (message.userFrom[i].photo) {
                        $(inboxSpecificMessagePicture).attr("src", "/~" + message.userFrom[i]["rep:userId"] + "/public/profile/" + message.userFrom[i].photo);
                    }
                    else {
                        $(inboxSpecificMessagePicture).attr("src", sakai.config.URL.USER_DEFAULT_ICON_URL);
                    }
                }
            }
            else {
                $(inboxSpecificMessageFrom).text(sakai.api.Security.saneHTML(message["sakai:from"]));
                $(inboxSpecificMessagePicture).attr("src", sakai.config.URL.USER_DEFAULT_ICON_URL);
            }
            
            // Reply part.
            $(inboxSpecificMessageComposeSubject).val("Re: " + message.subject);
            if (message["sakai:category"] === "invitation") {
                if (message["sakai:subcategory"] === "joinrequest") {
                    $.getJSON(message["sakai:sitepath"] + '/joinrequests/' + message.userFrom[0].hash + '.json', function(data){
                        var siteJoinRequestIsPending = (data["sakai:requestState"] && data["sakai:requestState"] === "pending");
                        if (siteJoinRequestIsPending) {
                            $("#inbox-sitejoin-accept").show();
                            $("#inbox-sitejoin-deny").show();
                        }
                        else {
                            $("#inbox-sitejoin-already").show();
                        }
                    });
                }
                else {
                    // Check whether this request is still pending
                    $.ajax({
                        url: "/var/contacts/invited?page=0&items=100",
                        success: function(data){
                            var pending = false;
                            for (var i = 0; i < data.results.length; i++) {
                                if (data.results[i].target === message["sakai:from"]) {
                                    // Still a pending invitation
                                    pending = true;
                                }
                            }
                            if (pending) {
                                $("#inbox-invitation-accept").show();
                            }
                            else {
                                $("#inbox-invitation-already").show();
                            }
                        }
                    });
                }
            }
            
            // This message has some replies attached to it.
            if (message["sakai:previousmessage"]) {
                $(inboxSpecificMessagePreviousMessages).show();
                var replieshtml = "";
                var replies = {};
                // We render the chat replies slightly differently.
                if (message["sakai:category"] === sakai.config.Messages.Categories.chat) {
                    $(".message-options").hide();
                    $("#inbox_message_previous_messages").hide();
                    replieshtml += $.TemplateRenderer(inboxSpecificMessageRepliesTemplateChats, message);
                }
                else {
                    var json = {
                        "message": message
                    };
                    replieshtml += $.TemplateRenderer(inboxSpecificMessageRepliesTemplate, json);
                }
                $(inboxSpecificMessageReplies).html(replieshtml);
            }
            else {
                // There are no replies hide the header that states 'Previous messages'.
                $(inboxSpecificMessagePreviousMessages).hide();
            }
            
            if (message["sakai:read"] === false) {
                // We haven't read this message yet. Mark it as read.
                markMessageRead(message, id);
            }
        }
        
    };
    
    /**
     *
     * ACCEPT INVITATION
     *
     */
    $("#inbox_message_accept_invitation").live("click", function(ev){
        var accepting = selectedMessage["sakai:from"];
        $.ajax({
            url: "/~" + sakai.data.me.user.userid + "/contacts.accept.html",
            type: "POST",
            data: {
                "targetUserId": accepting
            },
            success: function(data){
                $("#inbox-invitation-accept").hide();
                $("#inbox-invitation-already").show();
            }
        });
    });
    
    /**
     *
     * ACCEPT SITE JOIN REQUEST
     *
     */
    $("#inbox_message_accept_sitejoin").live("click", function(ev){
        var from = selectedMessage["sakai:from"];
        var sitePath = selectedMessage["sakai:sitepath"];
        $.ajax({
            url: sitePath + ".approve.html",
            type: "POST",
            data: {
                "user": from
            },
            success: function(data){
                $("#inbox-sitejoin-accept").hide();
                $("#inbox-sitejoin-deny").hide();
                $("#inbox-sitejoin-already").show();
            }
        });
    });
    
    /**
     *
     * DENY SITE JOIN REQUEST
     *
     */
    $("#inbox_message_deny_sitejoin").live("click", function(ev){
        var from = selectedMessage["sakai:from"];
        var sitePath = selectedMessage["sakai:sitepath"];
        $.ajax({
            url: sitePath + ".deny.html",
            type: "POST",
            data: {
                "user": from
            },
            success: function(data){
                $("#inbox-sitejoin-accept").hide();
                $("#inbox-sitejoin-deny").hide();
                $("#inbox-sitejoin-already").show();
            }
        });
    });
    
    /**
     *
     * SEND MESSAGE
     *
     */
    /**
     * When a message has been sent this function gets called.
     * @param {Object} data A JSON object that contains the response from the server.
     */
    var sendMessageFinished = function(success, data){
    
        showGeneralMessage($(inboxGeneralMessagesSent).text(), false);
        clearInputFields();
        
        // Show the sent inbox pane.
        filterMessages(sakai.config.Messages.Types.sent, "", "all", inboxFilterSent);
        $(inboxTableHeaderFromContent).text("To");
        
    };
    
    
    /**
     *
     * Delete a message
     *
     */
    /**
     * Removes all the messages from memory that are in pathToMessages if success = true
     * success = false will show an error.
     * @param {String[]} pathToMessages
     * @param {Boolean} success
     */
    var deleteMessagesFinished = function(pathToMessages, success){
        if (success) {
        
            // Repage the inbox
            currentPage = currentPage + 1;
            showPage(currentPage);
            
            var txt = "";
            if (pathToMessages.length === 1) {
                txt = $(inboxGeneralMessagesDeleted_1).text();
            }
            else {
                txt = pathToMessages.length + $(inboxGeneralMessagesDeleted_x).text();
            }
            
            showGeneralMessage(txt, false);
        }
        else {
            showGeneralMessage($(inboxGeneralMessagesDeletedFailed).text());
        }
    };
    
    /**
     * This will do a DELETE request to the specified path and harddelete that message.
     * @param {String[]} path The message that you want to delete.
     * @param {int} index The index of the array that needs to be deleted.
     */
    var hardDeleteMessage = function(pathToMessages){
        $.ajax({
            url: "/system/batch/delete",
            type: "POST",
            success: function(data){
                deleteMessagesFinished(pathToMessages, true);
            },
            error: function(xhr, textStatus, thrownError){
                deleteMessagesFinished(pathToMessages, false);
            },
            data: {
                "resources": pathToMessages,
                "_charset_": "utf-8"
            }
        });
    };
    
    /**
     * Delete all the messages that are in ids
     * @param {Array} ids    An array of ids that have to be deleted.
     */
    var deleteMessages = function(pathToMessages, hardDelete){
        if (typeof hardDelete === "undefined") {
            hardDelete = false;
        }
        if (hardDelete) {
            // We will have to do a hard delete to all the JCR files.
            hardDeleteMessage(pathToMessages);
        }
        else {
            var toDelete = pathToMessages.length;
            var deleted = 0;
            
            // Update unread number on left hand side
            var deletedUnreadMessages = 0;
            var deletedUnreadAnnouncements = 0;
            var deletedUnreadInvitations = 0;
            var deletedUnreadTotal = 0;
            
            for (var m = 0, n = toDelete; m < n; m++) {
                var toDeleteID = pathToMessages[m].split("/");
                toDeleteID = toDeleteID[toDeleteID.length - 1];
                for (var i = 0, j = allMessages.length; i < j; i++) {
                    if (allMessages[i].id === toDeleteID) {
                        if ((allMessages[i]["sakai:read"] === "false" || allMessages[i]["sakai:read"] === false) && allMessages[i]["sakai:category"]) {
                            if (allMessages[i]["sakai:category"] === "message") {
                                deletedUnreadMessages++;
                            }
                            else 
                                if (allMessages[i]["sakai:category"] === "invitation") {
                                    deletedUnreadInvitations++;
                                }
                                else 
                                    if (allMessages[i]["sakai:category"] === "announcement") {
                                        deletedUnreadAnnouncements++;
                                    }
                        }
                        break;
                    }
                }
            }
            unreadMessages -= deletedUnreadMessages;
            unreadAnnouncements -= deletedUnreadAnnouncements;
            unreadInvitations -= deletedUnreadInvitations;
            deletedUnreadTotal = deletedUnreadMessages + deletedUnreadAnnouncements + deletedUnreadInvitations;
            $(chatUnreadMessages).html(sakai.api.Security.saneHTML((parseInt($(chatUnreadMessages).text(), 10) - deletedUnreadTotal)));
            updateUnreadNumbers();
            
            for (var d = 0, e = pathToMessages.length; d < e; d++) {
                $.ajax({
                    url: pathToMessages[d],
                    type: "POST",
                    success: function(data){
                        deleted++;
                        if (deleted === toDelete) {
                            deleteMessagesFinished(pathToMessages, true);
                        }
                    },
                    error: function(xhr, textStatus, thrownError){
                        deleted++;
                        if (deleted === toDelete) {
                            deleteMessagesFinished(pathToMessages, false);
                        }
                    },
                    data: {
                        "sakai:messagebox": "trash",
                        "_charset_": "utf-8"
                    }
                });
            }
        }
    };
    
    
    /**
     *
     * Event Handling
     *
     */
    // Compose a new message.
    $(inboxComposeNew).click(function(){
        //    show the selector
        $(inboxComposeNewPanel).toggle();
        // set variable which tells us if the menu is open or closed
        if (inboxComposeNewPanelOpen) {
            inboxComposeNewPanelOpen = false;
        }
        else {
            inboxComposeNewPanelOpen = true;
        }
    });
    $(inboxComposeMessage).click(function(){
        showPane(inboxPaneCompose);
        
        // initialise the sendmessage widget
        // we tell it to show it in our id and NOT as a layover.
        sakai.sendmessage.initialise(null, true, inboxComposeNewContainer, sendMessageFinished);
    });
    
    //    This is the widget id!
    $(inboxComposeCancel).live("click", function(){
        //    Jump back to inbox
        showPane(inboxPaneInbox);
    });
    
    // Bind click event to hide menus
    $(document).bind("click", function(e){
        var $clicked = $(e.target);
        // Check if one of the parents is the element container AND check if menu is open
        if (!$clicked.parents().is(inboxComposeNew) && inboxComposeNewPanelOpen) {
            $(inboxComposeNewPanel).toggle();
            inboxComposeNewPanelOpen = false;
        }
    });
    
    
    /**
     *
     * Show a specific message
     *
     */
    $(inboxInboxMessage).live("click", function(e, ui){
    
        var id = e.target.id;
        id = id.split('_');
        displayMessage(id[id.length - 1]);
    });
    
    /* Filter the messages. */
    
    $(inboxFilterMessages).click(function(){
        filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.message, "all", inboxFilterMessages);
        currentFilter = inboxFilterMessages;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterMessages).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterReminders).click(function(){
        filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.reminder, "all", inboxFilterReminders);
        $(inboxTableHeaderDateContent).text("Due Date");
        $(inboxInboxDelete).hide();
        currentFilter = inboxFilterReminders;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterReminders).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterAnnouncements).click(function(){
        filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.announcement, "all", inboxFilterAnnouncements);
        currentFilter = inboxFilterAnnouncements;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterAnnouncements).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterChats).click(function(){
        filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.chat, "all", inboxFilterChats);
        currentFilter = inboxFilterChats;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterChats).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterInvitations).click(function(){
        filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.invitation, "all", inboxFilterInvitations);
        currentFilter = inboxFilterInvitations;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterInvitations).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterInbox).click(function(){
        filterMessages(sakai.config.Messages.Types.inbox, "", "all", inboxFilterInbox);
        currentFilter = inboxFilterInbox;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterInbox).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterSent).click(function(){
        filterMessages(sakai.config.Messages.Types.sent, "", "all", inboxFilterSent);
        $(inboxTableHeaderFromContent).text("To");
        currentFilter = inboxFilterSent;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterSent).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterArchive).click(function(){
        filterMessages(sakai.config.Messages.Types.archive, "", "all", inboxFilterArchive);
        $(inboxTableHeaderDateContent).text("Due Date");
        $(inboxInboxDelete).hide();
        currentFilter = inboxFilterArchive;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterArchive).parent();
        $(selectedFilterDiv).addClass("selected");
    });
    $(inboxFilterTrash).click(function(){
        filterMessages(sakai.config.Messages.Types.trash, "", "all", inboxFilterTrash);
        $(inboxInboxDelete).hide();
        $(inboxInboxEmptyTrash).show();
        currentFilter = inboxFilterTrash;
        $(inboxInboxCheckAll).attr("checked", '');
        tickMessages();
        $(selectedFilterDiv).removeClass("selected");
        selectedFilterDiv = $(inboxFilterTrash).parent().parent();
        $(selectedFilterDiv).addClass("selected");
    });
    
    
    // Check all messages
    $(inboxInboxCheckAll).change(function(){
        tickMessages();
    });
    
    // Deleting a message from the inbox display
    $(inboxInboxDelete).click(function(){
        // Delete all checked messages
        var pathToMessages = [];
        $(inboxInboxCheckMessage + ":checked").each(function(){
            var pathToMessage = $(this).val();
            pathToMessages.push(pathToMessage);
        });
        
        if (pathToMessages.length === 0) {
            showGeneralMessage($(inboxGeneralMessagesNoneSelected).text());
        }
        else {
            // If we are in trash we hard delete the messages
            deleteMessages(pathToMessages, (selectedType === sakai.config.Messages.Types.trash));
        }
    });
    
    // Emptying all the selected messages in Trash
    $(inboxInboxEmptyTrash).click(function(){
        // Delete all checked messages 
        var pathToMessages = [];
        $(inboxInboxCheckMessage + ":checked").each(function(){
            var pathToMessage = $(this).val();
            pathToMessages.push(pathToMessage);
        });
        
        if (pathToMessages.length === 0) {
            showGeneralMessage($(inboxGeneralMessagesNoneSelected).text());
        }
        else {
            // If we are in trash we hard delete the messages
            deleteMessages(pathToMessages, (selectedType === sakai.config.Messages.Types.trash));
            $(inboxInboxCheckAll).attr("checked", '');
            tickMessages();
        }
    });
    
    // Takes user back to the filter they were on before going into a specific message
    $(inboxInboxBackToList).click(function(){
        $(currentFilter).click();
        showPage(currentPage);
    });
    
    // Sorters for the inbox.
    $(inboxTableHeaderSort).bind("mouseenter", function(){
        if (sortOrder === 'descending') {
            $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortUp).html()));
        }
        else {
            $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortDown).html()));
        }
    });
    $(inboxTableHeaderSort).bind("mouseout", function(){
        $(inboxTable + " " + inboxArrow).remove();
    });
    $(inboxTableHeaderSort).bind("click", function(){
        sortBy = $(this).attr("id").replace(/inbox_tableHeader_/gi, "");
        sortOrder = (sortOrder === "descending") ? "ascending" : "descending";
        getAllMessages();
    });
    
    
    /**
     *
     * Specific message
     *
     */
    $(inboxSpecificMessageBackToInbox).click(function(){
        // Show the inbox.
        showPane(inboxPaneInbox);
        
        // Clear all the input fields
        clearInputFields();
    });
    
    $(inboxSpecificMessageOptionReply).click(function(){
        $(inboxSpecificMessageCompose).show();
        scrollTo($(inboxSpecificMessageCompose));
    });
    
    $(inboxSpecificMessageOptionDelete).click(function(){
        var harddelete = false;
        if ($.inArray(selectedMessage.types, "trash") > -1) {
            // This is a trashed message, hard delete it.
            harddelete = true;
        }
        
        // Delete the message
        deleteMessages([selectedMessage.pathToMessage], harddelete);
        
        // Show the inbox
        showPane(inboxPaneInbox);
        
        // Clear all the input fields
        clearInputFields();
    });
    
    // Deletes a message from inside the message
    $(inboxSpecificMessageDelete).click(function(){
        var harddelete = false;
        if (currentFilter === inboxFilterTrash) {
            harddelete = true;
        }
        
        // Delete the message
        var pathToMessages = [];
        pathToMessages.push(selectedMessage["jcr:path"]);
        deleteMessages(pathToMessages, harddelete);
        
        // Show the inbox
        showPane(inboxPaneInbox);
        
        // Clear all the input fields
        clearInputFields();
    });
    
    $(inboxSpecificMessageComposeCancel).click(function(){
        // Clear all the input fields
        clearInputFields();
    });
    
    $(inboxSpecificMessageComposeSend).click(function(){
        // We want to send a message.
        var subject = $(inboxSpecificMessageComposeSubject).val();
        var body = $(inboxSpecificMessageComposeBody).val();
        
        sakai.api.Communication.sendMessage(selectedMessage["sakai:from"], subject, body, "message", selectedMessage["sakai:id"], sendMessageFinished);
        
        // Clear all the input fields
        clearInputFields();
    });
    
    /**
     *
     * Init
     *
     */
    var doInit = function(){
        // Check if we are logged in or out.
        var person = sakai.data.me;
        var uuid = person.user.userid;
        if (!uuid || person.user.anon) {
            redirectToLoginPage();
        }
        else {
            // We are logged in. Do all the necessary stuff.
            // Load the list of messages.
            // getCount("all");
            // getAllMessages();
            showUnreadMessages();
            
            var qs = new Querystring();
            var qs_messageid = qs.get("message");
            if (qs.get("category") === "reminder") {
                selectedCategory = "reminder";
                currentFilter = inboxFilterReminders;
                $(selectedFilterDiv).removeClass("selected");
                selectedFilterDiv = $(inboxFilterReminders).parent();
                $(selectedFilterDiv).addClass("selected");
            }
            else {
                selectedCategory = "message";
                currentFilter = inboxFilterMessages;
                $(selectedFilterDiv).removeClass("selected");
                selectedFilterDiv = $(inboxFilterMessages).parent();
                $(selectedFilterDiv).addClass("selected");
            }
            
            if (qs_messageid) {
                var callback = function(){
                    displayMessage(qs_messageid);
                };
                
                getAllMessages(callback);
            }
            else {
            
                // Show messages by default (as if click on "Inbox > Messages")
                filterMessages(sakai.config.Messages.Types.inbox, "", "all", inboxFilterMessages);
                $(selectedFilterDiv).removeClass("selected");
                selectedFilterDiv = $(inboxFilterMessages).parent();
                $(selectedFilterDiv).addClass("selected");
            }
        }
    };
    
    doInit();
};

sakai.api.Widgets.Container.registerForLoad("sakai.inbox");
