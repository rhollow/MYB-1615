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
sakai.inbox = function() {

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
    var getAll = true;
    var allAllMessages = []; // Array that will truly hold all the messages in current filter, not limited to one page's worth


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
    var inboxTableMessage = inboxClass + "_message";    //    A row in the table
    var inboxTableMessageID = inboxTable + "_message_";
    var inboxTableMessagesTemplate = inbox + "_" + inbox + "_messages_template";
    var inboxTableSubject = inboxTable + "_subject_";
    var inboxTablesubjectReadClass = 'inbox-subject-read';
    var inboxTablesubjectUnreadClass = 'inbox-subject-unread';

    // subfolder labels
    var inboxSubfolderClass = ".inbox_subfolder";
    var inboxSubfolder = inboxID + "_subfolder";
    var inboxSubfolderChats = inboxSubfolder + "_chats";
    var inboxSubfolderMessages = inboxSubfolder + "_messages";
    var inboxSubfolderInvitations = inboxSubfolder + "_invitations";
    var inboxSubfolderAnnouncements = inboxSubfolder + "_announcements";
    var inboxSubfolderReminders = inboxSubfolder + "_reminders"; // myBerkeley: added subfolder for reminders
    var inboxSubfolderArchive = inboxSubfolder + "_archive"; // myBerkeley: added subfolder for reminders archive

    var inboxInbox = inboxID + "_inbox";
    var inboxInboxClass = inboxClass + "_inbox";

    var inboxInboxSortUp = inboxInbox + "_sort_up";
    var inboxInboxSortDown = inboxInbox + "_sort_down";

    var inboxInboxCheckAll = inboxInbox + "_checkAll";
    var inboxInboxDeleteButton = inboxInbox + "_delete_button";
    var inboxInboxArchiveCompletedButton = inboxInbox + "_archive_completed_button";
    var inboxInboxEmptyTrashButton = inboxInbox + "_empty_trash_button";
    var inboxInboxBackToListButton = inboxInbox + "_back_to_list_button";

    var inboxInboxMessage = inboxInboxClass + "_message";
    var inboxInboxHeader = inboxInboxClass + "_header";
    var inboxInboxCheckMessage = inboxInboxClass + "_check_message";
    var inboxInboxCheckDone = inboxInboxClass + "_check_done";

    var inboxTableHeaderSort = inboxInboxClass + "_table_header_sort";

    // Specific message
    var inboxSpecificMessage = inboxID + "_message";
    var inboxSpecificMessageDeleteButton = inboxSpecificMessage + "_delete_button";
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
    
    var inboxComposeForm = ".compose-form";

    // Viewing reminders
    var inboxReminder = inboxID + "_reminder";

    // Errors and messages
    var inboxGeneralMessages = inboxID + "_generalmessages";
    var inboxGeneralMessagesError = inboxGeneralMessages + "_error";
    var inboxGeneralMessagesErrorGeneral = inboxGeneralMessagesError + "_general";
    var inboxGeneralMessagesErrorReadFail = inboxGeneralMessagesError + "_read_fail";
    var inboxGeneralMessagesNrNewMessages = inboxGeneralMessages + "_nr_new_messages";
    var inboxGeneralMessagesNoneSelectedMessages = inboxGeneralMessages + "_none_selected_messages";
    var inboxGeneralMessagesNoneSelectedReminders = inboxGeneralMessages + "_none_selected_reminders";
    var inboxGeneralMessagesDeleted = inboxGeneralMessages + "_deleted";
    var inboxGeneralMessagesDeleted_1 = inboxGeneralMessagesDeleted + "_1";
    var inboxGeneralMessagesDeleted_x = inboxGeneralMessagesDeleted + "_x";
    var inboxGeneralMessagesSent = inboxGeneralMessages + "_sent";
    var inboxGeneralMessagesCompleted = inboxGeneralMessages + "_completed";
    var inboxGeneralMessagesNotCompleted = inboxGeneralMessages + "_not_completed";
    var inboxGeneralMessagesArchived1 = inboxGeneralMessages + "_archived_1";
    var inboxGeneralMessagesArchivedX = inboxGeneralMessages + "_archived_x";
    var inboxGeneralMessagesDeletedFailed = inboxGeneralMessagesDeleted + "_failed";
    var inboxGeneralMessagesSendFailed = inboxGeneralMessages + "_send_fail";

    // other IDs
    var chatUnreadMessages = "#chat_unreadMessages";
    var currentFilter = ""; // myBerkeley: added to keep track of current filter

    // Keep JSLint.com happy...
    var pageMessages = function() {};
    var getCount = function() {};
    var getAllMessages = function() {};

    /**
     *
     * Aid Functions
     *
     */

    var unreadMessages = 0;
    var unreadInvitations = 0;
    var unreadAnnouncements = 0;
    var unreadChats = 0;
    var unreadReminders = 0;

    /**
     * This function will redirect the user to the login page.
     */
    var redirectToLoginPage = function() {
        document.location = sakai.config.URL.GATEWAY_URL;
    };

    /**
     * This will show the preloader.
     */
    var showLoader = function() {
        $(inboxTable).append($.TemplateRenderer(inboxTablePreloader.substring(1), {}));
    };

    /**
     * Scroll to a specific element in a page
     * @param {Object} element The element you want to scroll to
     */
    var scrollTo = function(element) {
        $("html, body").animate({
            scrollTop: element.offset().top
        }, 1);
    };

    /**
     * Shows a general message on the top screen
     * @param {String} msg    the message you want to display
     * @param {Boolean} isError    true for error (red block)/false for normal message(green block)
     */
    var showGeneralMessage = function(msg, isError) {

        // Check whether to show an error type message or an information one
        var type = isError ? sakai.api.Util.notification.type.ERROR : sakai.api.Util.notification.type.INFORMATION;

        // Show the message to the user
        sakai.api.Util.notification.show("", msg, type);

    };

    /**
     * This will hide all the panes (the inbox, new reply, view message, etc..)
     */
    var hideAllPanes = function() {
        $(inboxPaneClass).hide();
    };

    /**
     * Will show the required pane and hide all the others.
     * @param {String} the Id of the pane you want to show
     */
    var showPane = function(pane) {
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
    var tickMessages = function() {
        $(inboxInboxCheckMessage).attr("checked", ($(inboxInboxCheckAll).is(":checked") ? "checked" : ''));
    };

    /**
     * This will display the first page of the specified messages
     * @param {String} type The type of the messages (inbox, sent or trash or * for all of them)
     * @param {String} category The category of the messages (chat, invitation, ... or * for all of them)
     * @param {String} read Whether we should fetch messages that are read, unread or all of them. Option: true, false, all
     * @param {String} id The id of the filter that got clicked in the side panel.
     */
    var filterMessages = function(type, category, read, id) {
        $(inboxTableHeaderFromContent).text("From");
        
        // Show/hide appropriate buttons
        $(inboxInboxDeleteButton).show();
        $(".different").show();
        $(".different_reminders").hide();
        $(inboxInboxArchiveCompletedButton).hide();
        $(inboxSpecificMessageDeleteButton).hide();
        $(inboxInboxEmptyTrashButton).hide();
        $(inboxInboxBackToListButton).hide();
        
        // The small header above the webpage
        $(inboxInboxHeader).hide();
        var temp = type;
        if(type === "inbox" || type === "archive") { // myBerkeley: check if reminders header needs to be displayed
            if (category === "Reminder") {
                temp = "reminders";
            }
        }
        $(inboxID + "_" + temp).show();

        // Remember the type and category we want to see.
        selectedType = type;
        selectedCategory = category;
        
        // Display first page.
        //getCount(read);
        getAllMessages();

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
    var removeAllMessagesOutDOM = function() {
        $(inboxTableMessage).remove();
    };

    /**
     * This method will clear the input fields for the reply part of a specific message.
     * It will also hide the form again.
     */
    var clearInputFields = function() {
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
    var formatDate = function(d, format) {
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
                alert("Updating " + url + " failed with status =" + textStatus + " and thrownError = " + thrownError + "\n" + xhr.responseText);
            },
            dataType: 'json'
        });
    };
    
    /**
     * Formats the read property and the due date property for a reminder
     * @param {Object} reminder A reminder object
     */
    var formatReminder = function(reminder){
        if (reminder["sakai:read"] === "true" || reminder["sakai:read"] === true) {
            reminder.read = true;
        } else {
            reminder.read = false;
        }
        
        var dateString;
        
        if (reminder["sakai:dueDate"] != null) {
            dateString = reminder["sakai:dueDate"];
        } else if(reminder["sakai:eventDate"] != null) {
            dateString = reminder["sakai:eventDate"];
        }
        
        var d = sakai.api.Util.parseSakaiDate(dateString);
        //format Jan 22, 2009 10:25 PM
        reminder.date = formatDate(d, "M j, Y g:i A");
        
        return reminder;
    }
    
    /**
     * Adds the correct format to a message.
     * ex: parsing the date
     * @param {Object} message
     */
    var formatMessage = function(message) {

        var dateString = message["sakai:created"];
        var d = sakai.api.Util.parseSakaiDate(dateString);
        //Jan 22, 2009 10:25 PM
        message.date = formatDate(d, "M j, Y g:i A"); // myBerkeley: changed time format G -> g so not military time

        if (message["sakai:read"] === "true" || message["sakai:read"] === true) {
            message.read = true;
        } else {
            message.read = false;
        }

        if (message["sakai:category"] === "message" || message["sakai:category"] === undefined){
            message.category = "Message";
        } else if (message["sakai:category"] === "announcement"){
            message.category = "Announcement";
        } else if (message["sakai:category"] === "invitation"){
            message.category = "Invitation";
        } else if (message["sakai:category"] === "chat"){
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
    var renderMessages = function(response) {

        if (!getAll) {
            for (var i = 0, k = response.results.length; i < k; i++) {

                if (box === "inbox" && cats === "" && response.results[i]["sakai:category"] === "chat") {
                    response.results.splice(i, 1);
                    // We are modifying the array we are iterating. We need to adjust the length otherwise we end up with undefined array elements
                    k--;
                }
            }
        } else {
            getAll = false;
        }

        for (var j = 0, l = response.results.length; j < l; j++) {
            // temporary internal id.
            // Use the name for the id.
            response.results[j].nr = j;
            response.results[j].subject = sakai.api.Security.escapeHTML(response.results[j]["sakai:subject"]);
            response.results[j].body = response.results[j]["sakai:body"];
            response.results[j].messagebox = response.results[j]["sakai:messagebox"];
            
            // style notice according to whether it's a message or reminder
            if (response.results[j]["sakai:category"] === "reminder") {
                response.results[j] = formatReminder(response.results[j]);
            } else {
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

        messagesForTypeCat = response.total;
        pageMessages(currentPage+1);

        // do checkboxes
        tickMessages();
        
        // if the notice is a reminder (either in the inbox or archive), style accordingly
        if (selectedCategory === "Reminder") {
            for (var p = 0, q = response.results.length; p < q; p++) {
                var tableRow = $(inboxTableMessageID + response.results[p]["jcr:name"]);
                var rowCheckbox = $("#inbox_checkbox_" + response.results[p]["jcr:name"], tableRow);
                $(inboxInboxCheckDone).attr("title", "Completed");
                tableRow.data("data", response.results[p]);
                
                if(response.results[p]["sakai:taskState"] != "completed") {
                    var critical;
                    if (response.results[p]["sakai:dueDate"] != null) {
                        critical = compareDates(response.results[p]["sakai:dueDate"]);
                    } else if (response.results[p]["sakai:eventDate"]){
                        critical = compareDates(response.results[p]["sakai:eventDate"]);
                    }
                    tableRow.addClass(critical);
                } else {
                    rowCheckbox.attr("checked", true);
                }
                
                rowCheckbox.live("click", function(evt){
                    var id = evt.target.id;
                    id = id.split("_");
                    id = id[id.length - 1];
                    
                    var rowCheckBox = "#inbox_checkbox_" + id;
                    var reminderData = $(inboxTableMessageID + id).data("data");
                    var path = reminderData["jcr:path"];
                    
                    var propertyToUpdate;
                    var funct;
                    
                    if (reminderData["sakai:taskState"] === "completed") {
                        if (reminderData["sakai:messagebox"] === "archive") {
                            if(reminderData["sakai:read"] === false) {
                                unreadReminders += 1;
                                $(chatUnreadMessages).html(sakai.api.Security.saneHTML((parseInt($(chatUnreadMessages).text(), 10) + 1)));
                                updateUnreadNumbers();
                            }
                            propertyToUpdate = {
                                "sakai:taskState": "created",
                                "sakai:messagebox": "inbox"
                            };
                            funct = function(){
                                $(inboxTableMessageID + id).empty();
                                $(inboxTableMessageID + id).remove();
                            }
                        } else {
                            propertyToUpdate = {
                                "sakai:taskState": "created"
                            };
                        }
                    } else {
                        propertyToUpdate = {
                            "sakai:taskState": "completed"
                        };
                    }
                    updateReminder(path, propertyToUpdate, funct);
                });
            }
        } else {
            $(inboxInboxCheckMessage).attr("title", "Delete");
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
    var showPage = function(pageNumber) {
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
    pageMessages = function(pageNumber) {
        $(inboxPager).pager({
            pagenumber: pageNumber,
            pagecount: Math.ceil(messagesForTypeCat / messagesPerPage),
            buttonClickCallback: showPage
        });
    };


    /**
     *
     * Server functions
     *
     */

    /**
     * Gets all the messages from the JCR.
     */
    getAllMessages = function(callback) {
        box = "inbox";
        if (selectedType === "sent"){
            box = "outbox";
        } else if (selectedType === "trash"){
            box = "trash";
        } else if (selectedType === "archive") {
            box = "archive";
        }

        var url = sakai.config.URL.MESSAGE_BOX_SERVICE + "?box=" + box + "&items=" + messagesPerPage + "&page=" + currentPage;

        var types = "&types=" + selectedType;
        if (typeof selectedType === "undefined" || selectedType === "") {
            types = "";
        }
        else if (typeof selectedType === "Array") {
            types = "&types=" + selectedType.join(",");
        }

        cats = selectedCategory;
        if (selectedCategory){
            if (selectedCategory === "Message"){
                cats = "message";
            } else if (selectedCategory === "Announcement"){
                cats = "announcement";
            } else if (selectedCategory === "Invitation"){
                cats = "invitation";
            } else if (selectedCategory === "Chat"){
                cats = "chat";
            } else if (selectedCategory === "Reminder"){ // myBerkeley: added reminder category
                cats = "reminder";
            }
            url = sakai.config.URL.MESSAGE_BOXCATEGORY_SERVICE + "?box=" + box + "&category=" + cats + "&items=" + messagesPerPage + "&page=" + currentPage;
        }

        switch(sortBy) {
            case "date":
                sortBy = "sakai:created";
                break;
            case "type":
                sortBy = "sakai:type";
                break;
            case "to":
                sortBy = "sakai:to";
                break;
            case "from":
            case "from_reminder":
                sortBy = "sakai:from";
                break;
            case "subject":
            case "subject_reminder":
                sortBy = "sakai:subject";
                break;
            case "date_reminder": // myBerkeley: added sort by due date
                sortBy = "sakai:dueDate";
                break;
            case "checkbox_reminder": // myBerkeley: added sort by completed
                sortBy = "sakai:taskState";
                break;
        }

        url += "&sortOn=" + sortBy + "&sortOrder=" + sortOrder;
        
        var url2 = sakai.config.URL.MESSAGE_BOXCATEGORY_SERVICE + "?box=" + box + "&category=" + cats;

        $.ajax({
            url: url,
            cache: false,
            success: function(data){
                if (data.results) {
                    // Render the messages
                    renderMessages(data);
                    showUnreadMessages();
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

        $.ajax({
            url: url2,
            cache: false,
            success: function(data) {
                if (data.results) {
                    allAllMessages = data.results;
                }
            },
            error: function(xhr, textStatus, thrownError) {
                showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
                $(inboxResults).html(sakai.api.Security.saneHTML($(inboxGeneralMessagesErrorGeneral).text()));
            }
        });
    };

    /**
     * Will do a count of all the unread messages and change the values in the DOM.
     * Note: This function will only check the nr of messages there are. It will not fetch them!
     */
    // myBerkeley: changed ajax call because myBerkeley messages are type=notice, not type=internal
    var showUnreadMessages = function() {
        $.ajax({
            url: sakai.config.URL.MESSAGE_BOXCATEGORY_SERVICE + "?box=inbox&category=reminder",
            cache: false,
            success: function(data){
                var temp = 0;
                for (var i = 0, j = data.results.length; i < j; i++) {
                    if (data.results[i]["sakai:read"] == false) {
                        temp++;
                    }
                }
                unreadReminders = temp;
                updateUnreadNumbers();
            },
            error: function(xhr, textStatus, thrownError){
                showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
                $(inboxResults).html(sakai.api.Security.saneHTML($(inboxGeneralMessagesErrorGeneral).text()));
            }
        });
        $.ajax({
            url: sakai.config.URL.MESSAGE_BOXCATEGORY_SERVICE + "?box=inbox&category=message",
            cache: false,
            success: function(data){
                var temp = 0;
                for (var i = 0, j = data.results.length; i < j; i++) {
                    if (data.results[i]["sakai:read"] == false) {
                        temp++;
                    }
                }
                unreadMessages = temp;
                updateUnreadNumbers();
            },
            error: function(xhr, textStatus, thrownError){
                showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
                $(inboxResults).html(sakai.api.Security.saneHTML($(inboxGeneralMessagesErrorGeneral).text()));
            }
        });
    };

    var updateUnreadNumbers = function(){
        if (unreadMessages > 0){
            $("#inbox_unread_nr_messages").text(sakai.api.Security.saneHTML("(" + unreadMessages + ")"));
        } else {
            $("#inbox_unread_nr_messages").text("");
            $(chatUnreadMessages).html("0");
        }

        if (unreadAnnouncements > 0){
            $("#inbox_unread_nr_announcements").text(sakai.api.Security.saneHTML("(" + unreadAnnouncements + ")"));
        } else {
            $("#inbox_unread_nr_announcements").text("");
        }

        if (unreadInvitations > 0){
            $("#inbox_unread_nr_invitations").text(sakai.api.Security.saneHTML("(" + unreadInvitations + ")"));
        } else {
            $("#inbox_unread_nr_invitations").text("");
        }

        // myBerkeley: added reminders category
        if (unreadReminders > 0) {
            $("#inbox_unread_nr_reminders").text(sakai.api.Security.saneHTML("(" + unreadReminders + ")"));
        } else {
            $("#inbox_unread_nr_reminders").text("");
        }
        
        var totalUnread = unreadMessages + unreadReminders;
        $(chatUnreadMessages).html(sakai.api.Security.saneHTML(totalUnread)); // myBerkeley: moved this line out
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
    var getMessageWithId = function(id) {

        for (var i = 0, j=allMessages.length; i<j; i++) {
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
    var markMessageRead = function(message, id) {
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
                $(inboxTableMessageID + id).addClass(inboxTablesubjectReadClass);
                $(inboxTableMessageID + id).removeClass(inboxTablesubjectUnreadClass);
                
                if (message["sakai:messagebox"] === "inbox") {
                    if (message["sakai:category"] === "message") {
                        unreadMessages -= 1;
                    } else if (message["sakai:category"] === "invitation") {
                        unreadInvitations -= 1;
                    } else if (message["sakai:category"] === "announcement") {
                        unreadAnnouncements -= 1;
                    } else if (message["sakai:category"] === "reminder") { // myBerkeley: added reminders category
                        unreadReminders -= 1;
                    }

                    updateUnreadNumbers();
                }
                
            },
            error: function(xhr, textStatus, thrownError) {
                showGeneralMessage($(inboxGeneralMessagesErrorReadFail).text());
            }
        });
    };

    /**
     * Displays only the message with that id.
     * @param {String} id    The id of a message
     */
    var displayMessage = function(id) {

        $(".message-options").show();
        $("#inbox_message_previous_messages").hide();
        $("#inbox_message_replies").html("");
        
        // Hide/show relevant buttons
        $(inboxInboxBackToListButton).show();
        $(inboxInboxEmptyTrashButton).hide();
        $(inboxInboxArchiveCompletedButton).hide();
        $(inboxInboxDeleteButton).hide();

        // Hide invitation links
        $("#inbox-invitation-accept").hide();
        $("#inbox-invitation-already").hide();
        $("#inbox-sitejoin-accept").hide();
        $("#inbox-sitejoin-deny").hide();
        $("#inbox-sitejoin-already").hide();

        showPane(inboxPaneMessage);
        var message = getMessageWithId(id);

        if (message["sakai:category"] === "message" || message["sakai:messagebox"] === "trash") {
            $(inboxSpecificMessageDeleteButton).show();
        } else {
            $(inboxSpecificMessageDeleteButton).hide();
        }

        selectedMessage = message;
        if (typeof message !== "undefined") {

            // Fill in this message values.
            $(inboxSpecificMessageSubject).text(sakai.api.Security.saneHTML(message["sakai:subject"]));
            var messageBody = ""+message["sakai:body"]; // coerce to string in case the body is all numbers
            $(inboxSpecificMessageBody).html(sakai.api.Security.saneHTML(messageBody.replace(/\n/gi, "<br />")));
            $(inboxSpecificMessageDate).text(sakai.api.Security.saneHTML(message.date));

            if (message.userFrom) {
                for (var i = 0, j = message.userFrom.length; i < j; i++) {
                    $(inboxSpecificMessageFrom).attr("href", sakai.config.URL.PROFILE_URL + "&id=" + message.userFrom[i].userid);
                    $(inboxSpecificMessageFrom).text(sakai.api.User.getDisplayName(message.userFrom[i]));
                    if (message.userFrom[i].photo) {
                        $(inboxSpecificMessagePicture).attr("src", "/~" + message.userFrom[i]["userid"] + "/public/profile/" + message.userFrom[i].photo);
                    }
                    else {
                        $(inboxSpecificMessagePicture).attr("src", sakai.config.URL.USER_DEFAULT_ICON_URL);
                    }
                }
            } else {
                $(inboxSpecificMessageFrom).text(sakai.api.Security.saneHTML(message["sakai:from"]));
                $(inboxSpecificMessagePicture).attr("src", sakai.config.URL.USER_DEFAULT_ICON_URL);
            }

            // Reply part.
            $(inboxSpecificMessageComposeSubject).val("Re: " + message.subject);

            if (message["sakai:category"] === "invitation"){
                if (message["sakai:subcategory"] === "joinrequest") {
                    $.getJSON(message["sakai:sitepath"] + '/joinrequests/' + message.userFrom[0].hash + '.json', function(data) {
                        var siteJoinRequestIsPending = (data["sakai:requestState"] && data["sakai:requestState"] === "pending");
                        if (siteJoinRequestIsPending) {
                            $("#inbox-sitejoin-accept").show();
                            $("#inbox-sitejoin-deny").show();
                        } else {
                            $("#inbox-sitejoin-already").show();
                        }
                    });
                } else {
                    // Check whether this request is still pending
                    $.ajax({
                        url: "/var/contacts/invited?page=0&items=100",
                        success: function(data) {
                            var pending = false;
                            for (var i = 0; i < data.results.length; i++){
                                if (data.results[i].target === message["sakai:from"]){
                                    // Still a pending invitation
                                    pending = true;
                                }
                            }
                            if (pending){
                                $("#inbox-invitation-accept").show();
                            } else {
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
                    var json = {"message" : message};
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
     * Delete a message
     *
     */


    /**
     * Removes all the messages from memory that are in pathToMessages if success = true
     * success = false will show an error.
     * @param {String[]} pathToMessages
     * @param {Boolean} success
     */
    var deleteMessagesFinished = function(pathToMessages, success) {
        if (success) {

            // Repage the inbox
            currentPage = currentPage + 1;
            messagesForTypeCat--;
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
    var hardDeleteMessage = function(pathToMessages) {
        var requests = [];
        $(pathToMessages).each(function(i,val) {
            var req = {
                "url": val,
                "method": "POST",
                "parameters": {
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
            success: function(data) {
                deleteMessagesFinished(pathToMessages, true);
            },
            error: function(xhr, textStatus, thrownError) {
               deleteMessagesFinished(pathToMessages, false);
            }
        });
    };

    /**
     * Delete all the messages that are in ids
     * @param {Array} pathToMessages    An array of ids that have to be deleted.
     */
    var deleteMessages = function(pathToMessages, hardDelete) {

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

            for (var i = 0, j = allMessages.length; i < j; i++){
                for (var m = 0, n = pathToMessages.length; m < n; m++){
                    if (allMessages[i].id === pathToMessages[m]){
                        if (allMessages[i]["sakai:read"] === "false" && allMessages[i]["sakai:category"]){
                            if (allMessages[i]["sakai:category"] === "message"){
                                deletedUnreadMessages++;
                            } else if (allMessages[i]["sakai:category"] === "invitation"){
                                deletedUnreadInvitations++;
                            } else if (allMessages[i]["sakai:category"] === "announcement"){
                                deletedUnreadAnnouncements++;
                            }
                        }
                    }
                }
            }
            unreadMessages -= deletedUnreadMessages;
            unreadAnnouncements -= deletedUnreadAnnouncements;
            unreadInvitations -= deletedUnreadInvitations;
            updateUnreadNumbers();
            $.bbq.removeState("message");

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
                    error: function(xhr, textStatus, thrownError) {
                        deleted++;
                        if (deleted === toDelete) {
                            deleteMessagesFinished(pathToMessages, false);
                        }
                    },
                    data: {
                        "sakai:messagebox":"trash",
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

    $(inboxComposeMessage).click(function() {
        $.bbq.pushState({"action":"composenew"},2);
    });

    //    This is the widget id!
    $(inboxComposeCancel).live("click", function() {
        //    Jump back to inbox
        $.bbq.pushState({"box":"inbox"},2);
    });

    /**
     *
     * Show a specific message
     *
     */

    $(inboxInboxMessage).live("click", function(e, ui) {
        var id = e.target.id;
        id = id.split('_');
        $.bbq.pushState({"message":id[id.length - 1]},2);
    });

    /* Filter the messages. */

    $(inboxFilterMessages).click(function() {
        $.bbq.pushState({"box": "messages"},2);
    });
    $(inboxFilterAnnouncements).click(function() {
        $.bbq.pushState({"box": "announcements"},2);
    });
    $(inboxFilterChats).click(function() {
        $.bbq.pushState({"box": "chats"},2);
    });
    $(inboxFilterInvitations).click(function() {
        $.bbq.pushState({"box": "invitations"},2);
    });
    $(inboxFilterInbox).click(function() {
        $.bbq.pushState({"box": "inbox"},2);
    });
    $(inboxFilterSent).click(function() {
        $.bbq.pushState({"box": "sent"},2);
    });
    $(inboxFilterTrash).click(function() {
        $.bbq.pushState({"box": "trash"},2);
    });
    $(inboxFilterReminders).click(function(){ // myBerkeley: added reminders category
        $.bbq.pushState({"box": "reminders"},2);
    });
    $(inboxFilterArchive).click(function(){ // myBerkeley: added archive for reminders category
        $.bbq.pushState({"box": "archive"},2);
    });



    // Check all messages
    $(inboxInboxCheckAll).change(function(){
        tickMessages();
    });

    $(inboxInboxDeleteButton).click(function() {
        // Delete all checked messages    
        var pathToMessages = [];
        $(inboxInboxCheckMessage + ":checked").each(function() {
            var pathToMessage = $(this).val();
            pathToMessages.push(pathToMessage);
        });

        // If we are in trash we hard delete the messages
        deleteMessages(pathToMessages, (currentFilter == inboxFilterTrash));
    });    
    
    // MyBerkeley: moving all reminders marked as complete to the archive
    $(inboxInboxArchiveCompletedButton).click(function() {
        var archived = 0;
        
        var propertyToUpdate = {
                "sakai:messagebox": "archive"
        };
        
        for(var i = 0, j = allAllMessages.length; i < j; i++){
            if(allAllMessages[i]["sakai:taskState"] === "completed"){
                archived++;
                
                if(allAllMessages[i]["sakai:read"] === false){
                    unreadReminders -= 1;
                }
                
                updateReminder(allAllMessages[i]["jcr:path"], propertyToUpdate);
            }
        }
        
        if (archived == 0) {
            showGeneralMessage($(inboxGeneralMessagesNoneSelectedReminders).text());
        } else { 
            if (archived == 1) {
                showGeneralMessage(1 + $(inboxGeneralMessagesArchived1).text());
            } else {
                showGeneralMessage(archived + $(inboxGeneralMessagesArchivedX).text());
            }
            
            updateUnreadNumbers();
            showPage(0);
        }
    });
    
    // Emptying all the selected messages in Trash
    $(inboxInboxEmptyTrashButton).click(function(){
        // Delete all checked messages 
        var pathToMessages = [];
        $(inboxInboxCheckMessage + ":checked").each(function(){
            var pathToMessage = $(this).val();
            pathToMessages.push(pathToMessage);
        });
        
        if (pathToMessages.length === 0) {
            showGeneralMessage($(inboxGeneralMessagesNoneSelectedMessages).text());
        } else {
            // If we are in trash we hard delete the messages
            deleteMessages(pathToMessages, true);
            $(inboxInboxCheckAll).attr("checked", '');
            tickMessages();
        }
    });
    
    // Sorters for the inbox.
    $(inboxTableHeaderSort).bind("mouseenter", function() {
        if (sortOrder === 'descending') {
            $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortUp).html()));
        }
        else {
            $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortDown).html()));
        }
    });
    $(inboxTableHeaderSort).bind("mouseout", function() {
        $(inboxTable + " " + inboxArrow).remove();
    });
    $(inboxTableHeaderSort).bind("click", function() {
        sortBy = $(this).attr("id").replace(/inbox_table_header_/gi, "");
        sortOrder = (sortOrder === "descending") ? "ascending" : "descending";

        getAllMessages();
    });


    /**
     *
     * Specific message
     *
     */

    var removeMessageAddBoxStates = function(boxState) {
        $.bbq.removeState("message");
        if (selectedMessage["sakai:messagebox"] === "trash" || boxState === "trash") {
            $.bbq.pushState({"box": "trash"}, 2);
        } else if(selectedMessage["sakai:messagebox"] === "archive" || boxState === "archive") {
            $.bbq.pushState({"box": "archive"}, 2);
        } else if(selectedMessage["sakai:messagebox"] === "sent" || boxState === "sent") {
            $.bbq.pushState({"box": "sent"}, 2);
        } else if(selectedMessage["sakai:messagebox"] === "inbox" || boxState) {
            if(selectedMessage["sakai:category"] === "reminder") {
                $.bbq.pushState({"box": "reminders"}, 2);
            } else {
                $.bbq.pushState({"box": "messages"}, 2);
            }
        }
    }

    $(inboxInboxBackToListButton + ", " + inboxSpecificMessageBackToInbox).click(function() {
        removeMessageAddBoxStates(); // myBerkeley: added to correctly set hash state
        
        // Show the inbox.
        showPane(inboxPaneInbox);

        // Clear all the input fields
        clearInputFields();
    });

    $(inboxSpecificMessageOptionReply).click(function() {
        $(inboxSpecificMessageCompose).show();
        scrollTo($(inboxSpecificMessageCompose));
    });

    $(inboxSpecificMessageDeleteButton + ", " + inboxSpecificMessageOptionDelete).click(function() {
        var harddelete = false;
        var boxState = "message" // myBerkeley: to keep track of what filter to go back to after deletion completes
        
        // myBerkeley: replaced using selectedMessage.types to set harddelete with checking sakai:messagebox instead
        if(selectedMessage["sakai:messagebox"] === "trash"){
            harddelete = true;
            boxState = "trash"; // to keep track of what filter to go back to after deletion completes
        }
        
        removeMessageAddBoxStates(boxState); // myBerkeley: added to correctly set hash state
        
        // Delete the message
        deleteMessages([selectedMessage["jcr:path"]], harddelete);
        
        // Show the inbox
        showPane(inboxPaneInbox);
        
        // Clear all the input fields
        clearInputFields();
    });

    $(window).bind('hashchange', function(e) {
        var box = $.bbq.getState("box");
        var msg = $.bbq.getState("message");
        var action = $.bbq.getState("action");
        if (action) {
            switch(action) {
                case "composenew":
                    showPane(inboxPaneCompose);
                    // initialise the sendmessage widget
                    // we tell it to show it in our id and NOT as a layover.
                    sakai.sendmessage.initialise(null, true, inboxComposeNewContainer, sendMessageFinished);
                    break;
            }
        } else if (msg) {
            displayMessage(msg);
        } else if (box) {
            switch (box) {
                case "inbox":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.inbox, "", "all", inboxFilterInbox);
                    break;
                case "messages":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.message, "all", inboxFilterMessages);
                    $(inboxSubfolderMessages).show();
                    currentFilter = inboxFilterMessages;
                    break;
                case "sent":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.sent, "", "all", inboxFilterSent);
                    $(inboxTableHeaderFromContent).text("To");
                    currentFilter = inboxFilterSent;
                    break;
                case "announcements":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.announcement, "all", inboxFilterAnnouncements);
                    $(inboxSubfolderAnnouncements).show();
                    currentFilter = inboxFilterAnnouncements;
                    break;
                case "chats":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.chat, "all", inboxFilterChats);
                    $(inboxSubfolderChats).show();
                    currentFilter = inboxFilterChats;
                    break;
                case "trash":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.trash, "", "all", inboxFilterTrash);
                    $(inboxInboxDeleteButton).hide();
                    $(inboxInboxEmptyTrashButton).show();
                    currentFilter = inboxFilterTrash;
                    break;
                case "invitations":
                    filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.invitation, "all", inboxFilterInvitations);
                    $(inboxSubfolderClass).hide();
                    $(inboxSubfolderInvitations).show();
                    currentFilter = inboxFilterInvitations;
                    break;
                case "reminders":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.reminder, "all", inboxFilterReminders);
                    $(inboxInboxDeleteButton).hide();
                    $(inboxInboxArchiveCompletedButton).show();
                    $(".different").hide();
                    $(".different_reminders").show();
                    $(inboxSubfolderReminders).show();
                    currentFilter = inboxFilterReminders;
                    break;
                case "archive":
                    $(inboxSubfolderClass).hide();
                    filterMessages(sakai.config.Messages.Types.archive, sakai.config.Messages.Categories.reminder, "all", inboxFilterArchive);
                    $(inboxInboxDeleteButton).hide();
                    $(".different").hide();
                    $(".different_reminders").show();
                    $(inboxSubfolderArchive).show();
                    currentFilter = inboxFilterArchive;
                    break;
            }
        } else { // show the inbox
            $(inboxSubfolderClass).hide();
            filterMessages(sakai.config.Messages.Types.inbox, sakai.config.Messages.Categories.message, "all", inboxFilterMessages);
            $(inboxSubfolderMessages).show();
        }
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
            // We are logged in. Do all the nescecary stuff.
            // load the list of messages.
            var getMsgsReady = false;
            var sendMsgReady = false;
            getAll = true;
            
            getAllMessages(function() {
                getMsgsReady = true;
                if (getMsgsReady && sendMsgReady)
                    $(window).trigger("hashchange");
            });
           
           $(window).trigger("hashchange");
           
            $(window).bind("sakai-sendmessage-ready", function() {
                sendMsgReady = true;
                if (getMsgsReady && sendMsgReady)
                    $(window).trigger("hashchange");
            });
        }
    };


    doInit();
};

sakai.api.Widgets.Container.registerForLoad("sakai.inbox");
