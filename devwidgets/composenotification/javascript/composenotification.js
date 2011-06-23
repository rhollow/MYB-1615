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

/*global $, sdata, opensocial, Config */

require(["jquery", "/dev/lib/myb/jquery/jquery-ui-datepicker.min.js", "sakai/sakai.api.core", "myb/myb.api.core"], function($, $datepick, sakai, myb) {


    sakai_global.composenotification = function(tuid, showSettings) {

        /**
         * Id of a message being edited (or null for new messages)
         */
        var currentMessageId = null;

        /**
         * A message being edited (or null for new messages)
         */
        var currentMessage = null;

         //////////////////////
        // jQuery selectors //
        //////////////////////

        /**
         * Widget's root element
         */
        var $rootElement = $("#" + tuid);








        var user = false; // user object that contains the information for the user that should be posted to
        var me = sakai.data.me;

        var isDirty = false; // Determines whether a notification has changed since it was last created or saved               

        /**
         *
         * NOTIFICATION AUTHORING FORM ELEMENT IDS
         *
         */
        //All required unless otherwise stated.
        var messageFieldRequiredCheck = "#composenotification_required";
        var messageRequiredYes = "#cn-requiredyes";
        var messageRequiredNo = "#cn-requiredno";
        var messageFieldSendDate = "#datepicker-senddate-text";
        var messageFieldTo = "#cn-dynamiclistselect";
        var messageFieldSubject = "#cn-subject";
        var messageFieldBody = "#cn-body";

        // Optional, unless required is checked.
        var messageReminderCheck = "#task-or-event-check";
        var messageReminderCheckbox = "#cn-remindercheck";

        // Optional, unless messageReminderCheck is checked.
        var messageTaskCheck = "#task-radio";
        var messageEventCheck = "#event-radio";

        // Optional, unless messageTaskCheck is checked.
        var messageTaskDueDate = "#datepicker-taskduedate-text";

        // Optional, unless messageEventCheck is checked.
        var messageEventDate = "#datepicker-eventdate-text";
        var messageEventStopDate = "#datepicker-eventstopdate-text";
        var messageEventTimeHour = "#cn-event-timehour";
        var messageEventTimeMinute = "#cn-event-timeminute";
        var messageEventTimeAMPM = "#cn-event-timeampm";
        var messageEventPlace = "#cn-event-place";

        // Invalid class for field validation.   
        var invalidClass = "composenotification_invalid";

        /**
         *
         * EVENT TIME KEY-VALUE PAIRS
         * (used for populating the dropdown menus)
         *
         */
        // Various arrays for event time picking.
        var allHourOptions = {
            '1' : '01',
            '2' : '02',
            '3' : '03',
            '4' : '04',
            '5' : '05',
            '6' : '06',
            '7' : '07',
            '8' : '08',
            '9' : '09',
            '10' : '10',
            '11' : '11',
            '12' : '12'
        };

        var allMinuteOptions = {
            '0' : '00',
            '15' : '15',
            '30' : '30',
            '45' : '45'
        };

        var allAMPMOptions = {
            'AM' : 'AM',
            'PM' : 'PM'
        };

        var formatISO8601 = function(date) {
            var gmtDate = sakai.api.Util.Datetime.toGMT(date);
            return Globalization.format(gmtDate, "yyyyMMddTHHmmssZ");
        };

        /**
         * Shows a general message on the top screen.
         * @param {String} msg The message you want to display.
         * @param {Boolean} isError True for error (red block) or false for normal message(green block).
         */
        var showGeneralMessage = function(msg, isError) {
            // Check whether to show an error type message or an information one.
            var type = isError ? sakai.api.Util.notification.type.ERROR : sakai.api.Util.notification.type.INFORMATION;

            // Show the message to the user.
            sakai.api.Util.notification.show("", msg, type);
        };

        /**
         * Re-enables previously disabled fields.
         */
        var reenableView = function() {
            $(".compose-form-elm").removeAttr("disabled");
            $('input[id|=datepicker]').each(function() {
                var buttonImagePath = $(this).datepicker("option", "buttonImage");
                var buttonImage = $("img[src$='" + buttonImagePath + "']");
                $(buttonImage).show();
            });
            $("#create-new-dynamic-list-button").show();
        };

        /**
         * Disables proper elements of the compose-form-elm class for the
         * notification authoring page. (used to create a non-editable mode)
         */
        var disableView = function() {
            $(".compose-form-elm").attr("disabled", "disabled");
            $('input[id|=datepicker]').each(function() {
                var buttonImagePath = $(this).datepicker("option", "buttonImage");
                var buttonImage = $("img[src$='" + buttonImagePath + "']");
                $(buttonImage).hide();
            });
            $("#create-new-dynamic-list-button").hide();
        };

        /**
         * Removes invalidClass from all elements that are currently within that class.
         */
        var clearInvalids = function() {
            $("." + invalidClass).removeClass(invalidClass);
        };

        /**
         * Resets whatever element it is passed as a parameter, as long as it is of
         * the text, textarea, checkbox, or radiobox type/tag.
         * @param {Object} toClear The element to reset.
         */
        var clearElement = function(toClear) {
            var type = toClear.type;
            var tag = toClear.tagName.toLowerCase();

            if (type === "text" || tag === "textarea") {
                toClear.value = "";
            } else if (type === "checkbox" || type === "radio") {
                toClear.checked = false;
            }
        };

        /**
         * Hide all the button lists (usually called as
         * part of resetting the page).
         */
        var hideAllButtonLists = function() {
            $(".notifdetail-buttons").hide();
        };

        /**
         * Unbind everything to prevent duplicacy issues.
         */
        /*var unbindAll = function() {
            $("#cn-saveasdraft-button").die();
            $("#cn-queue-button").die();
            $("#cn-queuedraft-button").die();
            $("#cn-updatedraft-button").die();
            $("#cn-deletedraft-button").die();
            $("#cn-deletetrashed-button").die();
            $("#cn-movetodrafts-button").die();
            $("#cn-queuecopytodrafts-button").die();
            $("#cn-deletequeued-button").die();
            $("#cn-editrashed-button").die();
            $("#cn-archivecopytodrafts-button").die();
            $("#dlc-save").die();
        };*/

        /**
         * This will reset the whole widget to its default state.
         * It will clear any values or texts that might have been entered.
         * Called every time the widget is initalised.
         */
        var resetView = function() {
            $(".compose-form-elm").each(function() {
                clearElement(this);
            });
            $(".cn-task").hide();
            $(".cn-event").hide();
            $("#must-be-req").hide();
            $("#task-radio").removeAttr("disabled");
            $(".composenotification_taskorevent").hide();
        };

        /**
         *
         * DROPDOWN MENU INITIALISATION FUNCTIONS
         *
         */

        /**
         * Returns a string containing a set of option elements.
         * @param {Object} optionArray Key-value pairs of option elements.
         * @param {String} selectedValue The key of the element option we want to select.
         * @param {Boolean} firstEmpty Start with an option element or not.
         */
        var createOptions = function (optionArray, selectedValue, firstEmpty) {
            var makeOption = function (val, title, selectedTorF) {
                var valString = (val) ? " value='" + val + "'" : "";
                var selectedString = (selectedTorF) ? " selected='selected'" : "";
                // escaping < and > with \x3C and \x3E
                return "\x3Coption " + valString + selectedString + "\x3E" + title + "\x3C/option\x3E\n";
            };
            var optionsString = "";

            if (firstEmpty) {
                // if the first selected value is empty then select the first element in the list
                optionsString += makeOption(null, "", (selectedValue === null || selectedValue === ""));
            }
            for (var key in optionArray) {
                optionsString += makeOption(key, optionArray[key], (selectedValue == key));
            }
            return optionsString;
        };

        /*
         * Sets up drop down menu for Dynamic List field (or otherwise known as
         * the 'Send To' field) based on the array pre-defined earlier in the JS.
         */
        var dynamicListInit = function(selectedID) {
            sakai.api.Server.loadJSON("/~" + me.user.userid + "/private/dynamic_lists.1.json", function(success, data) {

                if (success) {
                    // Iterate through data and make new array for createOptions.
                    var dynamicListOptions = {};
                    for (var element in data) {
                      var optionValue = data[element]["_path"];
                      dynamicListOptions[optionValue] = data[element]["sakai:name"];
                    }

                    // Call createOptions with our new array.
                    var optionsHTML = createOptions(dynamicListOptions, selectedID, true);
                }

                // Clear any old values and then append the new dynamic list options.          
                $(messageFieldTo).empty().append(optionsHTML);
            });
        };

        /*
         * Sets up the timepicker drop down menus for the Event Time fields, 
         * based on the arrays for hour, minutes, and AM/PM as pre-defined earlier in the JS.  
         * Hours are 1-12, minutes are in 15 min intervals.       
         */
        var eventTimeInit = function(hrSelectedID, minSelectedID, AMPMSelectedID) {
            // Filling in the hours dropdown menu.
            var hoursOptionsHTML = createOptions(allHourOptions, hrSelectedID, true);
            $(messageEventTimeHour).empty().append(hoursOptionsHTML);

            // Filling in minutes dropdown menu.                             
            var minuteOptionsHTML = createOptions(allMinuteOptions, minSelectedID, true);
            $(messageEventTimeMinute).empty().append(minuteOptionsHTML);

            // Filling in AM/PM dropdown menu.
            var ampmOptionsHTML = createOptions(allAMPMOptions, AMPMSelectedID, true);
            $(messageEventTimeAMPM).empty().append(ampmOptionsHTML);
        };

        /**
         *
         * DATEPICKER INITIALISATION FUNCTIONS
         *
         */
        $(messageFieldSendDate).datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
            buttonImageOnly: true,
            buttonText: 'Click to pick a date.',
            onSelect: function(dateText, inst) {
                $(messageFieldSendDate).removeClass(invalidClass);
                isDirty = true;
            } // Clearing validation errors
        });
        $(messageTaskDueDate).datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
            buttonImageOnly: true,
            buttonText: 'Click to pick a date.',
            onSelect: function(dateText, inst) {
                $(messageTaskDueDate).removeClass(invalidClass);
                isDirty = true;
            } // Clearing validation errors
        });
        $(messageEventDate).datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
            buttonImageOnly: true,
            buttonText: 'Click to pick a date.',
            onSelect: function(dateText, inst) {
                $(messageEventDate).removeClass(invalidClass);
                isDirty = true;
            } // Clearing validation errors
        });

        // There is no event stop date yet (it will be added later; and when you add it don't forget to set isDirty flag to true when you select something)
        // This code does nothing
        $(messageEventStopDate).datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
            buttonImageOnly: true,
            buttonText: 'Click to pick a date.'
        });

        /**
         *
         * VALIDATION EVENT HANDLERS
         *
         */
        $(messageFieldSendDate).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });
        $(messageFieldSendDate).keypress(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageTaskDueDate).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });
        $(messageTaskDueDate).keypress(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageEventDate).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });
        $(messageEventDate).keypress(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        // There is no event stop date yet (it will be added later; and when you add it don't forget to set isDirty flag to true when you select something)
        // This code was commented out on purpose
        /*
         $(messageEventStopDate).change(function(){
         // Clearing validation errors
         $(this).removeClass(invalidClass);
         });
         $(messageEventStopDate).keypress(function(){
         // Clearing validation errors
         $(this).removeClass(invalidClass);
         });*/

        $(messageFieldSubject).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });
        $(messageFieldSubject).keypress(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageFieldBody).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });
        $(messageFieldBody).keypress(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageEventPlace).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });
        $(messageEventPlace).keypress(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageFieldTo).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageEventTimeHour).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageEventTimeMinute).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });

        $(messageEventTimeAMPM).change(function() {
            // Clearing validation errors
            $(this).removeClass(invalidClass);
            isDirty = true;
        });


        /**
         *
         * EVENT HANDLERS FOR REMINDERS
         *
         */
        // If 'No' is checked for required, it CANNOT be a task.
        // Disable the task radiobox and show the appropriate message.
        $(messageRequiredNo).change(function() {
            $(messageFieldRequiredCheck + " .right").removeClass(invalidClass); // Clearing validation errors
            $(messageRequiredNo).attr("checked", "checked");
            $("#task-radio").attr("disabled", "disabled");
            $("#must-be-req").show();
            isDirty = true;
        });
        // If 'Yes' is checked for required, show the task/event field.
        $(messageRequiredYes).change(function() {
            $(messageFieldRequiredCheck + " .right").removeClass(invalidClass); // Clearing validation errors
            $(messageReminderCheckbox).attr("checked", "checked");
            $(".composenotification_taskorevent").show();
            $("#must-be-req").hide();
            $("#task-radio").removeAttr("disabled");
            isDirty = true;
        });
        // If we check that this message has a date (and is a task or event),
        // reset and show the task/event information fields.
        $(messageReminderCheckbox).change(function() {
            $(messageTaskDueDate).removeClass(invalidClass);
            $(messageEventDate).removeClass(invalidClass);
            $(messageEventTimeHour).removeClass(invalidClass);
            $(messageEventTimeMinute).removeClass(invalidClass);
            $(messageEventTimeAMPM).removeClass(invalidClass);
            $(messageEventPlace).removeClass(invalidClass);
            if ($("#cn-remindercheck").is(":checked")) {
                $(".composenotification_taskorevent").show();
            }
            else {
                $(".cn-task").hide();
                $(".cn-event").hide();
                $(".composenotification_taskorevent").hide();
                $(".composenotification_taskorevent").find(".compose-form-elm").each(function() {
                    clearElement(this);
                });
                eventTimeInit(null, null, null);
            }

            isDirty = true;
        });
        // If task is checked, show the information and make sure the event
        // details are hidden. Restore to their default state.        
        $(messageTaskCheck).change(function() {
            $(messageReminderCheck).removeClass(invalidClass); // Clearing validation errors
            $(".cn-task").show();
            $(".cn-event").hide();
            $(".cn-event").find(".compose-form-elm").each(function() {
                clearElement(this);
            });
            eventTimeInit(null, null, null);
            $(messageEventDate).removeClass(invalidClass);
            $(messageEventTimeHour).removeClass(invalidClass);
            $(messageEventTimeMinute).removeClass(invalidClass);
            $(messageEventTimeAMPM).removeClass(invalidClass);
            $(messageEventPlace).removeClass(invalidClass);
            isDirty = true;
        });
        // If event is checked, show the information and make sure the task
        // details are hidden. Restore to thier default state.     
        $(messageEventCheck).change(function() {
            $(messageReminderCheck).removeClass(invalidClass); // Clearing validation errors
            $(".cn-task").hide();
            $(".cn-event").show();
            $(".cn-task").find(".compose-form-elm").each(function() {
                clearElement(this);
            });
            $(messageTaskDueDate).removeClass(invalidClass);
            isDirty = true;
        });
        // If this event is marked as required, it MUST be a reminder (task or event).                                 
        $(messageReminderCheckbox).change(function() {
            if (!$(messageReminderCheckbox).attr("checked")) {
                if ($(messageRequiredYes).attr("checked")) {
                    $(messageReminderCheckbox).attr("checked", true);
                    $(".composenotification_taskorevent").show();

                    // Dialog overlay window to remind user that required notifications (reminders)
                    // require a date and are either a task or event.
                    $("#required_must_have_date_dialog").jqm({
                        modal: true,
                        overlay: 20,
                        toTop: true,
                        onShow: null
                    });
                    $("#required_must_have_date_dialog").css("position", "absolute");
                    $("#required_must_have_date_dialog").css("top", "250px");
                    $("#required_must_have_date_dialog").jqmShow();
                }
            }
            isDirty = true;
        });

        /**
         *
         * PANEL NAVIGATION FUNCTIONS
         *
         */
        // Return to drafts panel.
        var backToDrafts = function () {
            $.bbq.pushState({l: "notifications/drafts"}, 2);
            //sakai_global.groupnotificationmanager.showDrafts();
        };

        // Return to queue panel.
        var backToQueue = function () {
            $.bbq.pushState({l: "notifications/queue"}, 2);
            //sakai_global.groupnotificationmanager.showQueue();
        };

        // Return to archive panel.
        var backToArchive = function () {
            $.bbq.pushState({l: "notifications/archive"}, 2);
            //sakai_global.groupnotificationmanager.showArchive();
        };

        // Return to trash panel.
        var backToTrash = function() {
            $.bbq.pushState({l: "notifications/trash"}, 2);
            //sakai_global.groupnotificationmanager.showTrash();
        };

        /**
         *
         * CREATE NEW DYNAMIC LIST BUTTON FUNCTIONS
         *
         */
        // For dialog-overlay to remind user to save their draft.
        // (When user clicks on 'Create New Dyanmic List' button.)
        $("#save_reminder_dialog").jqm({
            modal: true,
            overlay: 20,
            toTop: true,
            onShow: null
        });
        $("#save_reminder_dialog").css("position", "absolute");
        $("#save_reminder_dialog").css("top", "250px");

        $("#create-new-dynamic-list-button").live("click", function() {
            if (isDirty) {
                $('#save_reminder_dialog').jqmShow();
            }
            else {
                goToCDNLPage();
            }
            return false;
        });

        // Redirect to the Create New Dynamic Lists page.
        var goToCDNLPage = function() {
            resetView();
            $.bbq.pushState({l: "dynlists"}, 2);
            //window.location = "/dev/listpage.html#tab=existing"; // modified link to reflect bbq state
        };

        /**
         * Fill in the notification detail page with the message's information.
         * @param {Object} message The message whose info we will extract.
         */
        var fillInMessage = function(message, callback) {
            var check = checkEventFields(message);
            var eventDate;
            var taskDate;
            var sendDate;

            // Fill out all the common fields.                    
            if (message["sendDate"] != null) {
                sendDate = sakai.api.Util.parseSakaiDate(message["sendDate"]);
                $(messageFieldSendDate).datepicker("setDate", sendDate);
            }
            dynamicListInit(message["dynamicListID"]);

            // If it's a reminder, fill in the reminder categories after checking if it
            // is a task or an event, and show the proper fields for the user.
            if (message.type === "calendar") {

                // it's a task or event
                $(messageFieldSubject).val(message.calendarWrapper.icalData.SUMMARY);
                $(messageFieldBody).val(message.calendarWrapper.icalData.DESCRIPTION);

                $(messageRequiredYes).attr("checked", true);
                $(messageReminderCheckbox).attr("checked", true);
                $(messageReminderCheck).show();

                // Is it a task or an event?                
                if (message.calendarWrapper.icalData.DUE != null) {
                    // It's a task.
                    $(messageTaskCheck).attr("checked", true);
                    $(".cn-task").show();
                    taskDate = sakai.api.Util.parseSakaiDate(message.calendarWrapper.icalData.DUE);
                    $(messageTaskDueDate).datepicker("setDate", taskDate);
                }
                else if (!check) {
                    // It's an event.                                       
                    $(messageEventCheck).attr("checked", true);
                    $(".cn-event").show();
                    // If the event date was filled out properly, we can extract all the proper date and time information from it.                       
                    if (message.calendarWrapper.icalData.DTSTART != null) {
                        eventDate = sakai.api.Util.parseSakaiDate(message.calendarWrapper.icalData.DTSTART);
                        var hours = eventDate.getHours();
                        var minutes = eventDate.getMinutes();
                        var AMPM = "AM";
                        $(messageEventDate).datepicker("setDate", eventDate);
                        if (hours > 11) {
                            if (hours !== 12) {
                                hours = hours - 12;
                            }
                            AMPM = "PM";
                        }
                        else
                        if (hours === 0) {
                            hours = hours + 12;
                        }
                        eventTimeInit(hours, minutes, AMPM);
                    }
                    // If it was not filled out properly but the time fields were still filled out by the user, we
                    // still need to handle this case and put in the information properly.
                    else if (!check) {
                        $(messageReminderCheckbox).attr("checked", "checked");
                        $(messageReminderCheck).show();
                        $(messageEventCheck).attr("checked", "checked");
                        $(".cn-event").show();

                        var hours = message.uxState["eventHour"];
                        var minutes = message.uxState["eventMin"];
                        var AMPM = message.uxState["eventAMPM"];

                        eventTimeInit(hours, minutes, AMPM);
                    }
                    $(messageEventPlace).val(message.calendarWrapper.icalData.LOCATION);
                }
            }
            else {

                // it's a message

                $(messageFieldSubject).val(message.subject);
                $(messageFieldBody).val(message.body);
                $(messageRequiredNo).attr("checked", "checked");
                $("#task-radio").attr("disabled", "disabled");
                $("#must-be-req").show();

            }
            if ($.isFunction(callback)) {
                callback(true);
            }
        };

        /**
         * This method will check if there are any required fields that are not filled in.
         * If a field is not filled in the invalidClass will be added to that field.
         * Returns a boolean that determines if the fields are valid or not.
         * @param {boolean} displayErrors Do we need to display errors or just do a check?
         * @return valid True = no errors, false = errors found.
         */
        var checkFieldsForErrors = function(displayErrors) {
            var valid = true;
            var today = new Date(); // full today's date
            var month = today.getMonth();
            var day = today.getDate();
            var year = today.getFullYear();
            var modtoday = new Date(year, month, day); // today's date with 00:00:00 time

            // Collect all elements that require validation on the page.
            var requiredEl = $(messageFieldRequiredCheck);
            var sendDateEl = $(messageFieldSendDate);
            var sendToEl = $(messageFieldTo);
            var subjectEl = $(messageFieldSubject);
            var bodyEl = $(messageFieldBody);
            var reminderCheckEl = $(messageReminderCheck);
            var taskDueDateEl = $(messageTaskDueDate);
            var eventDateEl = $(messageEventDate);
            var eventTimeHourEl = $(messageEventTimeHour);
            var eventTimeMinuteEl = $(messageEventTimeMinute);
            var eventTimeAMPMEl = $(messageEventTimeAMPM);
            var eventPlaceEl = $(messageEventPlace);

            // Collect the values of each of the elements that require validation.
            var requiredYes = $(messageRequiredYes).attr("checked");
            var requiredNo = $(messageRequiredNo).attr("checked");
            var sendDate = sendDateEl.val();
            var sendTo = sendToEl.val();
            var subject = subjectEl.val();
            var body = bodyEl.val();
            var reminderCheck = $(messageReminderCheckbox).attr("checked");
            var taskCheck = $(messageTaskCheck).attr("checked");
            var eventCheck = $(messageEventCheck).attr("checked");
            var taskDueDate = taskDueDateEl.val();
            var eventDate = eventDateEl.val();
            var eventTimeHour = eventTimeHourEl.val();
            var eventTimeMinute = eventTimeMinuteEl.val();
            var eventTimeAMPM = eventTimeAMPMEl.val();
            var eventPlace = eventPlaceEl.val();

            var sendDateObj = $(sendDateEl).datepicker("getDate");
            var taskDueDateObj = $(taskDueDateEl).datepicker("getDate");
            var eventDateObj = $(eventDateEl).datepicker("getDate");

            // Remove the invalidClass from each element first.
            if (displayErrors) clearInvalids();

            // Check for invalid values.
            if (!requiredYes && !requiredNo) {
                if (!displayErrors) return false;
                valid = false;
                $(".right", requiredEl).addClass(invalidClass);
            }
            if (!sendDate) {
                if (!displayErrors) return false;
                valid = false;
                sendDateEl.addClass(invalidClass);
            }
            else if (modtoday > sendDateObj) {
                if (!displayErrors) return false;
                valid = false;
                sendDateEl.addClass(invalidClass);
            }
            if (!sendTo) {
                if (!displayErrors) return false;
                valid = false;
                sendToEl.addClass(invalidClass);
            }

            if (!subject) {
                if (!displayErrors) return false;
                valid = false;
                subjectEl.addClass(invalidClass);
            }
            if (!body) {
                if (!displayErrors) return false;
                valid = false;
                bodyEl.addClass(invalidClass);
            }
            if (reminderCheck) {
                // This notification is a REMINDER. Either task or event must be checked.
                if (!taskCheck && !eventCheck) {
                    if (!displayErrors) return false;
                    valid = false;
                    reminderCheckEl.addClass(invalidClass);
                }

                // Now check to see what it is (task or event)...
                else if (taskCheck) {
                    // The reminder is a TASK.
                    if (!taskDueDate) {
                        if (!displayErrors) return false;
                        valid = false;
                        taskDueDateEl.addClass(invalidClass);
                    }
                    else if (modtoday > taskDueDateObj) {
                        if (!displayErrors) return false;
                        valid = false;
                        taskDueDateEl.addClass(invalidClass);
                    }
                    else if (sendDateObj > taskDueDateObj) {
                        if (!displayErrors) return false;
                        valid = false;
                        taskDueDateEl.addClass(invalidClass);
                    }
                }
                else if (eventCheck) {
                    // The reminder is an EVENT.
                    if (!eventDate) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventDateEl.addClass(invalidClass);
                    }
                    else if (modtoday > eventDateObj) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventDateEl.addClass(invalidClass);
                    }
                    else if (sendDateObj > eventDateObj) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventDateEl.addClass(invalidClass);
                    }
                    else {
                        // If the event date is today, check that the time hasn't already passed.
                        if ((eventDateObj.getTime() - modtoday.getTime()) === 0) {
                            if (eventTimeHour && eventTimeMinute && eventTimeAMPM) {
                                var compareToHour = parseInt(eventTimeHour);
                                var compareToMin = parseInt(eventTimeMinute);

                                // Convert to military time.
                                if (eventTimeAMPM == "PM") {
                                    if (compareToHour < 12) {
                                        compareToHour = compareToHour + 12;
                                    }
                                }

                                eventDateObj.setHours(compareToHour);
                                eventDateObj.setMinutes(compareToMin);

                                // If the event is today and the time of the event has already passed...
                                if (today.getTime() > eventDateObj.getTime()) {
                                    if (!displayErrors) return false;
                                    valid = false;
                                    eventTimeHourEl.addClass(invalidClass);
                                    eventTimeMinuteEl.addClass(invalidClass);
                                    eventTimeAMPMEl.addClass(invalidClass);
                                }
                            }
                        }
                    }
                    if (!eventTimeHour) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventTimeHourEl.addClass(invalidClass);
                    }
                    if (!eventTimeMinute) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventTimeMinuteEl.addClass(invalidClass);
                    }
                    if (!eventTimeAMPM) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventTimeAMPMEl.addClass(invalidClass);
                    }
                    if (!eventPlace) {
                        if (!displayErrors) return false;
                        valid = false;
                        eventPlaceEl.addClass(invalidClass);
                    }
                }
            }
            // Even if it isn't a reminder, it could still be a non-required event.
            if (eventCheck) {
                if (!eventDate) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventDateEl.addClass(invalidClass);
                }
                else if (modtoday > eventDateObj) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventDateEl.addClass(invalidClass);
                }
                else if (sendDateObj > eventDateObj) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventDateEl.addClass(invalidClass);
                }
                else {
                    // If the event date is today, check that the time hasn't already passed.
                    if ((eventDateObj.getTime() - modtoday.getTime()) == 0) {
                        if (eventTimeHour && eventTimeMinute && eventTimeAMPM) {
                            var compareToHour = parseInt(eventTimeHour);
                            var compareToMin = parseInt(eventTimeMinute);

                            // Convert to military time.
                            if (eventTimeAMPM == "PM") {
                                if (compareToHour < 12) {
                                    compareToHour = compareToHour + 12;
                                }
                            }

                            eventDateObj.setHours(compareToHour);
                            eventDateObj.setMinutes(compareToMin);

                            // If the event is today and the time of the event has already passed...
                            if (today.getTime() > eventDateObj.getTime()) {
                                if (!displayErrors) return false;
                                valid = false;
                                eventTimeHourEl.addClass(invalidClass);
                                eventTimeMinuteEl.addClass(invalidClass);
                                eventTimeAMPMEl.addClass(invalidClass);
                            }
                        }
                    }
                }
                if (!eventTimeHour) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventTimeHourEl.addClass(invalidClass);
                }
                if (!eventTimeMinute) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventTimeMinuteEl.addClass(invalidClass);
                }
                if (!eventTimeAMPM) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventTimeAMPMEl.addClass(invalidClass);
                }
                if (!eventPlace) {
                    if (!displayErrors) return false;
                    valid = false;
                    eventPlaceEl.addClass(invalidClass);
                }
            }

            // Displays an error message if displayErrors is true and valid is false.
            if (!valid && displayErrors) {
                showGeneralMessage("Please correct invalid fields.");
            }

            // Return the status of the form.
            return valid;
        };

        var checkEventFields = function(message) {
            if ((message.calendarWrapper) && message.calendarWrapper.icalData.LOCATION != null) {
                return false;
            }
            else if (message.uxState["eventMin"] != null) {
                return false;
            }
            else if (message.uxState["eventHour"] != null) {
                return false;
            }
            else if (message.uxState["eventAMPM"] != null) {
                return false;
            }
            return true;
        };

        /**
         * Save the information currently on the notifiaction detail page
         * into a ready-to-post message.
         * @param {Object} box The box to which we are going to save this message to.
         * @param {Object} isValidated Whether or not the message is validated via checkFieldsForErrors().
         * @return {Object} toPost The message we are going to post.
         */
        var saveData = function(box, isValidated) {
            // Filling out all common fields.
            var msgTo = $(messageFieldTo).val() || "";

            var toPost = {
                "dynamicListID": msgTo,
                "senderID" : me.user.userid,
                "sendState": "pending",
                "sakai:messagebox": box,
                "uxState" : {
                    "validated" : isValidated
                },
                calendarWrapper : {
                    uri : null,
                    etag : formatISO8601(new Date()),
                    icalData : {
                        SUMMARY : $(messageFieldSubject).val(),
                        DESCRIPTION : $(messageFieldBody).val()
                    }
                }
            };

            // sendDate is handled a little differently, because it is called
            // by a sakai date parsing util which throws an error if not
            // the proper Date object type.
            var sendDate = $(messageFieldSendDate).datepicker("getDate");
            if (sendDate === null) {
                toPost["sendDate"] = null;
            }
            else {
                toPost["sendDate"] = formatISO8601(sendDate);
            }


            var type = $("#cn-notification-type", $rootElement).val();

            switch(type) {
                case "task":

                    toPost["type"] = "calendar";

                    // Tasks are considered always required
                    toPost.calendarWrapper.icalData.CATEGORIES = ["MyBerkeley-Required"];

                    toPost.calendarWrapper.component = "VTODO";
                    toPost.calendarWrapper.icalData.STATUS = "NEEDS-ACTION";
                    if ($(messageTaskDueDate).val() !== "") {
                        toPost.calendarWrapper.icalData.DUE = formatISO8601($(messageTaskDueDate).datepicker("getDate"));
                        toPost.calendarWrapper.icalData.DTSTART = formatISO8601($(messageTaskDueDate).datepicker("getDate"));
                    }

                    break;

                case "event":

                    toPost["type"] = "calendar";
                    if ($(messageRequiredYes).attr("checked")) {
                        toPost.calendarWrapper.icalData.CATEGORIES = ["MyBerkeley-Required"];
                    } else {
                        toPost.calendarWrapper.icalData.CATEGORIES = [];
                    }

                    toPost.calendarWrapper.component = "VEVENT";
                    // If the event date is filled out, we handle this normally and save the time information there.
                    if ($(messageEventDate).datepicker("getDate") != null) {
                        var startDate = $(messageEventDate).datepicker("getDate");
                        startDate.setMinutes($(messageEventTimeMinute).val());
                        // Get the event time details and add to the eventDate obj.
                        if ($(messageEventTimeAMPM).val() == "PM" && parseInt($(messageEventTimeHour).val()) != 12) {
                            startDate.setHours(parseInt($(messageEventTimeHour).val()) + 12);
                        }
                        else {
                            startDate.setHours($(messageEventTimeHour).val());
                        }
                        toPost.calendarWrapper.icalData.DTSTART = formatISO8601(startDate);
                    }
                    // Otherwise, we need to check for and save the time information in case the user filled this out.
                    else {
                        toPost.uxState["eventMin"] = $(messageEventTimeMinute).val();
                        toPost.uxState["eventAMPM"] = $(messageEventTimeAMPM).val();
                        toPost.uxState["eventHour"] = $(messageEventTimeHour).val();
                    }
                    toPost.calendarWrapper.icalData.LOCATION = $(messageEventPlace).val();


                    break;

                case "message":
                    toPost["type"] = "message";
                    toPost["subject"] = $(messageFieldSubject).val();
                    toPost["body"] = $(messageFieldBody).val();
                    delete(toPost.calendarWrapper); // messages don't have calendar data

                default:

                    break;
            }



            //-------------------------------------------------------------------------------


            // This notification is a REMINDER--required task or event.              
            /*if ($(messageRequiredYes).attr("checked")) {
                // Fields common to reminders.
                toPost["category"] = "reminder";
                toPost.calendarWrapper.icalData.CATEGORIES = ["MyBerkeley-Required"];

                // This reminder is a TASK.
                if ($(messageTaskCheck).attr("checked")) {
                    toPost.calendarWrapper.component = "VTODO";
                    toPost.calendarWrapper.icalData.STATUS = "NEEDS-ACTION";
                    if ($(messageTaskDueDate).val() != "") {
                        toPost.calendarWrapper.icalData.DUE = formatISO8601($(messageTaskDueDate).datepicker("getDate"));
                        toPost.calendarWrapper.icalData.DTSTART = formatISO8601($(messageTaskDueDate).datepicker("getDate"));
                    }
                }

                // This reminder is an EVENT.
                else {
                    toPost.calendarWrapper.component = "VEVENT";
                    // If the event date is filled out, we handle this normally and save the time information there. 
                    if ($(messageEventDate).datepicker("getDate") != null) {
                        var startDate = $(messageEventDate).datepicker("getDate");
                        startDate.setMinutes($(messageEventTimeMinute).val());
                        // Get the event time details and add to the eventDate obj.                    
                        if ($(messageEventTimeAMPM).val() == "PM" && parseInt($(messageEventTimeHour).val()) != 12) {
                            startDate.setHours(parseInt($(messageEventTimeHour).val()) + 12);
                        }
                        else {
                            startDate.setHours($(messageEventTimeHour).val());
                        }
                        toPost.calendarWrapper.icalData.DTSTART = formatISO8601(startDate);
                    }
                    // Otherwise, we need to check for and save the time information in case the user filled this out.
                    else {
                        toPost.uxState["eventMin"] = $(messageEventTimeMinute).val();
                        toPost.uxState["eventAMPM"] = $(messageEventTimeAMPM).val();
                        toPost.uxState["eventHour"] = $(messageEventTimeHour).val();
                    }
                    toPost.calendarWrapper.icalData.LOCATION = $(messageEventPlace).val();
                }
            }

            // This is a general NOTIFICATION--nonrequired message or event.
            else {
                // Common field for all notifications.                     
                toPost["category"] = "message";
                toPost.calendarWrapper.icalData.CATEGORIES = [];

                // This is a NON-REQUIRED EVENT.
                if ($(messageEventCheck).attr("checked")) {
                    toPost.calendarWrapper.component = "VEVENT";
                    // If the event date is filled out, we handle this normally and save the time information there.                                                                  
                    if ($(messageEventDate).datepicker("getDate") != null) {
                        var startDate = $(messageEventDate).datepicker("getDate");
                        startDate.setMinutes($(messageEventTimeMinute).val());
                        // Get the event time details and add to the eventDate obj.                    
                        if ($(messageEventTimeAMPM).val() == "PM" && parseInt($(messageEventTimeHour).val()) != 12) {
                            startDate.setHours(parseInt($(messageEventTimeHour).val()) + 12);
                        }
                        else {
                            startDate.setHours($(messageEventTimeHour).val());
                        }
                        toPost.calendarWrapper.icalData.DTSTART = formatISO8601(startDate);
                    }
                    // Otherwise, we need to check for and save the time information in case the user filled this out.
                    else {
                        toPost.uxState["eventMin"] = $(messageEventTimeMinute).val();
                        toPost.uxState["eventAMPM"] = $(messageEventTimeAMPM).val();
                        toPost.uxState["eventHour"] = $(messageEventTimeHour).val();
                    }
                    toPost.calendarWrapper.icalData.LOCATION = $(messageEventPlace).val();
                }
            } */
            return toPost;
        };

        /**
         * Post the notification's data via an Ajax call, either to create a new
         * notification or to update an existing one. The only difference between the
         * two will be the URL to which we make the post, and we can determine which
         * case we are handling via whether or not the optional original parameter
         * is null (and therefore has a valid id) or not.
         * @param {Object} toPost Message to post; usually created via saveData() function.
         * @param {Object} successCallback (optional) Function to call if successful post; usually redirect function.
         * @param {Object} original (optional) The original message, if this is an update.
         * @param {boolean} copyCheck (optional) Are we copying? If we are, we need to append "Copy to" to the subject.
         * @param {Object} String to be used in the popup message text indicating success or failure.
         */
        var postNotification = function (toPost, successCallback, original, copyCheck, msgTxt) {
            var url = "/user/" + me.user.userid + "/.myb-notificationstore.html";

            // Are we creating a copy of an existing notification?
            if (copyCheck) {
                if ( toPost.type === "message") {
                    toPost.subject = "Copy of " + toPost.subject;
                } else {
                    toPost.calendarWrapper.icalData.SUMMARY = "Copy of " + toPost.calendarWrapper.icalData.SUMMARY;
                }

            }

            // Are we modifying an existing notification?
            if (original !== null) {
                toPost.id = original.id;

            }

            // Post all the data in an Ajax call.
            $.ajax({
                url: url,
                type: "POST",
                data: { notification : $.toJSON(toPost) },
                success: function() {
                    if (msgTxt != null) {
                        showGeneralMessage(msgTxt + " successful.");
                    }
                    // If a callback function is specified in argument, call it.
                    if ($.isFunction(successCallback)) {
                        successCallback(true);
                    }
                },
                error: function(data) {
                    if (msgTxt != null) {
                        showGeneralMessage(msgTxt + " failed.");
                    }
                    else {
                        showGeneralMessage("An error occurred.");
                    }
                }
            });
        };

        /**
         * This is the method that can be called from other widgets or pages.
         * @param {Object} userObj The user object containing the necessary information.
         * @param {Object} callback When the message is sent this function will be called. If no callback is provided a standard message will be shown that fades out.
         * @param {Object} calledFrom What pane we called this widget from so we know what mode to put it in. (default: null)
         * @param {Object} message Message data for if we are pre-filling with information. (default: null)
         */
        var loadNotification = function(calledFrom, messageId) {

            currentMessageId = messageId;

            var message = null;
            var url = "/~" + sakai.data.me.user.userid + "/_myberkeley_notificationstore/" + messageId + ".json";

            $.ajax({
                url: url,
                cache: false,
                async: false,
                success: function(data){
                    message = data;
                    message.calendarWrapper = $.parseJSON(message.calendarWrapper);
                    message.uxState = $.parseJSON(message.uxState);

                },
                error: function(xhr, textStatus, thrownError){
                    showGeneralMessage($(inboxGeneralMessagesErrorGeneral).text());
                    // Commented out because this displays the tiny bit of text at the bottom of the panel that
                    // says an error has occurred. Felt this was a bit redundant to have both the text and a
                    // popup alerting the user that there was an error, and also, the error message does not
                    // ever disappear for the remainder of the time the user spends on this page (unless they
                    // refresh or leave and come back).
                    //$(inboxResults).html(sakai.api.Security.saneHTML($(inboxGeneralMessagesErrorGeneral).text()));
                }
            });

            //TODO: fix this later, we need only one variable for this
            currentMessage = message;




            // Reset page back to its original condition.
            resetView();
            clearInvalids();
            hideAllButtonLists();
            //unbindAll();
            eventTimeInit(null, null, null);
            isDirty = false;

            // There are 2 calls to DLinit when we initialise the widget:
            // 1) to set up the dropdown with its options
            // 2) to select an option if there is an existing message
            // To prevent concurrency issues between these 2 calls, check if we are
            // NOT creating a new notification, and if this is indeed the case,
            // then initialise a blank dropdown. Otherwise, fillInMessage should handle it.
            if (message == null || message["dynamicListID"] == null) {
                dynamicListInit(null);
            }

            // Chose notification type
            var type = message.type;
            if ( type === "calendar" ) {
                type = message.calendarWrapper.component;
            }

            switch(type) {
                case "VTODO":
                    $("#cn-notification-type", $rootElement).val("task");
                    notificationTypeInit("task");
                    break;
                case "VEVENT":
                    $("#cn-notification-type", $rootElement).val("event");
                    notificationTypeInit("event");
                    break;
                case "message":
                    $("#cn-notification-type", $rootElement).val("message");
                    notificationTypeInit("message");
                    break;
                default:
                    break;
            }




             // Hide all the buttons
             hideAllButtonLists();

            // Are we calling this from drafts?
            if (calledFrom == "drafts") {
                // Re-enable all buttons and textboxes in case they were disabled during viewing of Queue or Trash notifications
                reenableView();

                $("#cn-widget-title").text("Edit Notification");

                // Fill out the proper information.
                fillInMessage(message, checkFieldsForErrors(false));

                // Show the proper button list
                $("#editdraft-buttons").show();


            }

            // Are we calling this from queue?
            if (calledFrom == "queue") {

                $("#cn-widget-title").text("View Notification");

                // Disable the form, disallowing the user to edit.
                disableView();

                // Now fill out the proper information.
                fillInMessage(message, null);

                // Display the proper buttons.                
                $("#queueview-buttons").show();

            }

            // Are we calling this from acrhive?
            if (calledFrom == "archive") {

                $("#cn-widget-title").text("View Notification");

                // Disable the form, disallowing the user to edit.
                disableView();

                // Now fill out the proper information.
                fillInMessage(message, null);

                // Display the proper buttons.                
                $("#archiveview-buttons").show();

                // Copying message to drafts...
                $("#cn-archivecopytodrafts-button").live('click', function() {
                    postNotification(saveData("drafts", true), backToArchive, null, true, "Copy");
                });
            }

            // Are we calling this from trash?       
            if (calledFrom == "trash") {

                $("#cn-widget-title").text("View Notification");

                // Now fill out the proper information.
                fillInMessage(message, null);

                // And disable the form, disallowing the user to edit.
                disableView();

                // Display the proper buttons.                
                $("#trashview-buttons").show();


            }

            // Else, user is creating a brand new blank notification.
            if (calledFrom == null) {

                $("#cn-widget-title").text("Create Notification");

                // Re-enable all buttons and textboxes in case they were disabled during viewing of Queue or Trash notifications
                reenableView();

                // Show the proper button list
                $("#createnew-buttons").show();

            }
        };


        // Event handler for when user clicks on DLC "Save" button.
        $("#dlc-save").live('click', function() {
            // Check that the subject field isn't empty before saving
            if ($(messageFieldSubject).val() != "") {
                // Save the draft.
                postNotification(saveData("drafts", checkFieldsForErrors(false)), goToCDNLPage, currentMessage, null, null);
            } else {
                // If subject field is empty, cancel jqm dialog and highlight subject field.
                $("#save_reminder_dialog").jqmHide();
                $(messageFieldSubject).addClass(invalidClass);
            }
        });

        // Event handler for when you click on the "Don't Save" button on DLC dialog.
        $("#dlc-dontsave").live('click', function() {
            // Hide jqm dialog before moving, so that clicking Back button on browser doesn't take you
            // back to this page with the dialog box still open
            $("#save_reminder_dialog").jqmHide();
            goToCDNLPage();
        });


        // Queueing this draft...
        $("#cn-queuedraft-button").live('click', function() {
            if (checkFieldsForErrors(true)) {
                postNotification(saveData("queue", true), backToDrafts, currentMessage, null, "Queue");
            }
        });

        // Updating and re-saving this draft...
        $("#cn-updatedraft-button").live('click', function() {
            postNotification(saveData("drafts", checkFieldsForErrors(false)), backToDrafts, currentMessage, null, "Save");
        });

        // Deleting the draft...
        $("#cn-deletedraft-button").live('click', function() {
            postNotification(saveData("trash", false), backToDrafts, currentMessage, null, "Delete");
        });

         // When someone clicks on the 'Queue' button from base panel.
        $("#cn-queue-button").live('click', function() {
            if (checkFieldsForErrors(true)) {
                postNotification(saveData("queue", true), backToDrafts, null, null, "Queue");
            }
        });
        // When someone clicks on the 'Save as Draft' button from base panel.
        $("#cn-saveasdraft-button").live('click', function() {
            if ($(messageFieldSubject).val() === "") {
                $(messageFieldSubject).addClass(invalidClass);
            } else {
                postNotification(saveData("drafts", checkFieldsForErrors(false)), backToDrafts, null, null, "Save");
            }
        });

        $("#cn-cancel-button").live("click", function(){
             $.bbq.removeState(['new', 'edit']);
            //return false;
        });

        // Moving message from queue to drafts...
        $("#cn-movetodrafts-button").live('click', function() {
            postNotification(saveData("drafts", true), backToQueue, currentMessage, null, "Move");
        });

        // Copying message to drafts...
        $("#cn-queuecopytodrafts-button").live('click', function() {
            postNotification(saveData("drafts", true), backToQueue, null, true, "Copy");
        });

        // Deleting message...
        $("#cn-deletequeued-button").live('click', function() {
            postNotification(saveData("trash", false), backToQueue, currentMessage, null, "Delete");
        });

        // Enable editing of message (move it to drafts and re-initialise widget).
        $("#cn-editrashed-button").live('click', function() {
            postNotification(saveData("drafts", checkFieldsForErrors(false)), loadNotification("drafts", currentMessageId), currentMessage, false, null);
        });

        // Hard delete this message (delete it from the trash).
        $("#cn-deletetrashed-button").live('click', function() {
            var requests = [];
            var msgUrl = "/~" + sakai.data.me.user.userid + "/_myberkeley_notificationstore/" + currentMessageId;

            var toDelete = {
                "url": msgUrl,
                "method": "POST",
                "parameters": {
                    ":operation": "delete"
                }
            };
            requests.push(toDelete);
            $.ajax({
                url: sakai.config.URL.BATCH,
                traditional: true,
                type: "POST",
                data: {
                    requests: $.toJSON(requests)
                },
                success: function(data) {
                    showGeneralMessage("Delete successful.");
                    backToTrash();
                },
                error: function(xhr, textStatus, thrownError) {
                    showGeneralMessage("Delete failed.");
                }
            });
        });



        var notificationTypeInit = function (type) {
            if ( type === "message" ) {
                $("#composenotification_event_date").hide();
                $("#composenotification_event_time").hide();
                $("#composenotification_event_place").hide();
                $("#composenotification_task_due_date").hide();
                $("#cn-requiredno", $rootElement).attr("checked", "checked");
                $("#composenotification_required .right input", $rootElement).attr("disabled", "disabled");
                $("#composenotification_required", $rootElement).hide();
            } else if (type === "task") {
                $("#composenotification_event_date").hide();
                $("#composenotification_event_time").hide();
                $("#composenotification_event_place").hide();

                $("#composenotification_task_due_date").show();

                $("#cn-requiredyes", $rootElement).attr("checked", "checked");
                $("#composenotification_required", $rootElement).show();
                $("#composenotification_required .right input", $rootElement).attr("disabled", "disabled");
            } else {
                $("#composenotification_event_date").show();
                $("#composenotification_event_time").show();
                $("#composenotification_event_place").show();

                $("#composenotification_task_due_date").hide();

                $("#cn-requiredyes", $rootElement).removeAttr("checked");
                $("#cn-requiredno", $rootElement).removeAttr("checked");
                $("#composenotification_required", $rootElement).show();
                $("#composenotification_required .right input", $rootElement).removeAttr("disabled");
            }
        };





        $("#cn-notification-type", $rootElement).change(function()
        {
            var value = $(this).attr('value');
            notificationTypeInit(value);
        });


        /**
         * Resets the editing form UI
         */
        var resetForm = function() {

            $(".compose-form-elm", $rootElement).each(function() {
                clearElement(this);
            });

            $("#cn-notification-type", $rootElement).val("task");


            dynamicListInit(null);
            eventTimeInit(null, null, null);
            /*$("#datepicker-taskduedate-text", $rootElement).val("");
            $("#datepicker-eventdate-text", $rootElement).val("");
            $("#cn-event-place", $rootElement).val("");
            $("#cn-consequence", $rootElement).val("");
            $("#datepicker-senddate-text", $rootElement).val("");
            $("#cn-subject", $rootElement).val("");
            $("#cn-body", $rootElement).val("");
            $("#cn-email", $rootElement).val("");
            $("#cn-link", $rootElement).val("");
             */

            reenableView();
            // Must be called after reenableView because it disables 'Required?' radio buttons
            notificationTypeInit("task");

            isDirty = false;


        };


        /**
         * Sets current state of this component (list mode or edit mode).
         * This function is called when hashchange event fires.
         */
         var setState = function(){

            hideAllButtonLists();
            var state = $.bbq.getState();

            if(state.hasOwnProperty("new")) {
                resetForm();
                //TODO: refactor this
                $("#cn-widget-title").text("Create Notification");
                $("#createnew-buttons").show();
                $rootElement.show();
            } else if(state.hasOwnProperty("edit")) {
                resetForm();

                var box, msgId;

                if(state.hasOwnProperty("l")) {
                    box = state.l;
                }

                if(state.hasOwnProperty("edit")) {
                    msgId = state.edit;
                }

                if (box === "notifications/drafts") {
                    loadNotification("drafts", msgId);
                } else if(box === "notifications/queue") {
                    loadNotification("queue", msgId);
                } else if(box === "notifications/archive") {
                    loadNotification("archive", msgId);
                } else if(box === "notifications/trash") {
                    loadNotification("trash", msgId);
                }

                $rootElement.show();

            } else {
                $rootElement.hide();
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

            resetForm();

            // TODO: HACK: To prevent flickering this widget was made invisible in HTML code, need to undo this
            $("div.composenotification_widget", $rootElement).show();

            setState();


        };

        doInit();


    };

    debug.info("Asking for load of composenotification widget");
    sakai.api.Widgets.widgetLoader.informOnLoad("composenotification");

});
