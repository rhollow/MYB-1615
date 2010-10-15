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

var sakai = sakai || {};
if (!sakai.composenotification){

    sakai.composenotification = function(tuid, showSettings) {

        var rootel = $("#"+tuid);        

        var user = false; // user object that contains the information for the user that should be posted to
        var me = sakai.data.me;        
        var callbackWhenDone = null; // callback function for when the message gets sent       

        /**
         * 
         * GENERAL CSS IDS
         * 
         */     
        var notificationBox = '#composenotification_box';
        var messageDone = "#message_done";
        var messageForm = "#message_form";       

        /**
         * 
         * NOTIFICATION AUTHORING FORM ELEMENT IDS.
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
        var messageEventTimeHour = "#cn-event-timehour";
        var messageEventTimeMinute = "#cn-event-timeminute";
        var messageEventTimeAMPM = "#cn-event-timeampm";        
        var messageEventPlace = "#cn-event-place";
                
        var buttonSendMessage = "#send_message";

        var invalidClass = "composenotification_invalid";
        var errorClass = "composenotification_error_message";
        var normalClass = "composenotification_normal_message";        

        var messageOK = "#sendmessage_message_ok";
        var messageError = "#sendmessage_message_error";
        var messageErrorFriends = "#sendmessage_friends_error"; 
        
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
        
         /**
          * Returns a string containing a set of option elements 
          * @param {Object} optionArray name:value pairs of option elements
          * @param {String} selectedValue the key of the selected option element
          * @param {Boolean} firstEmpty start with an option element or not
          */        
        var createOptions = function (optionArray, selectedValue, firstEmpty) {                  
            var makeOption = function (val, title, selectedTorF) {
                var valString = (val) ? " value='" + val + "'" : "";
                var selectedString = (selectedTorF) ? " selected='selected'" : "";
                return "<option " + valString + selectedString + ">" + title + "</option>\n";
            }
            
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

        /**
         * Re-enables previously disabled fields. 
         */
        var reenableView = function() {           
            $(".compose-form-elm").removeAttr("disabled"); 
            $('input[id|=datepicker]').each(function() {
                var buttonImagePath = $(this).datepicker( "option", "buttonImage" );
                var buttonImage = $("img[src$='"+buttonImagePath+"']");                
                $(buttonImage).show();
            });            
        };
        
        /**
         * Disables proper elements of the compose-form-elm class for the
         * notification authoring page.
         */
        var disableView = function() {            
            $(".compose-form-elm").attr("disabled","disabled");                          
            $('input[id|=datepicker]').each(function() {
                var buttonImagePath = $(this).datepicker( "option", "buttonImage" );
                var buttonImage = $("img[src$='"+buttonImagePath+"']");                
                $(buttonImage).hide();
            });                          
        };
        
        /**
         * Removes invalidClass from all elements that are currently within that class.
         */
        var clearInvalids = function(){            
            $("."+invalidClass).removeClass(invalidClass);
        };
        
        /**
         * Resets whatever element it is passed as a parameter, as long as it is of
         * the text, textarea, checkbox, radiobox, or select dropdown menu type/tag.
         * @param {Object} toClear The element to reset.
         */
        var clearElement = function(toClear) {            
            var type = toClear.type;
            var tag = toClear.tagName.toLowerCase();
            
            if (type === "text" || tag === "textarea") {                
                toClear.value="";
            } else if (type === "checkbox" || type === "radio") {                
                toClear.checked=false;
            } 
        };

        /**
         * This will reset the whole widget to its default state.
         * It will clear any values or texts that might have been entered.
         * Called every time the widget is initalised.
         */
        var resetView = function() {                                                                                          
            $(".compose-form-elm").each(function(){
                clearElement(this);
            });           
                         
            $(".cn-task").hide();
            $(".cn-event").hide();            
            $(".composenotification_taskorevent").hide();
        };
        
        /**
         * 
         * EVENT HANDLERS
         * 
         */
        // Reminder criterion functions.
        $(messageReminderCheckbox).change(function() {
            $(messageTaskDueDate).removeClass(invalidClass); 
            $(messageEventDate).removeClass(invalidClass);
            $(messageEventTimeHour).removeClass(invalidClass);
            $(messageEventTimeMinute).removeClass(invalidClass);
            $(messageEventTimeAMPM).removeClass(invalidClass);
            $(messageEventPlace).removeClass(invalidClass);
            
            if($("#cn-remindercheck").is(":checked")){
                $(".composenotification_taskorevent").show();    
            }
            else{
                $(".cn-task").hide();
                $(".cn-event").hide();
                $(".composenotification_taskorevent").hide();
                $(".composenotification_taskorevent").find(".compose-form-elm").each(function() {
                    clearElement(this);    
                });                
            };
        });                 
            
        // Task and event detail functions.        
        $(messageTaskCheck).change(function() {           
            $(".cn-task").show();
            $(".cn-event").hide();
            $(".cn-event").find(".compose-form-elm").each(function(){                
                clearElement(this);
            });                      
            $(messageEventDate).removeClass(invalidClass);
            $(messageEventTimeHour).removeClass(invalidClass);
            $(messageEventTimeMinute).removeClass(invalidClass);
            $(messageEventTimeAMPM).removeClass(invalidClass);
            $(messageEventPlace).removeClass(invalidClass);
        });          
        $(messageEventCheck).change(function() {          
            $(".cn-task").hide();        
            $(".cn-event").show();
            $(".cn-task").find(".compose-form-elm").each(function(){             
                clearElement(this);
            });            
            $(messageTaskDueDate).removeClass(invalidClass);                                                                                                         
        });     
        
        // Datepicker functions.
        $("#datepicker-senddate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.'      
        });     
        $("#datepicker-taskduedate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.'     
        });     
        $("#datepicker-eventdate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.'        
        });     
        $("#datepicker-eventstopdate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.'        
        });                                           
                    
        // For dialog-overlay to remind user to save their draft.
        // (When user clicks on 'Create New Dyanmic List' button.)
        $("#save_reminder_dialog").jqm({
             modal: true,
             trigger: $("#create-new-dynamic-list-button"),
             overlay: 20,
             toTop: true,
             onShow: null           
         }); 
         $("#save_reminder_dialog").css("position","absolute");
         $("#save_reminder_dialog").css("top", "250px"); 
         
         var goToCDNLPage = function(){
             resetView();                         
             window.location = "/dev/listpage.html";              
         }
         
         // Event handler for when user clicks on DLC "Save" button.
         $("#dlc-save").live('click', function(){
             // Save the draft.
             postNotification(saveData("drafts", checkFieldsForErrors(false)), goToCDNLPage);                                     
         });
         
         // Event handler for when you click on the "Don't Save" button on DLC dialog.
         $("#dlc-dontsave").live('click', goToCDNLPage);
         
         // If 'Yes' is checked for required, show the task/event field.
         $(messageRequiredYes).change(function(){                         
            $(messageReminderCheckbox).attr("checked", "checked");                                    
            $(".composenotification_taskorevent").show();                                                   
         });                    
         
         // If this event is marked as required, it MUST be a reminder (task or event).                                 
         $(messageReminderCheckbox).change(function() {            
           if(!$(messageReminderCheckbox).attr("checked")){
               if($(messageRequiredYes).attr("checked")){                   
                   $(messageReminderCheckbox).attr("checked",true);
                   $(".composenotification_taskorevent").show();   
                   
                   // Dialog overlay window to remind user that required notifications (reminders)
                   // require a date and are either a task or event.
                   $("#required_must_have_date_dialog").jqm({
                     modal: true,                     
                     overlay: 20,
                     toTop: true,
                     onShow: null           
                 }); 
                 $("#required_must_have_date_dialog").css("position","absolute");
                 $("#required_must_have_date_dialog").css("top", "250px");
                 $("#required_must_have_date_dialog").jqmShow();                                 
               }
           } 
        });                        

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
		    if(!requiredYes && !requiredNo){             
		        if (!displayErrors) return false;
				valid = false;
				requiredEl.addClass(invalidClass);	  
		    }           
		    if(!sendDate) {
		        if (!displayErrors) return false;
				valid = false;
	        	sendDateEl.addClass(invalidClass);
		    }                                    
		    else if(modtoday>sendDateObj){                                              
		        if (!displayErrors) return false;
				valid = false;
	        	sendDateEl.addClass(invalidClass);		         
		    }
		    if(!sendTo){
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
		    if(reminderCheck){
		        // This notification is a REMINDER. Either task or event must be checked.                
		        if(!taskCheck && !eventCheck){
		            if (!displayErrors) return false;
					valid = false;
	            	reminderCheckEl.addClass(invalidClass);             
		        }
		        else if (taskCheck) {
		            // The reminder is a TASK.                    
		            if (!taskDueDate) {
		                if (!displayErrors) return false;
						valid = false;
	                	taskDueDateEl.addClass(invalidClass);
		            }
		            else if(modtoday>taskDueDateObj) {                                              
		                if (!displayErrors) return false;
						valid = false;
		                taskDueDateEl.addClass(invalidClass);
		            }             
		            else if(sendDateObj>taskDueDateObj) {
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
		            else if(modtoday>eventDateObj){                                                    
		                if (!displayErrors) return false;
						valid = false;
	                	eventDateEl.addClass(invalidClass);            
		            }
		            else if(sendDateObj>eventDateObj){
		                if (!displayErrors) return false;
						valid = false;
	                	eventDateEl.addClass(invalidClass);
		            }
		            else{
		                // If the event date is today, check that the time hasn't already passed.                                        
		                if((eventDateObj.getTime()-modtoday.getTime())==0){                            
		                    if(eventTimeHour && eventTimeMinute && eventTimeAMPM){                                                    
		                        var compareToHour = parseInt(eventTimeHour);                             
		                        var compareToMin = parseInt(eventTimeMinute);                             
		                        
		                        // Convert to military time.
		                        if(eventTimeAMPM=="PM"){
		                            if(compareToHour<12){
		                                compareToHour = compareToHour+12;
		                            }                                
		                        }                                                                                   
		                                                    
		                        eventDateObj.setHours(compareToHour);
		                        eventDateObj.setMinutes(compareToMin);                                                        
		                        
		                        // If the event is today and the time of the event has already passed...                  
		                        if(today.getTime()>eventDateObj.getTime()){
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
		    
		    // Return the status of the form.        
		    return valid;
		};            
        
        /**
         * Sets up the timepicker drop down menus for the Event Time fields, 
         * based on the arrays for hour, minutes, and AM/PM as pre-defined earlier in the JS.  
         * Hours are 1-12, minutes are in 15 min intervals.       
         */          
        var eventTimeInit = function(){              
            // Filling in the hours dropdown menu.
            var hoursOptionsObj = createOptions(allHourOptions, null, true);
            $(messageEventTimeHour).empty().append(hoursOptionsObj);            
            
            // Filling in minutes dropdown menu.                             
            var minuteOptionsObj = createOptions(allMinuteOptions, null, true);
            $(messageEventTimeMinute).empty().append(minuteOptionsObj);           
            
            // Filling in AM/PM dropdown menu.
            var ampmOptionsObj = createOptions(allAMPMOptions, null, true);
            $(messageEventTimeAMPM).empty().append(ampmOptionsObj);                                                                         
        };
        
        /**
         * Sets up drop down menu for Dynamic List field (or otherwise known as
         * the 'Send To' field) based on the array pre-defined earlier in the JS.
         */
        var dynamicListInit = function(){
           //alert("called dynamiclistinit");
            sakai.api.Server.loadJSON("/~" + sakai.data.me.user.userid + "/private/dynamic_lists", function(success, data){
                if (success) {
                    // Iterate through data.lists and make new array for createOptions. (id:name)
                    var dynamicListArray = [];
                                        
                    for (var i = 0; i < data.lists.length; i++) {                        
                        dynamicListArray[data.lists[i]["sakai:id"]] = data.lists[i]["sakai:name"];                        
                    }
                    // Call createOptions with our new array.
                    var optionsObj = createOptions(dynamicListArray, null, true);
                }
                
                // Clear any old values and then append the new dynamic list options.          
                $(messageFieldTo).empty().append(optionsObj);                                            
            });
        }
        
        var hideAllButtonLists = function(){            
            $(".notifdetail-buttons").hide();
        }        
            
        var formatDate = function(dateStr){           
            dateObj = new Date(dateStr);
            var daysoftheweek = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"];
            var monthsoftheyear = ["January","February","March","April","May","June","July","August","September","October","November","December"];
            var month = dateObj.getMonth();
            var day = dateObj.getDate();
            var year = dateObj.getFullYear();
            var dayofweek = dateObj.getDay();            
            
            return daysoftheweek[dayofweek]+" "+monthsoftheyear[month]+" "+day+", "+year;                      
        }
        
        var fillInMessage = function(message){            
            var eventDate;
            var taskDate;
            var sendDate;                        
            
            // If it's a reminder, fill in the reminder categories after checking if it
            // is a task or an event, and show the proper fields for the user.
            if (message["sakai:category"] == "reminder") {
                $(messageRequiredYes).attr("checked", true);
                $(messageReminderCheckbox).attr("checked", true);
                $(messageReminderCheck).show();
                // Is it a task or an event?
                if (message["sakai:dueDate"] != null) {                    
                    // It's a task.
                    $(messageTaskCheck).attr("checked", true);
                    $(".cn-task").show();                    
                    taskDate = sakai.api.Util.parseSakaiDate(message["sakai:dueDate"]);
                    $(messageTaskDueDate).datepicker("setDate", taskDate);                    
                }
                else {
                    // It's an event.                                       
                    $(messageEventCheck).attr("checked", true);
                    $(".cn-event").show();
                    eventDate = sakai.api.Util.parseSakaiDate(message["sakai:eventDate"]);                                       
                    var hours = eventDate.getHours();
                    var minutes = eventDate.getMinutes();
                    var AMPM = "AM";                    
                    $(messageEventDate).datepicker("setDate", eventDate);
                    if (hours > 12) {
                        hours = hours - 12;
                        AMPM = "PM";
                    }                    
                    $(messageEventTimeHour).val(hours);                    
                    $(messageEventTimeMinute).val(minutes);                    
                    $(messageEventTimeAMPM).val(AMPM);                                                                                                  
                    $(messageEventPlace).val(message["sakai:eventPlace"]);                                                          
                }
            }
            else {
                $(messageRequiredNo).attr("checked", "checked");
                // Though not required, it could still be an event, so we should check 
                // and fill out the fields if necessary and show the correct page elements.
                if(message["sakai:eventDate"]!=null){
                    $(messageReminderCheckbox).attr("checked", "checked");
                    $(messageReminderCheck).show();
                    $(messageEventCheck).attr("checked", "checked");
                    $(".cn-event").show();
                    eventDate = sakai.api.Util.parseSakaiDate(message["sakai:eventDate"]);                                       
                    var hours = eventDate.getHours();
                    var minutes = eventDate.getMinutes();
                    var AMPM = "AM";                    
                    $(messageEventDate).datepicker("setDate", eventDate);
                    if (hours > 12) {
                        hours = hours - 12;
                        AMPM = "PM";
                    }                    
                    $(messageEventTimeHour).val(hours);                    
                    $(messageEventTimeMinute).val(minutes);                    
                    $(messageEventTimeAMPM).val(AMPM);                                                                                                  
                    $(messageEventPlace).val(message["sakai:eventPlace"]);
                }
            }
            
            // Fill out all the common fields.                    
            if(message["sakai:sendDate"]!=null){                
                sendDate = sakai.api.Util.parseSakaiDate(message["sakai:sendDate"]);
                $(messageFieldSendDate).datepicker("setDate", sendDate);
            }            
            $(messageFieldTo).val(message["sakai:to"]);            
            $(messageFieldSubject).val(message["sakai:subject"]);
            $(messageFieldBody).val(message["sakai:authoringbody"]);                                    
        }
        
        /**
         * Unbind everything to prevent duplicacy issues.
         */
        var unbindAll = function(){  
            $("#cn-saveasdraft-button").die();
            $("#cn-queue-button").die();
            $("#cn-cancel-button").die();                              
            $("#cn-queuedraft-button").die();
            $("#cn-updatedraft-button").die();
            $("#cn-deletedraft-button").die();
            $("#cn-deletetrashed-button").die();
            $("#cn-movetodrafts-button").die();
            $("#cn-copytodrafts-button").die();
            $("#cn-deletequeued-button").die();
            $("#cn-editrashed-button").die();                        
        }

        /**
         * This is the method that can be called from other widgets or pages.
         * @param {Object} userObj The user object containing the necessary information.          
         * @param {Object} callback When the message is sent this function will be called. If no callback is provided a standard message will be shown that fades out.
         * @param {Object} calledFrom What pane we called this widget from so we know what mode to put it in. (default: null)
         * @param {Object} message Message data for if we are pre-filling with information. (default: null)         
         */
        sakai.composenotification.initialise = function(calledFrom, message) {                       
            // Reset page back to its original condition.            
            resetView();
            clearInvalids();
            eventTimeInit();
            dynamicListInit();
            reenableView();                                       
            hideAllButtonLists(); 
            unbindAll();                                      
                       
            // Are we calling this from drafts?
            if(calledFrom=="drafts"){                
                // Fill out the proper information.
                fillInMessage(message);    
                
                // Hide all the buttons and show the proper button list.
                hideAllButtonLists();                                   
                $("#editdraft-buttons").show();                                  
                                
                // Queueing this draft...                
                $("#cn-queuedraft-button").live('click', function() {                                                         
                    if(checkFieldsForErrors(true)){                                                                                                                                                 
                        updateNotification(saveData("queue"), message, backToDrafts); 
                    }                                                         
                });         
                
                // Updating and re-saving this draft...
                $("#cn-updatedraft-button").live('click', function() {                    
                    updateNotification(saveData("drafts"), message, backToDrafts); 
                });         
                
                // Deleting the draft...
                $("#cn-deletedraft-button").live('click', function() {                    
                    updateNotification(saveData("trash"), message, backToDrafts);                    
                });                   
            }      
            
            // Are we calling this from queue?
            if(calledFrom=="queue"){
                // Now fill out the proper information.
                fillInMessage(message);
                
                // And disable the form, disallowing the user to edit.
                disableView();    
                
                // Display the proper buttons.                
                $("#queueview-buttons").show();   
                
                // Moving message from queue to drafts...
                $("#cn-movetodrafts-button").live('click', function() {
                   updateNotification(saveData("drafts"), message, backToQueue);                   
                });            
                
                // Copying message to drafts...
                $("#cn-copytodrafts-button").live('click', function() {                                    
                    postNotification(saveData("drafts"), backToQueue);                    
                });
                
                // Deleting message...
                $("#cn-deletequeued-button").live('click', function() {
                    updateNotification(saveData("trash"), message, backToQueue);                    
                });
            }  
            
            // Are we calling this from trash?       
            if(calledFrom=="trash"){                       
                // Now fill out the proper information.
                fillInMessage(message);
                // And disable the form, disallowing the user to edit.
                disableView();
                // Display the proper buttons.                
                $("#trashview-buttons").show();
                
                // Enable editing of message (move it to drafts and re-initialise widget).
                $("#cn-editrashed-button").live('click', function() {                                       
                    updateNotification(saveData("drafts", false), message);                    
                    sakai.composenotification.initialise("drafts", message);
                });                
            }
            
            // Else, user is creating a brand new blank notification.
            if(calledFrom==null){                             
                // When someone clicks on the 'Queue' button from base panel.
                $("#cn-queue-button").live('click', function(){                                      
                    if (checkFieldsForErrors(true)) {
                        postNotification(saveData("queue"), backToDrafts);
                    }                    
                });
                // When someone clicks on the 'Save as Draft' button from base panel.
        		$("#cn-saveasdraft-button").live('click', function() {                    
        			if ($(messageFieldSubject).val() != "") {
        				postNotification(saveData("drafts"), backToDrafts);
        			} else {
        				$(messageFieldSubject).addClass(invalidClass);
        			}
        		}); 
            }                                                                                   
        };                       
        
        var updateNotification = function(toUpdate, original, successCallback){                      
            $.ajax({
                url: original["jcr:path"],
                type: "POST",
                data: toUpdate,
                success: function(){
                    // If a callback function is specified in argument, call it.
                    if (typeof successCallback === "function") {
                        successCallback(true);
                    }                                          
                },
                error: function(){
                    alert("Failure on updating notification!");
                }
            });
        };     

        var saveData = function(box, isValidated){     
            var sendDate = $(messageFieldSendDate).datepicker("getDate");                                          
            var toPost = {
                "sakai:type": "notice",                
                "sakai:to": $(messageFieldTo).val(),                 
                "sakai:from": sakai.data.me.user.userid,
                "sakai:sendDate": sendDate,
                "sakai:sendDate@TypeHint": "Date",
                "sakai:subject": $(messageFieldSubject).val(), 
                "sakai:body": $(messageFieldBody).val(),
                "sakai:authoringbody": $(messageFieldBody).val(),
                "sakai:sendstate": "pending",
                "sakai:read": false,
                "sakai:messagebox": box,
				"sakai:validated": isValidated,
				"sakai:validated@TypeHint": "Boolean"         
            }                                                                       
                               
            // Is this notification required or not?               
            if($(messageRequiredYes).attr("checked")){                                     
                // Reminders (could be task or event).
                toPost["sakai:category"] = "reminder"; 
                // See if it is a task or an event and get the appropriate info.
                if($(messageTaskCheck).attr("checked")){                             
                    // It is a task.                                                                                                                   
                    toPost["sakai:dueDate"] = $(messageTaskDueDate).datepicker("getDate").toString();
                    toPost["sakai:dueDate@TypeHint"] = "Date";   
                    toPost["sakai:taskState"] = "created"; 
                    // Clear event details, in case it was an event before.
                    toPost["sakai:eventDate@Delete"]=true;                    
                    toPost["sakai:eventPlace@Delete"]=true;                   
                }
                else{                        
                    // It is an event.                                                                          
                    toPost["sakai:eventDate"] = $(messageEventDate).datepicker("getDate");                                        
                    toPost["sakai:eventDate"].setMinutes($(messageEventTimeMinute).val());                    
                    // Get the event time details and add to the eventDate obj.                    
                    if($(messageEventTimeAMPM).val()=="PM"){                                               
                        toPost["sakai:eventDate"].setHours(parseInt($(messageEventTimeHour).val())+12);                                            
                    }
                    else{
                        toPost["sakai:eventDate"].setHours($(messageEventTimeHour).val());                        
                    }
                    toPost["sakai:eventDate"] = toPost["sakai:eventDate"].toString();                                                                                                  
                    toPost["sakai:eventDate@TypeHint"] = "Date";
                    toPost["sakai:eventPlace"] = $(messageEventPlace).val();  
                    
                    var eventDetails = "Date: "+formatDate($(messageEventDate).datepicker("getDate"))+"\n"+
                                       "Time: "+$(messageEventTimeHour).val()+":"+$(messageEventTimeMinute).val()+" "+$(messageEventTimeAMPM).val()+"\n"+
                                       "Place: "+toPost["sakai:eventPlace"]+"\n\n"; 
                                   
                    toPost["sakai:body"] = eventDetails + toPost["sakai:authoringbody"];                    
                    // Clear task fields in case it was previously a task.
                    toPost["sakai:dueDate@Delete"]=true;               
                    toPost["sakai:taskState@Delete"]=true;                                                                                                                                                                                                                                                                                                                                                      
                }                    
            }
            else{                                                
                // Notifications (treated same as a message).                 
                toPost["sakai:category"] = "message";   
                
                // Clear task fields in case this message was previously a task.                
                toPost["sakai:dueDate@Delete"]=true;               
                toPost["sakai:taskState@Delete"]=true;                
               
                // Could still be an Event, meaning it is a non-required event with time, date, and place.
                if($(messageEventCheck).attr("checked")) {                    
                    toPost["sakai:eventDate"] = $(messageEventDate).datepicker("getDate");                                        
                    toPost["sakai:eventDate"].setMinutes($(messageEventTimeMinute).val());                    
                    // Get the event time details and add to the eventDate obj.                    
                    if($(messageEventTimeAMPM).val()=="PM"){                                               
                        toPost["sakai:eventDate"].setHours(parseInt($(messageEventTimeHour).val())+12);                                            
                    }
                    else{
                        toPost["sakai:eventDate"].setHours($(messageEventTimeHour).val());                        
                    }
                    toPost["sakai:eventDate"] = toPost["sakai:eventDate"].toString();                                                                                                  
                    toPost["sakai:eventDate@TypeHint"] = "Date";
                    toPost["sakai:eventPlace"] = $(messageEventPlace).val();  
                    
                    var eventDetails = "Date: "+formatDate($(messageEventDate).datepicker("getDate"))+"\n"+
                                       "Time: "+$(messageEventTimeHour).val()+":"+$(messageEventTimeMinute).val()+" "+$(messageEventTimeAMPM).val()+"\n"+
                                       "Place: "+toPost["sakai:eventPlace"]+"\n\n";  
                                   
                    toPost["sakai:body"] = eventDetails + toPost["sakai:authoringbody"];                                                                                                                                                                                                                                    
                }
                else{
                    // Clear event fields too, since it may have been (but no longer is) an event.                    
                    toPost["sakai:eventDate@Delete"]=true;                    
                    toPost["sakai:eventPlace@Delete"]=true;
                }                                          
            }                                   
            return toPost;
        }
        
        var backToDrafts = function () {            
            sakai.notificationsinbox.showDrafts();
        }
        
        var backToQueue = function () {            
            sakai.notificationsinbox.showQueue();
        }
        
        var postNotification = function(toPost, successCallback){                                                                                                                                    
            // Post all the data in an Ajax call.    
            $.ajax({
                url: "http://localhost:8080/user/"+sakai.data.me.user.userid+"/message.create.html",
                type: "POST",
                data: toPost,
                success: function(){
                    // If a callback function is specified in argument, call it.
                    if (typeof successCallback === "function") {
                        successCallback(true);
                    }
                }, 
                error: function(){
                    alert("Failure on posting notification!");                        
                }
            });
        }                              
    };              
}

sakai.api.Widgets.widgetLoader.informOnLoad("composenotification");