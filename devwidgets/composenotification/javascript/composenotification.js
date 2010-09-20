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
          '01' : '01',
          '02' : '02',
          '03' : '03',
          '04' : '04',
          '05' : '05',
          '06' : '06',
          '07' : '07',
          '08' : '08',
          '09' : '09',
          '10' : '10',
          '11' : '11',
          '12' : '12'  
        };
              
        var allMinuteOptions = {        
          '00' : '00',
          '15' : '15',
          '30' : '30',
          '45' : '45'      
        };
        
        var allAMPMOptions = {         
          'AM' : 'AM',
          'PM' : 'PM'  
        };       
        
        // Dynamic list options (for send to).
        var allDynamicListOptions = {
          'group1id' : 'Group 1',
          'group2id' : 'Group 2',
          'group3id' : 'Group 3'  
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
         * notification authoring page. (Used in the non-editable notification
         * detail views.)
         */
        var disableView = function() {            
            $(".compose-form-elm").attr("disabled","disabled");       
            $(".composenotification_taskorevent").show();
            $(".cn-task").show();
            $(".cn-event").show();         
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
            
            if(type=="text" || tag=="textarea"){                
                toClear.value="";
            }   
            else if(type=="checkbox" || type=="radio"){                
                toClear.checked=false;
            }
            else if(tag=="select"){                           
                toClear.selectedIndex=-1;                
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
            reenableView();
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
            buttonText: 'Click to pick a date.',        
        });     
        $("#datepicker-taskduedate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.',        
        });     
        $("#datepicker-eventdate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.',        
        });     
        $("#datepicker-eventstopdate-text").datepicker({
            showOn: 'button',
            buttonImage: '/devwidgets/composenotification/images/calendar_icon.gif',
    		buttonImageOnly: true,        
            buttonText: 'Click to pick a date.',        
        });                
        
        // Testing validation. REMOVE LATER.
        $('#validation-test').click(function(){            
            checkFieldsForErrors();                    
        });            
        
        // Testing disabling. REMOVE LATER.
        $('#disabling-test').click(function(){
            disableView();    
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
         
         // If 'Yes' is checked for required, then automatically check that it has a date.
         $(messageRequiredYes).change(function(){            
            $(messageReminderCheckbox).attr("checked", true);                                    
            $(".composenotification_taskorevent").show();                                                   
         });                                                     
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
         * @return valid True = no errors, false = errors found.
         */
        var checkFieldsForErrors = function() {                        
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
            
            // Remove the invalidClass from each element first.
            clearInvalids();

            // Check for invalid values.            
            if(!requiredYes && !requiredNo){
                valid = false;
                requiredEl.addClass(invalidClass);                
            }           
            if(!sendDate) {
                valid = false;
                sendDateEl.addClass(invalidClass);               
            }                                    
            else if(modtoday>$(sendDateEl).datepicker("getDate")){                                              
                valid = false;
                sendDateEl.addClass(invalidClass);                
            }
            if(!sendTo){
                valid = false;
                sendToEl.addClass(invalidClass);                
            }
            
            if (!subject) {
                valid = false;
                subjectEl.addClass(invalidClass);              
            }
            if (!body) {
                valid = false;
                bodyEl.addClass(invalidClass);                
            }
            if(reminderCheck){
                // This notification is a REMINDER. Either task or event must be checked.                
                if(!taskCheck && !eventCheck){
                    valid = false;
                    reminderCheckEl.addClass(invalidClass);                    
                }
                else if (taskCheck) {
                    // The reminder is a TASK.                    
                    if (!taskDueDate) {
                        valid = false;
                        taskDueDateEl.addClass(invalidClass);
                    }
                    else if(modtoday>$(taskDueDateEl).datepicker("getDate")) {                                              
                        valid = false;
                        taskDueDateEl.addClass(invalidClass);
                    }
                }
                else if (eventCheck) {
                    // The reminder is an EVENT.                                     
                    if (!eventDate) {
                        valid = false;
                        eventDateEl.addClass(invalidClass);
                    }
                    else if(modtoday>$(eventDateEl).datepicker("getDate")){                                                    
                        valid = false;
                        eventDateEl.addClass(invalidClass);            
                    }
                    if (!eventTimeHour) {
                        valid = false;
                        eventTimeHourEl.addClass(invalidClass);
                    }                    
                    if (!eventTimeMinute) {
                        valid = false;
                        eventTimeMinuteEl.addClass(invalidClass);
                    }
                    if (!eventTimeAMPM) {
                        valid = false;
                        eventTimeAMPMEl.addClass(invalidClass);
                    }
                    if (!eventPlace) {
                        valid = false;
                        eventPlaceEl.addClass(invalidClass);
                    } 
                    
                    // Checking event time for invalids.
                    if(eventTimeHour && eventTimeMinute && eventTimeAMPM){                        
                        var currentHour = today.getHours();                                               
                        var currentMin = today.getMinutes();
                        var compareToHour = parseInt(eventTimeHour); 
                        var compareToMin = parseInt(eventTimeMinute); 
                        
                        // Convert the user-picked hour to military time.                      
                        if (eventTimeAMPM=="PM"){
                            compareToHour = compareToHour+12;                                                       
                        }
                            
                        // Compare the hours.
                        // If they are equal, also compare the minutes.                    
                        if (compareToHour<currentHour){                           
                            valid = false;
                            eventTimeHourEl.addClass(invalidClass);
                            eventTimeMinuteEl.addClass(invalidClass);
                            eventTimeAMPMEl.addClass(invalidClass);
                        }       
                        else if (compareToHour==currentHour && compareToMin<currentMin){                          
                            valid = false;
                            eventTimeHourEl.addClass(invalidClass);
                            eventTimeMinuteEl.addClass(invalidClass);
                            eventTimeAMPMEl.addClass(invalidClass);
                        }               
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
            // Selecting the 'options' attributes on the various drop down menus.
            var eventTimeHoursOptions = $(messageEventTimeHour).attr('options');
            var eventTimeMinutesOptions = $(messageEventTimeMinute).attr('options');
            var eventTimeAMPMOptions = $(messageEventTimeAMPM).attr('options');
            
            // First, clear any pre-existing options that might be there already.
            // (Helps prevent duplication when user hits the 'Cancel' button.)                        
            $(messageEventTimeHour).html('');	     
            $(messageEventTimeMinute).html('');	      
            $(messageEventTimeAMPM).html(''); 
                      
            // Then, fill in all the options for each drop down menu based on the arrays.
            $.each(allHourOptions, function(val, text) {
                eventTimeHoursOptions[eventTimeHoursOptions.length] = new Option(text, val);
	        });   
            $.each(allMinuteOptions, function(val, text) {
                eventTimeMinutesOptions[eventTimeMinutesOptions.length] = new Option(text, val);
	        });          
            $.each(allAMPMOptions, function(val, text) {
               eventTimeAMPMOptions[eventTimeAMPMOptions.length] = new Option(text, val); 
            });                                          
        };
        
        /**
         * Sets up drop down menu for Dynamic List field (or otherwise known as
         * the 'Send To' field) based on the array pre-defined earlier in the JS.
         */
        var dynamicListInit = function(){
          // Select the 'options' attribute of the dynamic list drop down menu.
          var dynamicListOptions = $(messageFieldTo).attr('options');
          
          // Clear any pre-existing options that might be there already.
          // (Helps prevent duplication when user hits the 'Cancel' button.)
          $(messageFieldTo).html('');
          
          // Fill in options for menu based on array.
          $.each(allDynamicListOptions, function(val, text) {
              dynamicListOptions[dynamicListOptions.length] = new Option(text, val);
          });          
        };

        /**
         * This is the method that can be called from other widgets or pages.
         * @param {Object} userObj The user object containing the necessary information.
         * @param {Boolean} allowOtherReceivers If the user can add other users, default = false.    
         * @param {Object} callback When the message is sent this function will be called. If no callback is provided a standard message will be shown that fades out.
         */
        sakai.composenotification.initialise = function(userObj, allowOtherReceivers, callback, subject, body) {            
            // Reset page back to its original condition.
            clearInvalids();
            eventTimeInit();
            dynamicListInit();
            resetView();            

            // The user we are sending a message to.
            user = userObj;                                       
        };       
        
        // When someone clicks the send button.
        $("#cn-queue-button").bind("click", function(ev) {                                              
            // Check the fields if there are any required fields that are not filled in.
            if (checkFieldsForErrors()) {
                // Common values.
                var toSend = {
                    "sakai:type": "notice",
                    "sakai:to": $(messageFieldTo).val(), 
                    "sakai:from": sakai.data.me.user.userid,
                    "sakai:subject": $(messageFieldSubject).val(), 
                    "sakai:body": $(messageFieldBody).val(),
                    "sakai:sendstate": "pending",
                    "sakai:read": false,
                    "sakai:messagebox": "inbox"
                }                                     
                               
                // Is this notification required or not?               
                if($(messageRequiredYes).attr("checked")){                    
                    // Reminders (could be task or event).
                    toSend["sakai:category"] = "reminder";                   
                }
                else{                    
                    // Notifications (treated same as a message).  
                    toSend["sakai:category"] = "message";                                           
                }                                  
                
                // See if it is a task or an event and get the appropriate info.
                if($(messageTaskCheck).attr("checked", true)){
                    toSend["sakai:dueDate"] = $(messageTaskDueDate).datepicker("getDate");
                    toSend["sakai:dueDate@TypeHint"] = "date";    
                    toSend["sakai:taskState"] = "created";
                }
                else{
                    toSend["sakai:eventDate"] = $(messageEventDate).datepicker("getDate");
                    toSend["sakai:eventDate@TypeHint"] = "date";
                    toSend["sakai:eventPlace"] = $(messageEventPlace).val();
                    toSend["sakai:eventTime"] = $(messageEventTimeHour).val()+":"+$(messageEventTimeMinute).val()+" "+$(messageEventTimeAMPM).val();
                }            
                              
                // Post all the data in an AJAX call.    
                $.ajax({
                    url: "http://localhost:8080/user/"+sakai.data.me.user.userid+"/message.create.html",
                    type: "POST",
                    data: toSend,
                    success: function(){
                        alert("Success!");
                    }, 
                    error: function(){
                        alert("Failure!");
                    }
                });            
            }
        });
    };              
}

sakai.api.Widgets.widgetLoader.informOnLoad("composenotification");