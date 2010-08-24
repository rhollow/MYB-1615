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

        var rootel = $(tuid);

        var user = false; // user object that contains the information for the user that should be posted to.
        var me = sakai.data.me;        
        var callbackWhenDone = null;    //    Callback function for when the message gets sent       

        /**
         * 
         * CSS IDS
         * 
         */
        var dialogBoxContainer = "#sendmessage_dialog_box";
        var dialogFooterContainer = "#sendmessage_dialog_footer";

        var messageDialogContainer = '#message_dialog';
        var messageDone = "#message_done";
        var messageForm = "#message_form";

        var messageSingleToContainer = "#sendmessage_fixed_to_user";
        var messageMultipleToContainer = "#sendmessage_multiple_to_container";
        var messageMultipleToInputContainer = "#sendmessage_multiple_to_input_container";
        var messageMultipleToBox = "#sendmessage_box_template";
        var messageMultipleToBoxResult = ".sendmessage_multipleBox_result";
        var messageMultipleToBoxDelete = ".sendmessage_multipleBox_delete";
        var messageMultipleToWhat = "#sendmessage_multiple_to_what";

        // NOTIFICATION AUTHORING FORM ELEMENT IDS. (All required unless otherwise stated.)
        var messageFieldRequiredCheck = "#composenotification_required";
        var messageRequiredYes = "#cn-requiredyes";
        var messageRequiredNo = "#cn-requiredno";        
        var messageFieldSendDate = "#datepicker-senddate-text";
        var messageFieldTo = "#cn-dynamiclistselect";        
        var messageFieldSubject = "#cn-subject";        
        var messageFieldBody = "#cn-body";
        
        // Optional.
        var messageReminderCheck = "#task-or-event-check";
        var messageReminderCheckbox = "#cn-remindercheck";
        
        // Optional, unless messageReminderCheck is checked.
        var messageTaskCheck = "#task-radio"; 
        var messageEventCheck = "#event-radio";
        
        // Optional, unless messageTaskCheck is checked.
        var messageTaskDueDate = "#datepicker-taskduedate-text"; 
        
        // Optional, unless messageEventCheck is checked.
        var messageEventDate = "#datepicker-eventdate-text"; 
        var messageEventTime = "#cn-event-time";
        var messageEventAMPM = "#cn-event-ampm";
        var messageEventPlace = "#cn-event-place";
                
        var buttonSendMessage = "#send_message";

        var invalidClass = "composenotification_invalid";
        var errorClass = "composenotification_error_message";
        var normalClass = "composenotification_normal_message";
        var dialogBoxClass = "dialogue_box";
        var dialogFooterClass = "dialog_footer";
        var dialogHeaderClass = ".dialog_header";
        var dialogClass = ".dialog";

        var messageOK = "#sendmessage_message_ok";
        var messageError = "#sendmessage_message_error";
        var messageErrorFriends = "#sendmessage_friends_error";        
        
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
        $("#cn-remindercheck").click(function() {
            $(messageTaskDueDate).removeClass(invalidClass); 
            $(messageEventDate).removeClass(invalidClass);
            $(messageEventTime).removeClass(invalidClass);
            $(messageEventAMPM).removeClass(invalidClass);
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
        $('#task-radio').click(function() {
            $(".cn-task").show();
            $(".cn-event").hide();
            $(".cn-event").find(".compose-form-elm").each(function(){                
                clearElement(this);
            });                      
            $(messageEventDate).removeClass(invalidClass);
            $(messageEventTime).removeClass(invalidClass);
            $(messageEventAMPM).removeClass(invalidClass);
            $(messageEventPlace).removeClass(invalidClass);
        });          
        $('#event-radio').click(function() {
            $(".cn-task").hide();        
            $(".cn-event").show();
            $(".cn-task").find(".compose-form-elm").each(function(){             
                clearElement(this);
            });            
            $(messageTaskDueDate).removeClass(invalidClass);                                                                                                         
        });     
        
        // DATEPICKER FUNCTIONS
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
        })    

        /**         
         * This method will check if there are any required fields that are not filled in.
         * If a field is not filled in the invalidClass will be added to that field.
         * Returns a boolean that determines if the fields are valid or not.
         * @return valid True = no errors, false = errors found.
         */
        var checkFieldsForErrors = function() {
            // By default, valid is true.             
            var valid = true;
            
            // Collect all elements that require validation on the page.         
            var requiredEl = $(messageFieldRequiredCheck);            
            var sendDateEl = $(messageFieldSendDate);
            var sendToEl = $(messageFieldTo);           
            var subjectEl = $(messageFieldSubject);
            var bodyEl = $(messageFieldBody);                           
            var reminderCheckEl = $(messageReminderCheck); 
            var taskDueDateEl = $(messageTaskDueDate);                                                          
            var eventDateEl = $(messageEventDate);
            var eventTimeEl = $(messageEventTime);
            var eventAMPMEl = $(messageEventAMPM);
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
            var eventTime = eventTimeEl.val();
            var eventAMPM = eventAMPMEl.val();
            var eventPlace = eventPlaceEl.val();               
            
            // Remove the invalidClass from each element first.
            clearInvalids();

            // Check each one and set the boolean valid as appropriate.
            if(!requiredYes && !requiredNo){
                valid = false;
                $(requiredEl).addClass(invalidClass);                
            }
            if(!sendDate) {
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
                else if(taskCheck){
                    // The reminder is a TASK.                    
                    if(!taskDueDate){
                        valid = false;
                        taskDueDateEl.addClass(invalidClass);                       
                    }
                }
                else if(eventCheck){   
                    // The reminder is an EVENT.                                     
                    if(!eventDate){
                        valid = false;
                        eventDateEl.addClass(invalidClass);                        
                    }
                    if(!eventTime){
                        valid = false;
                        eventTimeEl.addClass(invalidClass);                      
                    }
                    if(!eventAMPM){
                        valid = false;
                        eventAMPMEl.addClass(invalidClass);                        
                    }
                    if(!eventPlace){
                        valid = false;
                        eventPlaceEl.addClass(invalidClass);                       
                    }
                }
            }
            // Return the status of the form.        
            return valid;
        };

        /**
         * This is the method that can be called from other widgets or pages.
         * @param {Object} userObj The user object containing the necessary information.
         * @param {Boolean} allowOtherReceivers If the user can add other users, default = false.    
         * @param {Object} callback When the message is sent this function will be called. If no callback is provided a standard message will be shown that fades out.
         */
        sakai.composenotification.initialise = function(userObj, allowOtherReceivers, callback, subject, body) {            
            // Make sure that everything is standard.
            clearInvalids();
            resetView();

            // The user we are sending a message to.
            user = userObj;        
          
            // Remove the dialog stuff.
            $(dialogHeaderClass).remove();
            $(messageDialogContainer).removeClass(dialogClass.replace(/\./, ''));
            $(dialogBoxContainer).removeClass(dialogBoxClass);
            $(dialogFooterContainer).removeClass(dialogFooterClass);            
        };

        // EDIT EDIT EDIT
        // When someone clicks the send button.
        $(buttonSendMessage).bind("click", function(ev) {

            // Check the fields if there are any required fields that are not filled in.
            if (checkFieldsForErrors()) {

                var body = $(messageFieldBody).val();
                var subject = $(messageFieldSubject).val();

                var tosend = selectedFriendsToPostTo.length;
                var sent = 0;

                for (var i = 0; i < selectedFriendsToPostTo.length; i++){

                    sakai.api.Communication.sendMessage(selectedFriendsToPostTo[i], subject, body, "message", null, function(success, data){
                        sent++;
                        if (sent === tosend){
                            showMessageSent(success);
                        }
                    });
                }
            }
        });
    };              
}

sakai.api.Widgets.widgetLoader.informOnLoad("composenotification");