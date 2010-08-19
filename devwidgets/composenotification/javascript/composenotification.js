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

        // CSS IDs EDIT EDIT EDIT
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

        var messageFieldSubject = "#comp-subject";
        var messageFieldBody = "#comp-body";
        var messageFieldFrom = "#message_from";
        var messageFieldToSingle = "#message_to";
        var messageFieldMultipleTo = "#sendmessage_multiple_to";

        var messageToResult = ".sendmessage_compose_to_result";

        var buttonSendMessage = "#send_message";

        var invalidClass = "sendmessage_invalid";
        var errorClass = 'sendmessage_error_message';
        var normalClass = 'sendmessage_normal_message';
        var dialogBoxClass = "dialogue_box";
        var dialogFooterClass = "dialog_footer";
        var dialogHeaderClass = ".dialog_header";
        var dialogClass = ".dialog";

        var messageOK = "#sendmessage_message_ok";
        var messageError = "#sendmessage_message_error";
        var messageErrorFriends = "#sendmessage_friends_error";
        
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
        }

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
        
        // REMINDER CRITERION FUNCTIONS
        $("#cn-remindercheck").click(function() {
            if($("#cn-remindercheck").is(":checked")){
                $(".composenotification_taskorevent").show();    
            }
            else{
                $(".composenotification_taskorevent").hide();
                $(".composenotification_taskorevent").find(".compose-form-elm").each(function() {
                    clearElement(this);    
                });
            };
        });
            
        // TASK AND EVENT DETAIL FUNCTIONS 
        $('#event-radio').click(function() {
            $(".cn-task").hide();        
            $(".cn-event").show();
            $(".cn-task").find(".compose-form-elm").each(function(){             
                clearElement(this);
            })
        });
        $('#task-radio').click(function() {
            $(".cn-task").show();
            $(".cn-event").hide();
            $(".cn-event").find(".compose-form-elm").each(function(){                
                clearElement(this);
            })
        });  

        /**
         * EDIT EDIT EDIT
         * This method will check if there are any required fields that are not filled in.
         * If a field is not filled in the invalidClass will be added to that field.
         * @return true = no errors, false = error
         */
        var checkFieldsForErrors = function() {
            var subjectEl = $(messageFieldSubject);
            var bodyEl = $(messageFieldBody);

            var valid = true;
            var subject = subjectEl.val();
            var body = bodyEl.val();

            subjectEl.removeClass(invalidClass);
            bodyEl.removeClass(invalidClass);

            if (!subject) {
                valid = false;
                subjectEl.addClass(invalidClass);
            }
            if (!body) {
                valid = false;
                bodyEl.addClass(invalidClass);
            }

            if (selectedFriendsToPostTo.length === 0) {
                //    No user selected.
                valid = false;
                //    This can only happen when a user wants to add multiple persons.
                $(messageMultipleToInputContainer).addClass(invalidClass);
                $(messageFieldMultipleTo).addClass(invalidClass);
            }
            return valid;
        };

        /**
         * This is the method that can be called from other widgets or pages
         * @param {Object} userObj The user object containing the nescecary information {uuid:  "user1", firstName: "foo", lastName: "bar"}
         * @param {Boolean} allowOtherReceivers If the user can add other users, default = false    
         * @param {Object} callback When the message is sent this function will be called. If no callback is provided a standard message will be shown that fades out.
         */
        sakai.composenotification.initialise = function(userObj, allowOtherReceivers, callback, subject, body) {
            
            // Make sure that everything is standard.
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
}

sakai.api.Widgets.widgetLoader.informOnLoad("composenotification");