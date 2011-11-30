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

// load the master sakai object to access all Sakai OAE API methods
require(["jquery","sakai/sakai.api.core", "myb/myb.api.core"], function($, sakai, myb) {

    /**
     * @name sakai_global.optin
     *
     * @class optin
     *
     * @version 0.0.1
     * @param {String} tuid Unique id of the widget
     * @param {Boolean} showSettings Show the settings of the widget or not
     */
    sakai_global.optin = function (tuid, showSettings) { 
    
        /////////////////////////////
        // Configuration variables //
        /////////////////////////////
        
        var $parentElm = $("#" + tuid);

        var $optinDialog = $("#join_myberkeley_dialog", $parentElm);
        var $chkAgreeToSocu = $("#chk_agree_to_socu", $parentElm);
        var $chkAgreeToReceiveAdviserMessages = $("#chk_agree_to_receive_adviser_messages", $parentElm);
        var $btnJoinMyBerkeley = $("#btn_join_my_berkeley", $parentElm);
        var $btnOptOut = $("#btn_cancel_optin", $parentElm);

        var ajaxPostTryNumber = 0;				// Counts failed AJAX POST tries before giving up
        var ajaxPostMaxTries = 3;				// Number of tries before giving up
        var ajaxPostRetryIntervalMs = 500;		// Retry interval in milliseconds
        var ajaxPostTimeBeforeLogOutMs = 3000;	// Time in milliseconds before logging user out (for reading an error message)
        
        var optoutURL = '/dev/logout.html';

        ///////////////////////
        // Utility functions //
        ///////////////////////

       /**
        * Checks if user agreed to participate and enables/disables "Join CalCentral" button
        */
        function checkIfUserAgreed() {

            var enableJoinButton = ($chkAgreeToSocu.is(":checked") && $chkAgreeToReceiveAdviserMessages.is(":checked"));

            $btnJoinMyBerkeley.attr('disabled', !enableJoinButton);

            // Making keyboard navigation easier
            if (enableJoinButton) {
                $btnJoinMyBerkeley.focus();
            }
        }

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
         * Redirects to logout page
         */
        function logOut() {
            document.location = "/dev/logout.html";
        };

        /**
         * Makes current user a participant. The function will try several times to save the data before giving up,
         * if this happens the user will be logged out.
         */
        function makeParticipant() {
            
            $.ajax({
                url: "/~" + sakai.data.me.profile["rep:userId"] + "/public/authprofile/myberkeley/elements/joinDate",
                traditional: true,
                type: "POST",
                data: {
                    "value@TypeHint": "date",
                    date: new Date()
                },
                success: function(data) {},
                error: function(xhr, textStatus, thrownError) {
    
                    // Not the last try?
                    if (ajaxPostTryNumber === 0) {
                        showGeneralMessage("Saving failed. Will retry...", true);
                    } else if (ajaxPostTryNumber === ajaxPostMaxTries - 1) {
                        // Saving failed for good
                        showGeneralMessage("Unable to save the data. Logging out...", true);
                        setTimeout(logOut, ajaxPostTimeBeforeLogOutMs);
                        return;
                    }
    
                    // Retrying ...
                    ajaxPostTryNumber++;
                    setTimeout(makeParticipant, ajaxPostRetryIntervalMs);
                }
            });
            
            ajaxPostTryNumber = 0;
            $.ajax({
                url: "/~" + sakai.data.me.profile["rep:userId"] + "/public/authprofile/myberkeley/elements/participant",
                traditional: true,
                type: "POST",
                data: {
                    value: $.toJSON(true)
                },
                success: function(data) {
                    $optinDialog.jqmHide();
                    showGeneralMessage("Welcome to CalCentral!", false);
                },
                error: function(xhr, textStatus, thrownError) {
    
                    // Not the last try?
                    if (ajaxPostTryNumber === 0) {
                        showGeneralMessage("Saving failed. Will retry...", true);
                    } else if (ajaxPostTryNumber === ajaxPostMaxTries - 1) {
                        // Saving failed for good
                        showGeneralMessage("Unable to save the data. Logging out...", true);
                        setTimeout(logOut, ajaxPostTimeBeforeLogOutMs);
                        return;
                    }
    
                    // Retrying ...
                    ajaxPostTryNumber++;
                    setTimeout(makeParticipant, ajaxPostRetryIntervalMs);
                }
            });
        };

        ////////////////////
        // Event handling //
        ////////////////////

        $chkAgreeToSocu.change(checkIfUserAgreed);

        $chkAgreeToReceiveAdviserMessages.change(checkIfUserAgreed);
        
        // Sets user's "participant" status to true
        $btnJoinMyBerkeley.click(function() {

            if (sakai.data.me.profile) {
                makeParticipant();
            } else {
                showGeneralMessage("Error: User profile data is not available.", true);
            }

        });
        
        $btnOptOut.click(function () {
           document.location = optoutURL; 
        });

        var isLoggedIn = function() {
            var person = sakai.data.me;
            var uuid = person.user.userid;
            return !(!uuid || person.user.anon);
        };

        var doInit = function() {
            // If the user is a member of a CalCentral pilot college, but not a participant of CalCentral project,
            // redirect him to the participation explanation page
            if (isLoggedIn() && !myb.api.security.isMyBerkeleyParticipant()) {
                // We will show a nice Join CalCentral dialog on this page
                $optinDialog.jqm({
                         modal: true,
                         overlay: 20,
                         toTop: true,
                         onShow: null
                     });
                $optinDialog.jqmShow();
            }
        };
        
        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("optin");
});
