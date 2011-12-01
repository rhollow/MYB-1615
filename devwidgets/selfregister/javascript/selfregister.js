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
     * @name sakai_global.selfregister
     *
     * @class selfregister
     *
     * @version 0.0.1
     * @param {String} tuid Unique id of the widget
     * @param {Boolean} showSettings Show the settings of the widget or not
     */
    sakai_global.selfregister = function (tuid, showSettings) { 

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////
        
        var $parentElm = $("#" + tuid);

        var $selfregisterDialog = $("#join_myberkeley_dialog", $parentElm);
        var $chkAgreeToSocu = $("#chk_agree_to_socu", $parentElm);
        var $chkAgreeToShareData = $("#chk_agree_to_share_data", $parentElm);
        var $btnJoinMyBerkeley = $("#btn_join_my_berkeley", $parentElm);
        var $btnOptOut = $("#btn_cancel_optin", $parentElm);

        var ajaxPostTimeBeforeLogOutMs = 3000;	// Time in milliseconds before logging user out (for reading an error message)
        var optoutURL = '/dev/logout.html';

        var personAttributes = {};

        ///////////////////////
        // Utility functions //
        ///////////////////////
        
        /**
         * Fill in provided personal attributes
         */
        function displayProvidedData() {
            $(".firstName").text(personAttributes.firstName || "");
            $(".lastName").text(personAttributes.lastName || "");
            $.each(['email', 'role', 'major', 'college'], function(i, attr) {
                if (personAttributes[attr]) {
                    $("#selfregister_dt_" + attr + " .selfregister_attribute").text(personAttributes[attr]);
                    $("#selfregister_dt_" + attr).show();
                    $("#selfregister_dd_" + attr).show();
               }
            });
        };

       /**
        * Checks if user agreed to participate and enables/disables "Join CalCentral" button
        */
        function checkIfUserAgreed() {

            var enableJoinButton = ($chkAgreeToSocu.is(":checked") && $chkAgreeToShareData.is(":checked"));

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
         * Makes current user a participant.
         */
        function makeParticipant() {
            $.ajax({
                url: "/system/myberkeley/selfProvision",
                type: "POST",
                data: {},
                success: function(data){
                    $selfregisterDialog.jqmHide();
                    showGeneralMessage("Welcome to CalCentral!", false);

                    // Wait for 2 seconds
                    setTimeout(function(){
                        // Round trip through CAS to handle the OAE authentication.
                        document.location = "/system/sling/cas/login?resource=/me";
                    }, 2000);
                },
                error: function(xhr, textStatus, thrownError){
                    showGeneralMessage("Unable to save the data. Logging out...", true);
                    setTimeout(logOut, ajaxPostTimeBeforeLogOutMs);
                }
            });
        };

        ////////////////////
        // Event handling //
        ////////////////////

        $chkAgreeToSocu.change(checkIfUserAgreed);

        $chkAgreeToShareData.change(checkIfUserAgreed);
        
        // Sets user's "participant" status to true
        $btnJoinMyBerkeley.click(function() {
            makeParticipant();
        });
        $btnOptOut.click(function () {
            document.location = optoutURL; 
         });

        var isLoggedIn = function() {
            var person = sakai.data.me;
            var uuid = person.user.userid;
            return !(!uuid || person.user.anon);
        };
        
        var renderDialog = function() {
            $selfregisterDialog.jqm({
                modal: true,
                overlay: 20,
                toTop: true,
                onShow: null
            });
            $selfregisterDialog.jqmShow();
            displayProvidedData();
        }
        
        var checkRegistrationState = function(success, data) {
            if (success) {
                personAttributes = data;
                // If the self-provision servlet is not running, Sling may automatically
                // return an empty JSON object rather than a 404 status. Check for the
                // most basic of the person attributes before proceeding.
                if (personAttributes[":name"]) {
                    renderDialog();
                }
            }
        }
        
        var getRegistrationState = function() {
            if (!isLoggedIn()) {
                sakai.api.Server.loadJSON("/system/myberkeley/selfProvision.json", checkRegistrationState, {});
            }
        }

        var doInit = function() {
            getRegistrationState();
        };
        
        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("selfregister");
});
