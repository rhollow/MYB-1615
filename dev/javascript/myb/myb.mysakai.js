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


/*global Querystring, Config, $,  set_cookie */

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core"], function($, sakai, myb) {

    /* HACK : Preventing redirection to 'not-a-participant' page from this page because this page IS the 'not-a-participant' page
     See myb.securepage.js for details. */
    myb.api.security.allowRedirectToParticipantPage = false;

    sakai_global.myberkeleymysakai = function(){

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////

        var chkAgreeToSocu = "#chk_agree_to_socu";
        var chkAgreeToReceiveAdvisorMessages = "#chk_agree_to_receive_advisor_messages";
        var btnJoinMyBerkeley = "#btn_join_my_berkeley";

        var ajaxPostTryNumber = 0;				// Counts failed AJAX POST tries before giving up
        var ajaxPostMaxTries = 3;				// Number of tries before giving up
        var ajaxPostRetryIntervalMs = 500;		// Retry interval in milliseconds
        var ajaxPostTimeBeforeLogOutMs = 3000;	// Time in milliseconds before logging user out (for reading an error message)


        ///////////////////////
        // Utility functions //
        ///////////////////////

       /**
        * Checks if user agreed to participate and enables/disables "Join myBerkeley" button
        */
        function checkIfUserAgreed() {

            var enableJoinButton = ($(chkAgreeToSocu).is(":checked") && $(chkAgreeToReceiveAdvisorMessages).is(":checked"));

            $(btnJoinMyBerkeley).attr('disabled', !enableJoinButton);

            // Making keyboard navigation easier
            if (enableJoinButton) {
                $(btnJoinMyBerkeley).focus();
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
        }

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
				value: "@TypeHint=date"
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
				$("#join_myberkeley_dialog").jqmHide();
				showGeneralMessage("Welcome to myBerkeley portal!", false);
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
        }

        ////////////////////
        // Event handling //
        ////////////////////

        $(chkAgreeToSocu).change(function() {
            checkIfUserAgreed();
        });

        $(chkAgreeToReceiveAdvisorMessages).change(function() {
            checkIfUserAgreed();
        });

        // Makes keyboard navigation easier
        $(chkAgreeToSocu).keyup(function() {
            $(chkAgreeToReceiveAdvisorMessages).focus();
        });

        // Makes keyboard navigation easier
        $(chkAgreeToReceiveAdvisorMessages).keyup(function() {
            $(btnJoinMyBerkeley).focus();
        });

        // Sets user's "participant" status to true
        $(btnJoinMyBerkeley).click(function() {

            if (sakai.data.me.profile) {
                makeParticipant();
            } else {
                showGeneralMessage("Error: User profile data is not available.", true);
            }

        });

        /////////////////////////////
        // Initialization function //
        /////////////////////////////

        var doInit = function() {
            // If the user is a member of Berkeley's College of Environmental Design, but not a participant of myBerkeley project,
            // redirect him to the participation explanation page
            if (!myb.api.security.isMyBerkeleyParticipant()) {
                // We will show a nice Join myBerkeley dialog on this page
                $("#join_myberkeley_dialog").jqm({
                         modal: true,
                         overlay: 20,
                         toTop: true,
                         onShow: null
                     });

                $("#join_myberkeley_dialog").css("margin-left","-285px");
                $("#join_myberkeley_dialog").css("width","570px");
                $("#join_myberkeley_dialog").jqmShow();
            }
        };

        doInit();
    };

    sakai.api.Widgets.Container.registerForLoad("myberkeleymysakai");

});
