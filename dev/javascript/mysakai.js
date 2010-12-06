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


/*global Config, $ */

var sakai = sakai || {};

sakai.mysakai = function(){

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
        
    }
	
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

    var showHideMoreMenu = function(hideOnly){
        var el = $("#mysakai_more_menu");
        if (el) {
            if (el.css("display").toLowerCase() !== "none" || hideOnly) {
                $("#mysakai_more_link").removeClass("clicked");
                el.hide();
            } else {
                $("#mysakai_more_link").addClass("clicked");
                var x = $("#mysakai_more_link").position().left;
                var y = $("#mysakai_more_link").position().top;
                el.css(
                        {
                          "top": y + 28 + "px",
                          "left": x + 2 + "px"
                        }
                    ).show();
            }
        }
    };

    var addBindings = function() {
        // Bind Insert Link click event
        $("#mysakai_more_link").live("click", function(ev){
            showHideMoreMenu(false);
        });
        // Bind mousedown and mouseup to give an optional clicking effect via css
        $("#mysakai_more_link").live("mousedown", function(e) {
            $("#mysakai_more_link").addClass("clicking");
        });
        $("#mysakai_more_link").live("mouseup", function(e) {
            $("#mysakai_more_link").removeClass("clicking");
        });

        $("#mysakai_more_customize_page").live("click", function() {
            $(window).trigger("sakai-dashboard-showAddWidgetDialog", "mysakaidashboard");
            showHideMoreMenu(true);
        });

        $("#mysakai_more_change_layout").live("click", function() {
            // get title
            var title = $("#pagetitle").html();
            sakai.dashboard.changeLayout(title);
            showHideMoreMenu(true);
        });

        $(document).bind("click", function(e) {
            if (!($(e.target).is("#mysakai_more_menu") || $(e.target).parents("#mysakai_more_menu").length || $(e.target).is("#mysakai_more_link") || $(e.target).parents("#mysakai_more_link").length)) {
                showHideMoreMenu(true);
            }
        });
    };

    var initDashboard = function() {
        sakai.dashboard.init("/~" + sakai.data.me.user.userid + "/dashboardwidgets/", true, "personalportal", true);
        $("#mysakai_edit_page").show();
        addBindings();
    };

    /**
     * Init function for the mysakai page
     */
    var init = function(){
        // Initialise the entity widget
        $(window).bind("sakai.api.UI.entity.ready", function(e){
            sakai.api.UI.entity.render("myprofile", false);
        });

        // Insert the widget in before the changepic widget, we do this here to add in the userid dynamically and to get it in the first pass
        // of insertWidgets to reduce HTTP requests
        $("#widget_changepic").before(sakai.api.Security.saneHTML("<div id='widget_dashboard_mysakaidashboard_/~" + sakai.data.me.user.userid + "/dashboard/' class='widget_inline'></div>"));

        if (sakai.dashboard && sakai.dashboard.isReady) {
            initDashboard();
        } else {
            $(window).bind("sakai.dashboard.ready", function(e, tuid) {
                initDashboard();
            });
        }
        // If the user isn't logged in, redirect them to do so, as the dashboard is relevant
        // only when you're logged in
        $(window).bind("sakai.dashboard.notLoggedIn sakai.dashboard.notUsersDashboard", function(e) {
            document.location = sakai.config.URL.GATEWAY_URL;
        });

        // If the user is a member of Berkeley's College of Environmental Design, but not a participant of myBerkeley project,
        // redirect him to the participation explanation page
        if (!sakai.myberkeleysecurity.isMyBerkeleyParticipant()) {
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

    init();

};

sakai.api.Widgets.Container.registerForLoad("sakai.mysakai");