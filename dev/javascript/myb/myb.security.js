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

/* global $, Config, opensocial */

var sakai = sakai || {};
sakai.myberkeleysecurity = function(){

    var groupCEDAdvisors = "g-ced-advisors"; // CED Advisors group ID

    sakai.myberkeleysecurity.isUserAnAdvisor = function() {
        return isCurrentUserAMember(groupCEDAdvisors);
    };

    var isCurrentUserAMember = function(groupid) {
        if(!groupid || typeof(groupid) !== "string") {
            return false;
        }
        return ($.inArray(groupid, sakai.data.me.user.subjects) !== -1);
    };
        
    /**
     * Checks if the user is logged in
     * @return true if the user is logged in, false otherwise
     */
    var isLoggedIn = function(){
        var person = sakai.data.me;
        var uuid = person.user.userid;
        return !(!uuid || person.user.anon);
    };

    /**
	* Function that can be called by pages that don't have the permission to show the content
	* they should be showing because the user in not a myBerkeley participant
	*/
	var sendToNotAMyBerkeleyParticipantPage = function() {		
		document.location = sakai.config.URL.MY_DASHBOARD_URL;
		return false;
	};

    var doInit = function() {
       if (!isLoggedIn()) {
           sakai.api.Security.sendToLogin();
           return;
       }
		
		//HACK: You can disable redirection to 'not-a-participant' page by setting the global variable allowRedirectToParticipantPage = false
		var useRedirect = true;
		if(typeof(sakai.myberkeleysecurity.allowRedirectToParticipantPage) !== 'undefined'){
			useRedirect = sakai.myberkeleysecurity.allowRedirectToParticipantPage; 
		}
       // If the user is a member of Berkeley's College of Environmental Design, but not a participant of myBerkeley project,
       // redirect him to the participation explanation page
       if (!sakai.myberkeleysecurity.isMyBerkeleyParticipant() && useRedirect) {
           sendToNotAMyBerkeleyParticipantPage();
       }
    };

    doInit();

};

/**
 * Check if the user is a myBerkeley participant.
 * There could be CED members who can log in, but are not myBerkeley participants.
 * This function checks 'sakai.data.me.profile.myberkeley.elements.participant' property.
 * @return true if the user is a myBerkeley participant, false otherwise
 */
sakai.myberkeleysecurity.isMyBerkeleyParticipant = function() {
    try {
        if (sakai.data.me.profile.myberkeley.elements.participant &&
            sakai.data.me.profile.myberkeley.elements.participant.value === "true") {
            return true;
        }
    } catch(ex) {
        // Ignoring the exception
    }

    return false;
};


sakai.api.Widgets.Container.registerForLoad("sakai.myberkeleysecurity");
