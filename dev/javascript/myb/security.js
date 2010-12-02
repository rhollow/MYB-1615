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

    // redefine some core Sakai API functions

    sakai.api = sakai.api || {};
    sakai.api.Security = sakai.api.Security || {};

    /**
     * myberkeley custom function: Checks if the user is logged in
     * @return true if the user is logged in, false otherwise
     */
    sakai.api.Security.isLoggedIn = function(){
        var person = sakai.data.me;
        var uuid = person.user.userid;
        if (!uuid || person.user.anon) {
            return false;
        }
        return true;
    };

    /**
     * myberkeley custom function: Check if the user is a myBerkeley participant.
     * There could be CED members who can log in, but are not myBerkeley participants.
     * This function checks 'sakai.data.me.profile.myberkeley.elements.participant' property.
     * @return true if the user is a myBerkeley participant, false otherwise
     */
    sakai.api.Security.isMyBerkeleyParticipant = function(){
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

    /**
     * myberkeley custom function: Function that can be called by pages that don't have the permission to show the content
     * they should be showing because the user in not a myBerkeley participant
     */
    sakai.api.Security.sendToNotAMyBerkeleyParticipantPage = function(){
        var redurl = window.location.pathname + window.location.hash;
        document.location = "/dev/403_not_a_participant.html?redurl=" + escape(window.location.pathname + window.location.search + window.location.hash);
        return false;
    };

    var doInit = function() {
        // Check if the user is logged in before loading data
       var security = sakai.api.Security;
       if (!security.isLoggedIn()) {
           security.sendToLogin();
           return;
       }

       // If the user is a member of Berkeley's College of Environmental Design, but not a participant of myBerkeley project,
       // redirect him to the participation explanation page
       if (!security.isMyBerkeleyParticipant()) {
           security.sendToNotAMyBerkeleyParticipantPage();
       } 
    };

    doInit();

};

sakai.api.Widgets.Container.registerForLoad("sakai.myberkeleysecurity");
