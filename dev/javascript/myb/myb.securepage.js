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

/* Include this file in any html page to secure the whole page against non-MyBerkeley Participants. */

require(["jquery","sakai/sakai.api.core","myb/myb.api.core"], function($, sakai, myb) {

    sakai_global.mybsecurepage = function() {

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
            document.location = "/dev/explore.html";
            return false;
        };

        var doInit = function() {

          //HACK: You can disable redirection to 'not-a-participant' page by setting the global variable allowRedirectToParticipantPage = false
          var useRedirect = true;
          if (typeof(myb.api.security.allowRedirectToParticipantPage) !== 'undefined') {
            useRedirect = myb.api.security.allowRedirectToParticipantPage;
          }

          if (!isLoggedIn()) {
            if ( useRedirect ) {
              sakai.api.Security.sendToLogin();
            }
            return;
          }

          // If the user is a member of Berkeley's College of Environmental Design, but not a participant of myBerkeley project,
          // redirect him to the participation explanation page
          if (!myb.api.security.isMyBerkeleyParticipant() && useRedirect) {
            sendToNotAMyBerkeleyParticipantPage();
          }

        };

      doInit();
    };

    sakai.api.Widgets.Container.registerForLoad("mybsecurepage");

});
