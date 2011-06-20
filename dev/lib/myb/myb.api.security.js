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
define(["jquery", "sakai/sakai.api.core"], function($, sakai) {

    var security = {};

    security.isUserAnAdviser = function() {
        return (sakai.data.me.dynamiclistcontexts &&
                sakai.data.me.dynamiclistcontexts.length > 0);
    };

    /**
     * Check if the user is a myBerkeley participant.
     * There could be CED members who can log in, but are not myBerkeley participants.
     * This function checks 'sakai.data.me.profile.myberkeley.elements.participant' property.
     * @return true if the user is a myBerkeley participant, false otherwise
     */
    security.isMyBerkeleyParticipant = function() {
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

    security.isCurrentUserAMember = function(groupid) {
        if(!groupid || typeof(groupid) !== "string") {
            return false;
        }
        return ($.inArray(groupid, sakai.data.me.user.subjects) !== -1);
    };

    return security;

});
