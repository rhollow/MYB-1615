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

    sakai_global.accountprovision = function (tuid, showSettings) { 
        var userIds = [];
        var userIdsData = {};

        function displayProvidedData(data) {
            $("#provisioned_data_container").html(sakai.api.Util.TemplateRenderer("provisioned_data_container_template", data));
            $("#new_user_confirm").show();
        };
        
        function displaySuccessfulProvision(data) {
            $("#added_accounts_container").html(sakai.api.Util.TemplateRenderer("added_accounts_container_template", data));
        }
        
        /**
         * Shows a general message on the top screen
         * @param {String} msg    the message you want to display
         * @param {Boolean} isError    true for error (red block)/false for normal message(green block)
         */
        function showGeneralMessage(msg, isError) {
            // Check whether to show an error type message or an information one
            var type = isError ? sakai.api.Util.notification.type.ERROR : sakai.api.Util.notification.type.INFORMATION;

            // Show the message to the user
            sakai.api.Util.notification.show("", msg, type);
        };

        function fetchUserIds() {
            userIds = $("#new_user_ids").val().split(/\s+/);
            userIdsData = $.map(userIds, function(v) {
                return {name:'userIds', value:v};
            });
        };

        function checkProvisionData(success, data) {
            if (success) {
                displayProvidedData(data);
            } else {
                showGeneralMessage("Unable to find user data", true);
            }
        };
        
        $("#btn_find_users").click(function() {
            fetchUserIds();
            sakai.api.Server.loadJSON("/system/myberkeley/personProvision.json",
                    checkProvisionData, userIdsData);
        });
        
        $("#btn_add_users").click(function() {
            fetchUserIds();
            $.ajax({
                url: "/system/myberkeley/personProvision",
                type: "POST",
                data: userIdsData,
                success: function(data){
                    displaySuccessfulProvision(data);
                },
                error: function(xhr, textStatus, thrownError){
                    showGeneralMessage("Unable to create the specified accounts: " + textStatus, true);
                }
            });
        });

    };

    sakai.api.Widgets.Container.registerForLoad("accountprovision");
});
