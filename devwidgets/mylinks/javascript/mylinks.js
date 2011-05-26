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

/*
 global $, Config, jQuery, sakai, sdata, fluid
 */

require(["jquery", "sakai/sakai.api.core", "myb/myb.api.core", "/devwidgets/mylinks/javascript/default-links.js"], function($, sakai, myb, defaultLinks) {

    sakai_global.mylinks = function (tuid) {

        // page elements
        var widgetContainer = $("#" + tuid);
        var accordionContainer = $("ul#accordion", widgetContainer);

        // data files and paths
        var linksDataPath = "/~" + sakai.data.me.user.userid + "/private/my_links";

        var userLinkData = defaultLinks.sections[defaultLinks.userSectionIndex];

        var saveUserList = function(updatedList, callback) {
            sakai.api.Server.saveJSON(linksDataPath, updatedList, callback);
        };

        var renderLinkList = function(data) {
            accordionContainer.html(sakai.api.Util.TemplateRenderer("accordion_template", data));
            setupAccordion();
        };

        var loadUserList = function() {
            sakai.api.Server.loadJSON(linksDataPath, function (success, data) {
                if (success) {
                    // merge the user's links with the default links
                    userLinkData = data;
                    defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;
                } else {
                    defaultLinks.sections[0].selected = true;
                }
                renderLinkList(defaultLinks);
            });
        };

        var setupEventHandlers = function() {
            $("#add-link-mode", widgetContainer).live("click", function() {
                $(".link_list").hide();
                $(".addedit_link_panel").show();
                $("#savelink-button").hide();
            });
            $("#cancel-button", widgetContainer).live("click", function() {
                cancelEditMode();
            });
            $("#addlink-button", widgetContainer).live("click", function() {
                validateLink();
            });
            $("#savelink-button", widgetContainer).live("click", function() {
                validateLink();
            });

        };

        var cancelEditMode = function() {
            $(".link_list").show();
            $(".addedit_link_panel").hide();
        };

        var validateLink = function() {
            var linkTitle = $("#link-title")[0].value;
            var linkUrl = $("#link-url")[0].value;
            var isValid = true;

            if (linkTitle.length === 0) {
                sakai.api.Util.notification.show("", "You must enter a link title.",
                        sakai.api.Util.notification.type.ERROR, false);
                isValid = false;
            }
            if (linkUrl.length === 0) {
                sakai.api.Util.notification.show("", "You must enter a link URL.",
                        sakai.api.Util.notification.type.ERROR, false);
                isValid = false;
            }

            if (isValid) {
                userLinkData.links[userLinkData.links.length] = {
                    "id"   : "",
                    "name" : linkTitle,
                    "url" : linkUrl,
                    "popup_description": linkTitle
                };
                defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;
                selectUserSection();

                debug.log("defaultLinks after update = ", defaultLinks);

                saveUserList(userLinkData, function(success) {
                    if (success) {
                        cancelEditMode();
                        renderLinkList(defaultLinks);
                    } else {
                        sakai.api.Util.notification.show("", "A server error occurred while trying to save your link.",
                                sakai.api.Util.notification.type.ERROR, false);
                    }
                });
            }
        };

        var selectUserSection = function () {
            for (var i = 0; i < defaultLinks.sections.length; i++) {
                defaultLinks.sections[i].selected = false;
            }
            defaultLinks.sections[defaultLinks.userSectionIndex].selected = true;
        };

        var setupAccordion = function() {
            $("#accordion > li > div", widgetContainer).click(function() {
                if (false === $(this).next().is(':visible')) {
                    $("#accordion div", widgetContainer).removeClass("selected");
                    $("#accordion div", widgetContainer).addClass("notSelected");
                    $("#accordion table", widgetContainer).slideUp(300);
                }

                $(this).next().slideToggle(300);
                $(this).removeClass("notSelected");
                $(this).addClass("selected");
            });

            // Add Google Analytics outbound links tracking
            $("ul#accordion td.link a", widgetContainer).click(function () {
                myb.api.google.recordOutboundLink(this, 'Outbound Links', $(this).attr('href'));
                return false;
            });

            $("#accordion table.accordion_opened", widgetContainer).show();
        };

        /**
         * Set up the widget
         * grab the users link data, then fire callback loadLinksList
         */
        var doInit = function() {
            loadUserList();
            setupEventHandlers();
        };

        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");

});
