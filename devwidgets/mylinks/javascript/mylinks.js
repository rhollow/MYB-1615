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
        var linkTitleInput = $("#link-title", widgetContainer)[0];
        var linkUrlInput = $("#link-url", widgetContainer)[0];
        var linkList = $(".link_list", widgetContainer);
        var addEditPanel = $(".addedit_link_panel", widgetContainer);
        var saveLinkButton = $("#savelink-button", widgetContainer);
        var addLinkButton = $("#addlink-button", widgetContainer);

        // index of the link currently being edited
        var currentLinkIndex = null;

        // data files and paths
        var linksDataPath = "/~" + sakai.data.me.user.userid + "/private/my_links";

        // the user's own links ("My Links")
        var userLinkData = defaultLinks.sections[defaultLinks.userSectionIndex];

        var renderLinkList = function(data) {
            accordionContainer.html(sakai.api.Util.TemplateRenderer("accordion_template", data));
            setupAccordion();
            setupEditIcons();
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

        var cancelEditMode = function() {
            currentLinkIndex = null;
            linkList.show();
            addEditPanel.hide();
            linkTitleInput.value = "";
            linkUrlInput.value = "";
        };

        var enterAddMode = function() {
            currentLinkIndex = null;
            linkList.hide();
            addEditPanel.show();
            addLinkButton.show();
            saveLinkButton.hide();
            linkTitleInput.focus();
        };

        var enterEditMode = function(index, link) {
            linkTitleInput.value = link.name;
            linkUrlInput.value = link.url;
            currentLinkIndex = index;
            linkList.hide();
            addEditPanel.show();
            addLinkButton.hide();
            saveLinkButton.show();
            linkTitleInput.focus();
        };

        var saveLink = function() {
            var isValid = true;

            if (linkTitleInput.value.length === 0) {
                sakai.api.Util.notification.show("", "You must enter a link title.",
                        sakai.api.Util.notification.type.ERROR, false);
                isValid = false;
            } else if (linkUrlInput.value.length === 0) {
                sakai.api.Util.notification.show("", "You must enter a link URL.",
                        sakai.api.Util.notification.type.ERROR, false);
                isValid = false;
            }

            if (isValid) {
                var index = currentLinkIndex;
                if (index === null) {
                    index = userLinkData.links.length;
                }
                userLinkData.links[index] = {
                    "name" : linkTitleInput.value,
                    "url" : linkUrlInput.value,
                    "popup_description": linkTitleInput.value
                };
                defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;
                selectUserSection();

                sakai.api.Server.saveJSON(linksDataPath, userLinkData, function(success) {
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

        var setupEventHandlers = function() {
            $("#add-link-mode", widgetContainer).live("click", function() {
                enterAddMode();
            });
            $("#cancel-button", widgetContainer).live("click", function() {
                cancelEditMode();
            });
            addLinkButton.live("click", function() {
                saveLink();
            });
            saveLinkButton.live("click", function() {
                saveLink();
            });
        };

        var setupEditIcons = function() {
            for (var i = 0; i < userLinkData.links.length; i++) {

                // edit button
                var editIcon = $("#mylinks_edit_" + i, widgetContainer);
                editIcon.click(function() {
                    var idx = this.id.replace("mylinks_edit_", "");
                    enterEditMode(idx, userLinkData.links[idx]);
                });

                // delete button
                var deleteIcon = $("#mylinks_delete_" + i, widgetContainer);
                deleteIcon.click(function() {
                    var idx = this.id.replace("mylinks_delete_", "");
                    userLinkData.links.splice(idx, 1);
                    defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;
                    sakai.api.Server.saveJSON(linksDataPath, userLinkData, function(success) {
                        if (success) {
                            renderLinkList(defaultLinks);
                        } else {
                            sakai.api.Util.notification.show("", "A server error occurred while trying to delete your link.",
                                    sakai.api.Util.notification.type.ERROR, false);
                        }
                    }, true);
                });
            }
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
