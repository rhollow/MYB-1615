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
 global $, jQuery, sakai, myb
 */

require(["jquery", "sakai/sakai.api.core", "myb/myb.api.core", "/devwidgets/mylinks/javascript/default-links.js"], function($, sakai, myb, defaultLinks) {

   /**
     * @name sakai_global.mylinks
     *
     * @class mylinks
     *
     * @description
     * The 'mylinks' widget displays "Quicklinks" a set of campus links  
     * and allows the user to add thier own links to the My Links pane
     *
     * @version 0.0.1
     * @param {String} tuid Unique id of the widget
     */
    sakai_global.mylinks = function (tuid) {

        // DOM elements
        var widgetContainer = $("#" + tuid);
        var linkTitleInput = $("#link-title", widgetContainer);
        var linkUrlInput = $("#link-url", widgetContainer);
        var addEditPanel = $(".addedit_link_panel", widgetContainer);
        var saveLinkButton = $("#savelink-button", widgetContainer);
        var addLinkButton = $("#addlink-button", widgetContainer);
        var addEditPanelTitle = $(".addedit_link_panel_title", widgetContainer);
        var accordionContainer = $("#accordion", widgetContainer);
        var $addLinkModeBtn = $("#add-link-mode", widgetContainer);
        var $cancelButton = $("#cancel-button", widgetContainer);

        // index of the link currently being edited
        var currentLinkIndex = null;

        // data files and paths
        var linksDataPath = "/~" + sakai.data.me.user.userid + "/private/my_links";

        // the user's own links ("My Links")
        var userLinkData = defaultLinks.sections[defaultLinks.userSectionIndex];

        var validator = $("#mylinks-form").validate({
            rules : {
                "link-title" : {
                    required: true
                },
                "link-url" : {
                    required : true,
                    url : true,
                    appendhttp : true
                }
            }
        });

        var renderLinkList = function(data) {
            accordionContainer.html(sakai.api.Util.TemplateRenderer("accordion_template", data));
            setupAccordion();
            setupEditIcons();
        };

        /*
         Load user's list if there is one, and merge it with the default links. If no user's list, just use
         the default ones.
         */
        var loadUserList = function() {
            sakai.api.Server.loadJSON(linksDataPath + ".2.json", function (success, data) {
                if (success) {
                    // merge the user's links with the default links
                    userLinkData = data;
                    defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;
                } else {
                    userLinkData.activeSection = 0;
                }
                defaultLinks.activeSection = userLinkData.activeSection;
                renderLinkList(defaultLinks);
            });
        };


        var setAddEditLinkTitle = function (title) {
            addEditPanelTitle.text(title);
        };

        var cancelEditMode = function() {
            validator.resetForm();
            currentLinkIndex = null;
            addEditPanel.hide();
            linkTitleInput.attr("value", "").removeClass("error");
            linkUrlInput.attr("value", "").removeClass("error");
            $("label.error").hide();
            addLinkButton.removeClass("disabled");
            saveLinkButton.removeClass("disabled");
        };

        var enterAddMode = function() {
            showPane($(".accordion_pane:last"));
            setAddEditLinkTitle("Add link");
            currentLinkIndex = null;
            addEditPanel.show();
            addLinkButton.show();
            saveLinkButton.hide();
            linkTitleInput[0].focus();
        };

        var enterEditMode = function(index) {
            var link = userLinkData.links[index];
            setAddEditLinkTitle("Edit link");
            linkTitleInput[0].value = link.name;
            linkUrlInput[0].value = link.url;
            currentLinkIndex = index;
            addEditPanel.show();
            addLinkButton.hide();
            saveLinkButton.show();
            linkTitleInput[0].focus();
        };

        var saveLink = function() {
            if ( addLinkButton.is(".disabled")) {
                return;
            }
            var isValid = validator.form();
            if (isValid) {
                addLinkButton.addClass("disabled");
                saveLinkButton.addClass("disabled");
                var index = currentLinkIndex;
                if (index === null) {
                    index = userLinkData.links.length;
                }
                userLinkData.links[index] = {
                    "name" : linkTitleInput[0].value,
                    "url" : linkUrlInput[0].value,
                    "popup_description": linkTitleInput[0].value
                };
                defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;

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

        var setupEventHandlers = function() {
            
            $addLinkModeBtn.on("click", function() {
                enterAddMode();
            });
            
            $cancelButton.on("click", function() {
                cancelEditMode();
            });
            
            addLinkButton.click(function() {
                saveLink();
            });
            
            saveLinkButton.click(function() {
                saveLink();
            });
            linkTitleInput.keydown(function(event) {
                if (event.keyCode === 13) {
                    event.preventDefault();
                    saveLink();
                }
            });
            
            linkUrlInput.keydown(function(event) {
                if (event.keyCode === 13) {
                    event.preventDefault();
                    saveLink();
                }
            });
            
        };

        var setupEditIcons = function() {
            $(".myb-edit-mylink", widgetContainer).on("click", function() {
                var idx = this.id.replace("mylinks_edit_", "");
                enterEditMode(idx);
            });
            
            $(".myb-delete-mylink", widgetContainer).on("click", function() {
                var idx = this.id.replace("mylinks_delete_", "");
                userLinkData.links.splice(idx, 1);
                defaultLinks.sections[defaultLinks.userSectionIndex] = userLinkData;
                sakai.api.Server.saveJSON(linksDataPath, userLinkData, function(success) {
                    if (success) {
                        renderLinkList(defaultLinks);
                    } else {
                        sakai.api.Util.notification.show("", "A server error occurred while trying to delete your link.", sakai.api.Util.notification.type.ERROR, false);
                    }
                }, true);
            });            
        };

        var showPane = function (pane) {
            if (!pane.hasClass("accordion_open")) {
                closePanes();
                pane.addClass("accordion_open");
            }
            pane.children(".accordion_content").slideDown(300, function() {
                pane.children(".accordion_content").css("overflow", "auto");
            });

            var previousActiveSection = userLinkData.activeSection;
            if ( typeof pane[0] != 'undefined') {
                userLinkData.activeSection = pane[0].id.replace("mylinks_section_", "");
            } else {
                userLinkData.activeSection = 0;
            }
            defaultLinks.activeSection = userLinkData.activeSection;
            if (previousActiveSection != userLinkData.activeSection) {
                // save active section only if it's different
                sakai.api.Server.saveJSON(linksDataPath, userLinkData);
            }
        };

        var hidePane = function (pane) {
            pane.removeClass("accordion_open");
            pane.children(".accordion_content").slideUp(300);
        };

        var closePanes = function () {
            $(".accordion_pane", accordionContainer).each(function () {
                hidePane($(this));
            });
        };

        var setupAccordion = function() {

            $(".section_label", accordionContainer).on("click", function() {
                showPane($(this).parent());
            });

            // Add Google Analytics outbound links tracking
            $("li.link a", accordionContainer).on("click", function () {
                myb.api.google.recordOutboundLink(this, 'Outbound Links', $(this).attr('href'));
                return false;
            });

            if (accordionContainer.width() > 250) {
                accordionContainer.addClass("link_grid")
            }

            showPane($(".accordion_open", accordionContainer));

        };

        var doInit = function() {
            loadUserList();
            setupEventHandlers();
        };

        // run doInit() function when sakai_global.mylinks object loads
        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");

});
