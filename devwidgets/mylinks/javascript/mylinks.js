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
        var linkTitleInput = $("#link-title", widgetContainer);
        var linkUrlInput = $("#link-url", widgetContainer);
        var linkList = $(".link_list", widgetContainer);
        var addEditPanel = $(".addedit_link_panel", widgetContainer);
        var saveLinkButton = $("#savelink-button", widgetContainer);
        var addLinkButton = $("#addlink-button", widgetContainer);
        var addEditPanelTitle = $(".addedit_link_panel_title", widgetContainer);
        var accordionContainer = $("#accordion", widgetContainer);

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
                            url : true
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
                    defaultLinks.sections[0].selected = true;
                }
                renderLinkList(defaultLinks);
            });
        };
        
        
        var setAddEditLinkTitle = function (title) {
            addEditPanelTitle.text(title);
        };

        var cancelEditMode = function() {
            
            currentLinkIndex = null;
            //linkList.show();
            addEditPanel.hide();
            linkTitleInput.attr("value","").removeClass("error");
            linkUrlInput.attr("value","").removeClass("error");
            $("label.error").hide();
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
            var isValid = validator.form();
            if (isValid) {
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
            $("#add-link-mode", widgetContainer).click(function() {
                enterAddMode();
            });
            $("#cancel-button", widgetContainer).click(function() {
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
            for (var i = 0; i < userLinkData.links.length; i++) {

                // edit button
                var editIcon = $("#mylinks_edit_" + i, widgetContainer);
                editIcon.click(function() {
                    var idx = this.id.replace("mylinks_edit_", "");
                    enterEditMode(idx);
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
        
        var showPane = function (pane) {
            if (!pane.hasClass("accordion_open")) {
                closePanes();
                pane.addClass("accordion_open");
            }
            pane.children(".accordion_content").slideDown(300, function(){
                pane.children(".accordion_content").css("overflow","auto");
            });
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

            $(".section_label", accordionContainer).click(function() {
                showPane($(this).parent());
            });

            // Add Google Analytics outbound links tracking
            $("li.link a", accordionContainer).click(function () {
                myb.api.google.recordOutboundLink(this, 'Outbound Links', $(this).attr('href'));
                return false;
            });

            if (accordionContainer.width() > 250) {
                accordionContainer.addClass("link_grid")
            }
            
            showPane($(".accordion_open", accordionContainer));
            
        };

        var doInit = function() {
            console.log("beep");
            loadUserList();
            setupEventHandlers();
        };

        doInit();

    };

    sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");

});
