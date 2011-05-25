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

require(["jquery", "sakai/sakai.api.core", "myb/myb.api.core", "/devwidgets/mylinks/default-links.js"], function($, sakai, myb, defaultLinks) {

    sakai_global.mylinks = function (tuid) {

        // page elements
        var widgetContainer = $("#" + tuid);
        var accordionContainer = $("ul#accordion", widgetContainer);

        // data files and paths
        var linksDataPath = "/~" + sakai.data.me.user.userid + "/private/my_links";

        var userLinkData = defaultLinks.sections[defaultLinks.userSectionIndex];

        var saveUserList = function(updatedList) {
            sakai.api.Server.saveJSON(linksDataPath, updatedList);
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
                }
                renderLinkList(defaultLinks);
            });
        };

        var setupEventHandlers = function() {
            $("#add-link-mode", widgetContainer).live("click", function() {
                $(".link_list").hide();
                $(".addedit_link_panel").show();
                $("#editlink-button").hide();
            });
            $("#cancel-button", widgetContainer).live("click", function() {
                $(".link_list").show();
                $(".addedit_link_panel").hide();
            });
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
