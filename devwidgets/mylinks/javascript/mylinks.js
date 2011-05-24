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

require(["jquery", "sakai/sakai.api.core", "myb/myb.api.core"], function($, sakai, myb) {

  sakai_global.mylinks = function (tuid) {

    // page elements
    var widgetContainer = $("#" + tuid);
    var accordionContainer = $("#accordion");

    // data files and paths
    var linksDataPath = "/~" + sakai.data.me.user.userid + "/private/my_links";

    // path for the default list of links to display in the widget
    var defaultLinksPath = "/devwidgets/mylinks/default-links.json";

    /**
     * write the users links to JCR
     */
    var saveLinkList = function (updatedList) {
      sakai.api.Server.saveJSON(linksDataPath, updatedList);
    };

    var renderLinkList = function (data) {
      debug.log(data);
      accordionContainer.html(sakai.api.Util.TemplateRenderer("accordion_template", { sections : data }));

      // Add Google Analytics outbound links tracking
      $("div.mylinks_widget li.link a").click(function () {
        myb.api.google.recordOutboundLink(this, 'Outbound Links', $(this).attr('href'));
        return false;
      });


      setupAccordion();
    };

    /**
     * retrieve default data, use that and save it back
     */
    var loadDefaultList = function () {
      $.ajax({
                url: defaultLinksPath,
                cache: false,
                dataType: "json",
                success: function(data) {
                  renderLinkList(data);
                  // save the default link list back to the server
                  //saveLinkList(parsedData);
                },
                error: function() {
                  sakai.api.Util.notification.show("", translate("AN_ERROR_OCCURRED_CONTACTING_THE_SERVER"),
                          sakai.api.Util.notification.type.ERROR, false);
                  debug.error("Got error contacting the server.");
                }
              });
    };

    var gotoMainMode = function() {
      $(".link_list").show();
      $(".addedit_link_panel").hide();
      setupEventHandlers();
    };

    var gotoAddLinkMode = function() {
      $(".link_list").hide();
      $(".addedit_link_panel").show();
      $("#editlink-button").hide();
      setupEventHandlers();
    };

    var gotoModifyMode = function() {
      $(".edit").css("display", "block");
      $("#modify-links-mode").css("display", "none");
      $("#normal-mode").css("display", "block");
    };

    var gotoNormalMode = function() {
      $(".edit").css("display", "hidden");
      $("#normal-mode").css("display", "none");
      $("#modify-links-mode").css("display", "block");
    };

    var resetEventHandlers = function() {
      $("#add-link-mode").die();
      $("#cancel-button").die();
    };

    var setupEventHandlers = function() {
      resetEventHandlers();
      $("#add-link-mode").click(gotoAddLinkMode);
      $("#modify-links-mode").click(gotoModifyMode);
      $("#normal-mode").click(gotoNormalMode);
      $("#cancel-button").click(gotoMainMode);
    };

    var setupAccordion = function() {
      $("#accordion > li > div").click(function() {
        if (false === $(this).next().is(':visible')) {
          $("#accordion div").removeClass("selected");
          $("#accordion div").addClass("notSelected");
          $("#accordion table").slideUp(300);
        }

        $(this).next().slideToggle(300);
        $(this).removeClass("notSelected");
        $(this).addClass("selected");
      });

      $("#accordion table.accordion_opened").show();
    };

    var translate = function(key) {
      return sakai.api.i18n.Widgets.getValueForKey("mylinks", "default", key);
    };

    /**
     * Set up the widget
     * grab the users link data, then fire callback loadLinksList
     */

    var doInit = function() {
      sakai.api.Server.loadJSON(linksDataPath, function (success) {
        if (success) {
          // load the users link list from their saved link data
          //createLinkList(data);
          loadDefaultList();
        } else {
          // else retrieve default data, use that and save it back
          loadDefaultList();
        }
      });
      setupEventHandlers();
    };

    doInit();

  };

  sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");

});
