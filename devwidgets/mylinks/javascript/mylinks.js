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

    sakai_global.mylinks = function (tuid, showSettings) {

        // page elements
        var $elm_container = $("#" + tuid);

        // templates
        var mylinksListTemplate = "mylinks_list_template";

        // data files and paths
        var userLinks = "my_links";
        var linksDataNode = "/~" + sakai.data.me.user.userid + "/private/" + userLinks;

        // path for the default list of links to display in the widget
        var defaultLinksPath = "/var/defaults/mylinks/mylinks-defaults.json";

        /**
         * write the users links to JCR
         * @param {object} the current state of the users links list
        */
        var saveLinkList = function (updatedList) {
            sakai.api.Server.saveJSON(linksDataNode, updatedList);
        };

        var createLinkList = function (data) {
            // Add Google Analytics outbound links tracking
            $("div.mylinks_widget li.link a").click(function () {
                myb.api.google.recordOutboundLink(this, 'Outbound Links', $(this).attr('href'));
                return false;
            });
        };

        /**
         * retrieve default data, use that and save it back
         */
        var loadDefaultList = function () {
            $.ajax({
                url: defaultLinksPath,
                cache: false,
                dataType: "json",
                success: function(data){
                    var parsedData = data;
                    // create the link list from the default list and then save it back
                    createLinkList(parsedData);
                    // save the default link list back to the server
                    saveLinkList(parsedData);
                },
                error: function(xhr, textStatus, thrownError) {
                        //alert("An error has occured");
                }
            });
        };               
        
        var gotoMainMode = function() {            
            $(".link_list").show();
            $(".addedit_link_panel").hide();
            setupEventHandlers();
        }
        
        var gotoAddLinkMode = function() {            
            $(".link_list").hide();
            $(".addedit_link_panel").show();
            $("#editlink-button").hide();
            setupEventHandlers();            
        }
        
        var resetEventHandlers = function() {
            $("#add-link-mode").die();
            $("#cancel-button").die();
        }

        var setupEventHandlers = function() {   
            resetEventHandlers();         
            $("#add-link-mode").click(gotoAddLinkMode);
            $("#cancel-button").click(gotoMainMode);
        }

		var setupAccordion = function() {
			$("#accordion > li > div").click(function(){
    			if(false == $(this).next().is(':visible')) {
					$("#accordion div").removeClass("selected");
					$("#accordion div").addClass("notSelected");
        			$("#accordion table").slideUp(300);
    			}
    			$(this).next().slideToggle(300);
				$(this).removeClass("notSelected");
				$(this).addClass("selected");
			});
 
			$("#accordion table:eq(0)").show();
		}
		
        /**
         * Set up the widget
         * grab the users link data, then fire callback loadLinksList
         * @param {Boolean} whether the loadJSON got it's data
         * @param {Object} contains the users links record
         */

        var doInit = function() {
            sakai.api.Server.loadJSON(linksDataNode, function (success, data) {
                if (success) {
                    // load the users link list from their saved link data
                    createLinkList(data);
                } else {
                    // else retrieve default data, use that and save it back
                    loadDefaultList();
                }
            });
            setupEventHandlers();
            setupAccordion();
        };

        doInit();

    };
    
    sakai.api.Widgets.widgetLoader.informOnLoad("mylinks");

});
