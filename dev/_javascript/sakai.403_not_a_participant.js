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

var sakai = sakai || {};
sakai.not_a_participant = function(tuid, showSettings) {
    
    var permissionsErrorLoggedInTemplate = "permission_error_not_a_participant_template";
    var permissionsError = ".permissions_error";    
	var errorReportEmailSubject = "myBerkeley participation is required for accessing a web page"; //default e-mail subject

    var doInit = function(){
		var querystring = new Querystring();
        var redurl = window.location.pathname + window.location.hash;
        // Parameter that indicates which page to redirect to.
		if (querystring.contains("redurl")){
            redurl = querystring.get("redurl");
			
			// For error reporting			
			if (redurl.length > 0) {
				errorReportEmailSubject = "myBerkeley participation is required for accessing '" + redurl + "' page";
			}
        }
		
		var subject = {
				"subject": encodeURIComponent(errorReportEmailSubject)
				};
		
        // the user is logged in and should get a page in Sakai itself
        var renderedTemplate = $.TemplateRenderer(permissionsErrorLoggedInTemplate, subject).replace(/\r/g, '');
		
        $(permissionsError).append(renderedTemplate);
        $("#permission_error").addClass("error_page_bringdown");
    }

    doInit();

};
sakai.api.Widgets.Container.registerForLoad("sakai.not_a_participant");