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

var sakai = sakai || {};
sakai.myb = sakai.myb || {};
sakai.myb.api = sakai.myb.api || {};
sakai.myb.api.google = sakai.myb.api.google || {};

// This variable has to be global, see http://code.google.com/apis/analytics/docs/gaJS/gaJSApi_gaq.html
var _gaq = _gaq || [];

/**
 * Records an outbound link before leaving the current page
 * See http://code.google.com/apis/analytics/docs/tracking/asyncMigrationExamples.html#EventTracking ,
 * http://code.google.com/apis/analytics/docs/tracking/eventTrackerGuide.html
 * 
 * @param {Object} link Link DOM object (a), whose href attribute will be used.
 * @param {Object} category	The name you supply for the group of objects you want to track.
 * @param {Object} action A string that is uniquely paired with each category, and commonly used to define the type of user interaction for the web object (hint: put URL to track here).
 */
sakai.myb.api.google.recordOutboundLink = function(link, category, action) {
  try {
    _gaq.push(['_trackEvent', category,  action]);
    
	var tgt = $(link).attr("target");
	if(tgt === "_blank") {
		window.open(link.href);
	} else {
		setTimeout('document.location = "' + link.href + '"', 100);	
	}
		
  }catch(err){}
};
	
/////////////////////////////
// Initialization function //
/////////////////////////////
sakai.myb.api.google.doInit = function() {
	// Set your account ID here
	_gaq.push(['_setAccount', sakai.config.Tracking.GoogleAnalytics.WebPropertyID]);
	_gaq.push(['_trackPageview']);

	var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    
	// custom_config.js must be loaded before GA script, so we have to add GA as the last script
	var arrLen = document.getElementsByTagName('script').length;
	var s = document.getElementsByTagName('script')[arrLen-1]; s.parentNode.insertAfter(ga, s);
};
	
$(document).ready(function() {
	sakai.myb.api.google.doInit();
});