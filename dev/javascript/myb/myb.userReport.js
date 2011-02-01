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
/*global $, Config, opensocial */

var sakai = sakai || {};

/**
 * Upgrades user dashboards from 0.1 to 0.2 by removing the old "myreminders" widget and putting
 * "myevents" and "mytasks" in its place.
 */
sakai.userReport = function() {

    var SEARCH_URL = "/var/_myb_user_report";
    var MAX_USER_SEARCH_RESULTS = 1000;

	// curl -u 271592:testuser -Fvalue=2011-01-23T00:00:00-08:00 -Fvalue@TypeHint=date 
	// http://localhost:8080/~271592/public/authprofile/myberkeley/elements/joinDate
    var userSearch = {
        "sakai:query-language": "xpath",
        "sakai:query-template": "/jcr:root//*/public/authprofile/myberkeley/elements/joinDate",
        "sling:resourceType": "sakai/search",
        "sakai:propertyprovider" : "Node",
        "sakai:resultprocessor": "Node",
        "sakai:title" : "Search for users"
    };

    var doPost = function(url, props, callback) {
        $.ajax({
            type: "POST",
            url: url,
            data: props,
            success: function() {
                if ($.isFunction(callback)) {
                    callback();
                }
            },
            error: function(xhr) {
                console.log("POST to url " + url + " failed. XHR response:" + xhr.responseText);
            },
            dataType: 'json'
        });
    };

    var createSearch = function() {
        doPost(SEARCH_URL, userSearch,
              function() {
                  console.log("Created search successfully.");
                  runSearch();
              });
    };

    var runSearch = function() {
        $.ajax({
            url: SEARCH_URL + "?items=" + MAX_USER_SEARCH_RESULTS,
            cache: false,
            success: function(data) {
                console.dir(data.results);
                cleanup();
            },
            error: function(xhr) {
                console.log("GET failed. XHR response:" + xhr.responseText);
            }
        });
    };

    var cleanup = function() {
        doPost(SEARCH_URL, {
            ":operation" : "delete"
        }, function() {
            console.log("Deleted search.");
        });
    };

    var doInit = function() {
        createSearch();
    };

    doInit();
};

sakai.api.Widgets.Container.registerForLoad("sakai.userReport");