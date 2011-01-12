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
sakai.upgradeReminders = function() {

    var SEARCH_URL = "/var/_upgradeMyBerkeleyRemindersSearch0.1-0.2";
    var MAX_ITEMS = 2;

    var requiredRemindersSearch = {
        "sakai:query-language": "xpath",
        "sakai:query-template": "//element(*)MetaData[@sling:resourceType='sakai/message' and @sakai:type='notice' and @sakai:category='reminder' ]",
        "sling:resourceType": "sakai/search",
        "sakai:propertyprovider" : "Message",
        "sakai:resultprocessor": "Message",
        "sakai:title" : "Search for required reminders used by one-time upgrade script"
    };

    var doPost = function(url, props, callback) {
        console.log("POST to url " + url);
        console.dir(props);
        $.ajax({
            type: "POST",
            url: url,
            data: props,
            success: function() {
                if ($.isFunction(callback)) {
                    callback();
                }
            },
            error: function(xhr, textStatus, thrownError) {
                console.log("POST failed. XHR response:" + xhr.responseText);
            },
            dataType: 'json'
        });
    };

    var deleteSearch = function() {
        doPost(SEARCH_URL, {
            ":operation" : "delete"
        }, function() {
            console.log("Successfully deleted search.");
        });
    };

    var createSearch = function() {
        doPost(SEARCH_URL, requiredRemindersSearch,
              function() {
                  console.log("Created search successfully.");
                  runSearchAndUpgrade();
              });
    };

    var runSearchAndUpgrade = function() {
        $.ajax({
            url: SEARCH_URL + "?items=" + MAX_ITEMS,
            cache: false,
            success: function(data) {
                $.each(data.results, function(index, row) {
                    console.log("Got result at " + row["jcr:path"]);

                });
                cleanup();
            },
            error: function(xhr, textStatus, thrownError) {
                console.log("GET failed. XHR response:" + xhr.responseText);
            }
        });
    };

    var cleanup = function() {
        deleteSearch();
    };

    var doInit = function() {
        cleanup();
        createSearch();
    };

    doInit();
};

sakai.api.Widgets.Container.registerForLoad("sakai.upgradeReminders");