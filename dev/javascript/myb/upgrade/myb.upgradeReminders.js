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
 * Upgrades reminders from 0.1 to 0.2 by adding "sakai:required=true" to all required tasks and events.
 */
sakai.upgradeReminders = function() {

    var SEARCH_URL = "/var/_upgradeMyBerkeleyRemindersSearch0.1-0.2";
    var MAX_ITEMS = 1000;
    var dryRun = true;

    var requiredRemindersSearch = {
        "sakai:query-language": "xpath",
        "sakai:query-template": "//element(*)MetaData[@sling:resourceType='sakai/message' and @sakai:type='notice' and @sakai:category='reminder' and not(@sakai:required) ]",
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
            error: function(xhr) {
                console.log("POST failed. XHR response:" + xhr.responseText);
            },
            dataType: 'json'
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
                doBatchUpdate(data.results);
            },
            error: function(xhr) {
                console.log("GET failed. XHR response:" + xhr.responseText);
            }
        });
    };

    var doBatchUpdate = function(results) {
        var requests = [];
        $.each(results, function(index, row) {
            requests[requests.length] = {
                url : row["jcr:path"],
                method : "POST",
                parameters : { "sakai:required": true }
            };
        });
        if (requests.length > 0) {
            console.log("batch requests[] = ");
            console.dir(requests);

            if ( !dryRun ) {
                doPost(sakai.config.URL.BATCH, {
                    requests: $.toJSON(requests)
                }, function() {
                    console.log("Batch update succeeded!");
                    console.log("Doing another search for reminders to upgrade...");
                    runSearchAndUpgrade();
                });
            } else {
                cleanup();
            }
        } else {
            console.log("No more reminders to upgrade!");
            cleanup();
        }
    };

    var cleanup = function() {
        doPost(SEARCH_URL, {
            ":operation" : "delete"
        }, function() {
            console.log("Deleted search.");
        });
    };

    var doInit = function() {
        var querystring = new Querystring();
        if ( querystring.contains("dryRun") && querystring.get("dryRun") === "false") {
            dryRun = false;
        }
        $("#upgrade_reminders_button").live("click", function() {
            console.log("Running upgrade; dryRun=" + dryRun);
            cleanup();
            createSearch();
        });
    };

    doInit();
};

sakai.api.Widgets.Container.registerForLoad("sakai.upgradeReminders");