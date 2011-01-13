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
sakai.upgradeDashboard = function() {

    var SEARCH_URL = "/var/_upgradeMyBerkeleyDashboardSettings0.1-0.2";
    var MAX_USER_SEARCH_RESULTS = 1000;
    var MYREMINDERS = "myreminders";
    var MYTASKS = "mytasks";
    var MYEVENTS = "myevents";

    var dryRun = true;

    var userSearch = {
        "sakai:query-language": "xpath",
        "sakai:query-template": "//element(*)MetaData[@sling:resourceType='sakai/user-home']",
        "sling:resourceType": "sakai/search",
        "sakai:propertyprovider" : "Node",
        "sakai:resultprocessor": "Node",
        "sakai:title" : "Search for users who need their dashboards upgraded"
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
        doPost(SEARCH_URL, userSearch,
              function() {
                  console.log("Created search successfully.");
                  runSearchAndUpgrade();
              });
    };

    var runSearchAndUpgrade = function() {
        $.ajax({
            url: SEARCH_URL + "?items=" + MAX_USER_SEARCH_RESULTS,
            cache: false,
            success: function(data) {
                checkDashboards(data.results);
            },
            error: function(xhr) {
                console.log("GET failed. XHR response:" + xhr.responseText);
            }
        });
    };

    var checkDashboards = function(results) {
        var requests = [];
        $.each(results, function(index, row) {

            requests[requests.length] = {
                url : row["jcr:path"] + "/dashboard/mysakaidashboard/dashboard.infinity.json",
                method : "GET",
                dataType : "json"
            };
        });
        if (requests.length > 0) {
            $.ajax({
                type: "POST",
                url: sakai.config.URL.BATCH,
                data: {
                    requests: $.toJSON(requests)
                },
                success: function(data) {
                    console.log("Updating dashboards...");
                    $.each(data.results, function(index, row) {
                        if ( row.body.length > 0 ) {
                            var rawObj = $.parseJSON(row.body);
                            console.log("Processing dashboard at url " + row.url);
                            var dashboard = sakai.api.Server.loadJSON.convertObjectToArray(rawObj, null, null);
                            processDashboard(dashboard, row.url.replace(".infinity.json", ""));
                        }
                    });
                    cleanup();
                },
                error: function(xhr) {
                    console.log("POST failed. XHR response:" + xhr.responseText);
                },
            dataType: 'json'}
                    );

        } else {
            console.log("No more user dashboards to upgrade!");
            cleanup();
        }
    };

    var processDashboard = function(dashboard, url) {
        sakai.api.Util.removeJCRObjects(dashboard);
        var hasMyTasks = false;
        var hasMyEvents = false;
        var hasChanged = false;
        var columnCount = 0;

        // remove the myreminders widget wherever it is found
        $.each(dashboard.columns, function(index, column) {
            if ( typeof column === "object" ) {
                for ( var i = 0; i < column.length; i++ ) {
                    if ( typeof column[i] === "object" ) {
                        columnCount++;
                        if ( column[i].name ) {
                            if ( column[i].name === MYREMINDERS ) {
                                column.splice(i, 1);
                                hasChanged = true;
                            } else if ( column[i].name && column[i].name === MYTASKS ) {
                                hasMyTasks = true;
                            } else if ( column[i].name && column[i].name === MYEVENTS ) {
                                hasMyEvents = true;
                            }
                        }
                    }
                }
            }
        });

        // add the mytasks and myevents widgets unless user already has 'em

        // put event widget in second column if it exists, otherwise first col
        if ( ! hasMyEvents ) {
            hasChanged = true;
            var targetCol = "column1";
            if ( dashboard.columns["column2"] ) {
                targetCol = "column2";
            }
            dashboard.columns[targetCol].splice(0, 0, {
                name : MYEVENTS,
                uid : "id" + Math.round(Math.random() * 10000000000),
                visible : "block"
            });
        }

        // put tasks widget in first column
        if ( ! hasMyTasks ) {
            hasChanged = true;
            dashboard.columns["column1"].splice(0, 0, {
                name : MYTASKS,
                uid : "id" + Math.round(Math.random() * 10000000000),
                visible : "block"
            });
        }

        if ( hasChanged ) {
            console.log("Dashboard modified at " + url);
            if ( ! dryRun ) {
                sakai.api.Server.saveJSON(url, dashboard, function() {
                    console.log("Saved dashboard for " + url);
                });
            } else {
                console.dir(dashboard);
            }
        } else {
            console.log("Unchanged dashboard at " + url);
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
        console.log("Running dashboard upgrade; dryRun=" + dryRun);
        cleanup();
        createSearch();
    };

    doInit();
};

sakai.api.Widgets.Container.registerForLoad("sakai.upgradeDashboard");