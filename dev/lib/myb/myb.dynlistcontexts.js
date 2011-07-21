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

define(["jquery", "sakai/sakai.api.core"], function($, sakai) {
    sakai_global.dynlistcontexts = function() {

        var parseDynamicListContexts = function(data) {
            var children = [];
            $.each(data, function(key, val) {
               var inner = data[key];
               if (inner.hasOwnProperty("sling:resourceType")) {
                   if (inner["sling:resourceType"] === "myberkeley/dynamicListContext") {
                       inner["name"] = key;
                       children.push(inner);
                   }
               }
            });
            return children;
        };
        
        /**
         * Load the available dynamic list contexts. They will look like so:
         * 
         * [{
         *    "name": "myb-cnr-grads-plant",
         *    "sling:resourceType": "myberkeley/dynamicListContext",
         *    "myb-clauses": [
         *       "/colleges/NAT RES/standings/grad/majors/PLANT BIOLOGY",
         *       "/colleges/NAT RES/standings/grad/majors/Microbiology",
         *       "/colleges/NAT RES/standings/grad/majors/AGRICULTURAL CHEM"
         *    ],
         *    "myb-filters": ["/student/*"]
         * }, ... ]
         */
        var loadDynamicListContexts = function() {
            var availableDynamicListContexts;
            sakai.api.Server.loadJSON("/var/myberkeley/dynamiclists.tidy.1.json", function(success, data) {
                var availableDynamicListContexts;
                if (success) {
                    availableDynamicListContexts = parseDynamicListContexts(data);
                } else {
                    availableDynamicListContexts = [];
                }
                sakai.data.me.dynamiclistcontexts = availableDynamicListContexts;
            });
        };
        
        loadDynamicListContexts();
    };
    
    sakai.api.Widgets.Container.registerForLoad("dynlistcontexts");
});
