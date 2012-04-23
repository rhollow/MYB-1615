/*
 * Licensed to the Sakai Foundation (SF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The SF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */


// load the master sakai object to access all Sakai OAE API methods
require(['jquery', 'sakai/sakai.api.core'], function($, sakai) {

    /**
     * @name sakai.walktime
     *
     * @class walktime
     *
     * @description
     * Estimate time/distance between campus buildings.
     *
     * @version 0.0.1
     * @param {String} tuid Unique id of the widget
     * @param {Boolean} showSettings Show the settings of the widget or not
     */
    sakai_global.walktime = function (tuid, showSettings) {

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////
        var $rootel = $('#' + tuid);  // unique container for each widget instance
        var $mainContainer = $('#walktime_main', $rootel);
        var $resultsDisplayContainer = $('#walktime_response', $rootel);
        var $startPoint = $('#walktime_startpoint', $rootel);
        var $endPoint = $('#walktime_endpoint', $rootel);
        var $distanceText = $('#walktime_distance_text', $rootel);
        var $minutesText = $('#walktime_minutes_text', $rootel);

        // Default position is Doe Library
        var defaultCoords = [37.87243,-122.25955,37.87243,-122.25955];

        ///////////////////////
        // Utility functions //
        ///////////////////////

        // If all four coords are available, calculate distance
        var findDistance = function(coords) {

            if (coords && coords.length === 4) {
                // Get vals out of coords array and cast strings to floats
                lat1 = parseFloat(coords[0]);
                lon1 = parseFloat(coords[1]);
                lat2 = parseFloat(coords[2]);
                lon2 = parseFloat(coords[3]);

                var R = 6371; // Radius of the earth in km
                var dLat = setToRad(lat2-lat1);  // Javascript functions in radians
                var dLon = setToRad(lon2-lon1);

                var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
                    Math.cos(setToRad(lat1)) * Math.cos(setToRad(lat2)) *
                    Math.sin(dLon/2) * Math.sin(dLon/2);
                var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
                var d = R * c * 1000; // Distance in meters
                var distance = Math.round(d);

                return distance;
            };
        };


        var metersToMinutes = function(meters) {
            // Assuming avg walking speed of 5 foot/second = 91 meter/minute.
            // But you can never get there as the crow flies, so add 20% padding.

            meters = meters * 1.2;
            var minutes = Math.round(parseInt(meters, 10)/91);
            return minutes;
        };



        var setToRad = function(thenum) {
            // Convert numeric degrees to radians
            return thenum * Math.PI / 180;
        };        


        /////////////////////////
        // Main View functions //
        /////////////////////////

        var showMainView = function() {
            $mainContainer.show();
        };

        // Convert all currently selected coords into a single array
        var getStartEndPoints = function() {

            var startPoint = $startPoint.val().split(',');
            var endPoint = $endPoint.val().split(',');

            return [startPoint[0], startPoint[1], endPoint[0], endPoint[1]];
        };

        // Update all display text by obtaining coords and calculating distance.
        var updateDisplayStrings = function(startfinish) {

            var coords = getStartEndPoints();
            var distance = findDistance(coords);

            if (startfinish === 'start') {
                var selectlist = $startPoint;
            } else {
                var selectlist = $endPoint;
            };

            // First use regex to add commas to long numeric strings
            var distanceString = distance.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
            $distanceText.text(distanceString);
            $minutesText.text(metersToMinutes(distance));

            // Only show the response text if we have an actual distance
            if(isNaN(distance)){
                $resultsDisplayContainer.hide();
            } else {
                $resultsDisplayContainer.show();
            };
        };

        // Determine optimum zoom level for a distance between two points,
        // so we can always show start and end points simultaneously on large map.
        var getZoom = function(coords) {
            var distance = findDistance(coords);
            var zoom;

            if (distance >= 1000) {
                zoom = 16;
            } else if (distance > 500 && distance < 1000 ) {
                zoom = 17;
            } else {
                zoom = 18;
            };
            return zoom;
        };

        ////////////////////
        // Event Handlers //
        ////////////////////

        // Update displayed strings when start or end selections change
        var startUpdate = function(startend) {
            var coords = getStartEndPoints();
            var zoom = getZoom(coords);

            // Don't fire the map update if we don't have an end point (else we end up in the ocean!)
            if (coords[3] !== undefined) {
                updateDisplayStrings(startend);
                updateLargeMapLink(coords,zoom);
            }
        };

        $startPoint.on('change', {startend: 'start'}, startUpdate);
        $endPoint.on('change', {startend: 'end'}, startUpdate);


        // Update linked map image and footer URL on load or when coords change
        var updateLargeMapLink = function(coords,zoom) {
            var goToURL = 'http://maps.google.com/maps?saddr='+coords[0]+','+coords[1]+'&daddr='+coords[2]+','+coords[3]+'&l=en&dirflg=w&t=m&z='+zoom;
            $('.walktime_item_link').attr({href : goToURL});

            $('#walktime_map_link').on('click',function(event){
                event.preventDefault();
                window.open(this.href,'map_large');
            });

            $('#walktime_footermap_link').on('click',function(event){
                event.preventDefault();
                window.open(this.href,'map_large');
            });

            // Using the static maps image generator in the Google Maps API instead of iframe
            var imgURL = 'http://maps.googleapis.com/maps/api/staticmap?center='+coords[2]+','+coords[3]+'&zoom=16&size=200x200&maptype=roadmap&markers=color:blue%7C'+coords[2]+','+coords[3]+'&sensor=false';
            $('#walktime_googleimg').attr({src : imgURL});
        };

        /////////////////////////////
        // Initialization function //
        /////////////////////////////

        /**
         * Initialization function DOCUMENTATION
         */
        var doInit = function () {

            // Make sure all browsers have access to the ToRad() func
            setToRad();

            // Initally hide the response text (until we have an actual distance to work with)
            $distanceText.text(0);
            $minutesText.text(0);

            updateLargeMapLink(defaultCoords);

            // Read XML and draw select lists
            var select_string = '';
            $.ajax({
                 type: 'GET',
                 url: '/devwidgets/walktime/map_coordinates.xml',
                 dataType: 'xml',
                 success: function(xml) {
                    $(xml).find('Locations').find('Location').each(function(){
                        var Name = $(this).find('Name').text();
                        var LatLong = $(this).find('Lat').text() + ',' + $(this).find('Lon').text();
                        var newOpt = '<option value="'+ LatLong +'">'+ Name +'</option>\n';
                        select_string = select_string.concat(newOpt);
                    });

                    // Modify DOM selects just once for each list
                    $startPoint.append(select_string);
                    $endPoint.append(select_string);
                }
            });
            showMainView();
        };

        // run the initialization function when the widget object loads
        doInit();
    };

    // inform Sakai OAE that this widget has loaded and is ready to run
    sakai.api.Widgets.widgetLoader.informOnLoad('walktime');
});
