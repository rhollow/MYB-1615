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
/* global $, Config, jQuery, sakai, sdata */
require(["jquery", "sakai/sakai.api.core", "/dev/lib/myb/myb.noticewidgets.js"], function($, sakai, noticeWidgets) {

    sakai_global.myevents = function(tuid) {
        var rootContainer = $("#" + tuid);
        var eventsListContainer = $(".events_list", rootContainer);
        var template = "myevents_template";
        var detailTemplate = "myevents_detail_template";
        var dataURL = "/system/myberkeley/caldav?type=VEVENT";
        var filterSettingsURL = "/~" + sakai.data.me.user.userid + "/private/myevents_filter";
        var widgetName = "myevents";

        var getDateRange = function() {
            return $("input[name=myevents_date_range]:radio:checked", rootContainer).val();
        };

        var getItemStatus = function() {
            return $("input[name=myevents_item_status]:radio:checked", rootContainer).val();
        };

        var filterSelectionToMessage = function() {
            var itemStatus = $("input[name=myevents_item_status]:radio:checked", rootContainer).val();
            var dateRange = getDateRange();
            // translate every possible combo of the 2 radio buttons to a human readable message using
            // an associative array instead of a giant switch-case statement, since it's prettier this way.
            var msgs = {
                all : {
                    all : "ALL_EVENTS",
                    next7 : "EVENTS_NEXT_7_DAYS",
                    next30 : "EVENTS_NEXT_30_DAYS"
                },
                required : {
                    all : "REQUIRED_EVENTS",
                    next7 : "REQUIRED_EVENTS_NEXT_7_DAYS",
                    next30 : "REQUIRED_EVENTS_NEXT_30_DAYS"
                },
                unrequired : {
                    all : "UNREQUIRED_EVENTS",
                    next7 : "UNREQUIRED_EVENTS_NEXT_7_DAYS",
                    next30 : "UNREQUIRED_EVENTS_NEXT_30_DAYS"
                }
            };
            return msgs[itemStatus][dateRange];
        };

        var buttonMessages = {
            viewArchiveButton : {
                listMode : "PAST_EVENTS",
                archiveMode : "UPCOMING_EVENTS"
            },
            noItemsMessage : {
                listMode : "YOU_HAVE_NO_EVENTS",
                archiveMode : "YOU_HAVE_NO_EVENTS_IN_THE_ARCHIVE"
            }
        };

        var buildExtraQueryParams = function(isArchiveMode) {
            var today = new Date();
            today.setHours(0);
            today.setMinutes(0);
            today.setSeconds(0);
            today.setMilliseconds(0);

            var startDate = new Date();
            var endDate = new Date();

            if (isArchiveMode) {
                startDate = noticeWidgets.BEGINNING_OF_TIME;
                endDate.setTime(today.getTime() - 1);
            } else {
                startDate = today;
                switch (getDateRange()) {
                    case "all" :
                        endDate = noticeWidgets.END_OF_TIME;
                        break;
                    case "next7" :
                        endDate.setTime(today.getTime() + 7 * noticeWidgets.ONE_DAY);
                        break;
                    case "next30" :
                        endDate.setTime(today.getTime() + 30 * noticeWidgets.ONE_DAY);
                        break;
                }
            }

            var itemStatus = getItemStatus();
            var mode = "ALL_UNARCHIVED";
            if (itemStatus === "required") {
                mode = "REQUIRED";
            } else if (itemStatus === "unrequired") {
                mode = "UNREQUIRED";
            }

            return "&mode=" + mode
                    + "&start_date=" + Globalization.format(startDate, noticeWidgets.DATE_FORMAT_ISO8601)
                    + "&end_date=" + Globalization.format(endDate, noticeWidgets.DATE_FORMAT_ISO8601);
        };

        var doInit = function() {
            var eventWidget = noticeWidgets.Widget({
                rootContainer : rootContainer,
                widgetName : widgetName,
                dataURL : dataURL,
                filterSettingsURL : filterSettingsURL,
                template : template,
                container : eventsListContainer,
                detailTemplate : detailTemplate,
                convertFilterStateToMessage : filterSelectionToMessage,
                defaultSortOn : "DATE",
                buttonMessages : buttonMessages,
                buildExtraQueryParams : buildExtraQueryParams,
                getDateRange : getDateRange,
                getItemStatus : getItemStatus
            });
            eventWidget.init();
            eventWidget.start();
        };

        doInit();
    };

    sakai.api.Widgets.widgetLoader.informOnLoad("myevents");

});
