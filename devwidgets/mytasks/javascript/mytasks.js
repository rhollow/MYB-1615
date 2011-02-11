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
require(["jquery", "sakai/sakai.api.core", "/dev/lib/myb/myb.noticewidgets.js"], function($, sakai, noticewidgets) {

    /*
     * Begin the definition of specific widgets, which instantiate the generic Widget defined above.
     */
    
    sakai_global.mytasks = function(tuid) {
        var rootContainer = $("#" + tuid);
        var tasksListContainer = $(".tasks_list", rootContainer);
        var template = "mytasks_template";
        var detailTemplate = "mytasks_detail_template";
        var dataURL = "/var/notices/tasks.json";
        var archiveDataURL = "/var/notices/tasks_archive.json";
        var filterSettingsURL = "/~" + sakai.data.me.user.userid + "/private/mytasks_filter";
        var widgetName = "mytasks";
    
        var getDateRange = function() {
            return $("input[name=mytasks_date_range]:radio:checked", rootContainer).val();
        };
    
        var getItemStatus = function() {
            return $("input[name=mytasks_item_status]:radio:checked", rootContainer).val();
        };
    
        var filterSelectionToMessage = function() {
            var itemStatus = $("input[name=mytasks_item_status]:radio:checked", rootContainer).val();
            var dateRange = getDateRange();
            // translate every possible combo of the 2 radio buttons to a human readable message using
            // an associative array instead of a giant switch-case statement, since it's prettier this way.
            var msgs = {
                all : {
                    all : "ALL_TASKS",
                    overdue : "OVERDUE_TASKS",
                    next7 : "TASKS_DUE_THIS_WEEK",
                    next30 : "TASKS_DUE_THIS_MONTH"
                },
                required : {
                    all : "REQUIRED_TASKS",
                    overdue : "REQUIRED_OVERDUE_TASKS",
                    next7 : "REQUIRED_TASKS_DUE_THIS_WEEK",
                    next30 : "REQUIRED_TASKS_DUE_THIS_MONTH"
                },
                unrequired : {
                    all : "UNREQUIRED_TASKS",
                    overdue : "UNREQUIRED_OVERDUE_TASKS",
                    next7 : "UNREQUIRED_TASKS_DUE_THIS_WEEK",
                    next30 : "UNREQUIRED_TASKS_DUE_THIS_MONTH"
                }
            };
            return msgs[itemStatus][dateRange];
        };
    
        var buttonMessages = {
            viewArchiveButton : {
                listMode : "VIEW_ARCHIVE",
                archiveMode : "VIEW_LIST"
            },
            noItemsMessage : {
                listMode : "YOU_HAVE_NO_TASKS",
                archiveMode : "YOU_HAVE_NO_TASKS_IN_THE_ARCHIVE"
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
                startDate = noticewidgets.BEGINNING_OF_TIME;
                endDate = noticewidgets.END_OF_TIME;
            } else {
                startDate = today;
                switch (getDateRange()) {
                    case "all" :
                        startDate = noticewidgets.BEGINNING_OF_TIME;
                        endDate = noticewidgets.END_OF_TIME;
                        break;
                    case "next7" :
                        startDate = new Date();
                        endDate.setTime(today.getTime() + 7 * noticewidgets.ONE_DAY);
                        break;
                    case "next30" :
                        startDate = new Date();
                        endDate.setTime(today.getTime() + 30 * noticewidgets.ONE_DAY);
                        break;
                    case "overdue" :
                        startDate = noticewidgets.BEGINNING_OF_TIME;
                        endDate = new Date();
                        break;
                }
            }
    
            var itemStatus = getItemStatus();
            var excludeRequiredState = "none";
            if (itemStatus === "required") {
                excludeRequiredState = "false";
            } else if (itemStatus === "unrequired") {
                excludeRequiredState = "true";
            }
            return "&startDate=" + Globalization.format(startDate, noticewidgets.DATE_FORMAT_ISO8601)
                    + "&endDate=" + Globalization.format(endDate, noticewidgets.DATE_FORMAT_ISO8601)
                    + "&excludeRequiredState=" + excludeRequiredState;
        };
    
        var checkForOverdueTasks = function() {
            // TODO when KERN-1471 is fixed, bundle this request into batch request with the main search
            var overdueTaskSearchURL = "/var/notices/tasks.json?startDate=" +
                    Globalization.format(noticewidgets.BEGINNING_OF_TIME, noticewidgets.DATE_FORMAT_ISO8601)
                    + "&endDate=" + Globalization.format(new Date(), noticewidgets.DATE_FORMAT_ISO8601) +
                    "&excludeRequiredState=completed&items=1";
            $.ajax({
                url: overdueTaskSearchURL,
                cache: false,
                success: function(data) {
                    if ($.isArray(data.results) && data.results.length > 0) {
                        $("#mytasks_overdue_tasks_exist", rootContainer).show();
                    }
                },
                error: function(xhr, textStatus, thrownError) {
                    sakai.api.Util.notification.show("",
                            sakai.api.i18n.Widgets.getValueForKey(widgetName, "default", "AN_ERROR_OCCURRED_CONTACTING_THE_SERVER"),
                            sakai.api.Util.notification.type.ERROR, false);
                    window.debug.error("Checking overdue tasks failed for:\n" + overdueTaskSearchURL + "\ncategory=reminders with status=" + textStatus +
                            " and thrownError=" + thrownError + "\n" + xhr.responseText);
                }
            });
        };
    
        var doInit = function() {
            var taskWidget = noticewidgets.Widget({
                rootContainer : rootContainer,
                widgetName : widgetName,
                dataURL : dataURL,
                archiveDataURL : archiveDataURL,
                filterSettingsURL : filterSettingsURL,
                template : template,
                container : tasksListContainer,
                detailTemplate : detailTemplate,
                convertFilterStateToMessage : filterSelectionToMessage,
                defaultSortOn : "sakai:dueDate",
                buttonMessages : buttonMessages,
                buildExtraQueryParams : buildExtraQueryParams,
                getDateRange : getDateRange,
                getItemStatus : getItemStatus
            });
            taskWidget.init();
            taskWidget.start();
            checkForOverdueTasks();
        };
    
        doInit();
    };
    
    sakai_global.myevents = function(tuid) {
        var rootContainer = $("#" + tuid);
        var tasksListContainer = $(".events_list", rootContainer);
        var template = "myevents_template";
        var detailTemplate = "myevents_detail_template";
        var dataURL = "/var/notices/events.json";
        var archiveDataURL = "/var/notices/events.json";
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
                    next7 : "EVENTS_THIS_WEEK",
                    next30 : "EVENTS_THIS_MONTH"
                },
                required : {
                    all : "REQUIRED_EVENTS",
                    next7 : "REQUIRED_EVENTS_THIS_WEEK",
                    next30 : "REQUIRED_EVENTS_THIS_MONTH"
                },
                unrequired : {
                    all : "UNREQUIRED_EVENTS",
                    next7 : "UNREQUIRED_EVENTS_THIS_WEEK",
                    next30 : "UNREQUIRED_EVENTS_THIS_MONTH"
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
                startDate = noticewidgets.BEGINNING_OF_TIME;
                endDate.setTime(today.getTime() - 1);
            } else {
                startDate = today;
                switch (getDateRange()) {
                    case "all" :
                        endDate = noticewidgets.END_OF_TIME;
                        break;
                    case "next7" :
                        endDate.setTime(today.getTime() + 7 * noticewidgets.ONE_DAY);
                        break;
                    case "next30" :
                        endDate.setTime(today.getTime() + 30 * noticewidgets.ONE_DAY);
                        break;
                }
            }
    
            var itemStatus = getItemStatus();
            var excludeRequiredState = "none";
            if (itemStatus === "required") {
                excludeRequiredState = "false";
            } else if (itemStatus === "unrequired") {
                excludeRequiredState = "true";
            }
            return "&startDate=" + Globalization.format(startDate, noticewidgets.DATE_FORMAT_ISO8601)
                    + "&endDate=" + Globalization.format(endDate, noticewidgets.DATE_FORMAT_ISO8601)
                    + "&excludeRequiredState=" + excludeRequiredState;
        };
    
        var doInit = function() {
            var eventWidget = noticewidgets.Widget({
                rootContainer : rootContainer,
                widgetName : widgetName,
                dataURL : dataURL,
                archiveDataURL : archiveDataURL,
                filterSettingsURL : filterSettingsURL,
                template : template,
                container : tasksListContainer,
                detailTemplate : detailTemplate,
                convertFilterStateToMessage : filterSelectionToMessage,
                defaultSortOn : "sakai:eventDate",
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
    
    sakai.api.Widgets.widgetLoader.informOnLoad("mytasks");
    sakai.api.Widgets.widgetLoader.informOnLoad("myevents");
    
});
