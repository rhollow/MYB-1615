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

var sakai = sakai || {};
sakai.myb = sakai.myb || {};

sakai.myb.noticewidgets = {};

sakai.myb.noticewidgets.widget = function(config) {
    var that = function() {
    };
    that.config = config;
    that.data = null;

    that.getNotices = function() {

        $.ajax({
            url: config.dataURL,
            cache: false,
            success: function(data) {
                if (data.results) {
                    that.data = data;
                    config.container.html($.TemplateRenderer(config.template, that.data));
                }
            },
            error: function(xhr, textStatus, thrownError) {
                alert("Getting notices failed for:\n" + url + "\ncategory=reminders and taskstate=" + taskState + " with status=" + textStatus +
                        " and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        })
    };

    var hideFilters = function() {
        config.filterContainer.hide();
        updateFilterStatus();
    };

    var updateFilterStatus = function() {
        config.filterControl.html(sakai.myb.noticewidgets.i18n(config.widgetname, "FILTER")
                + " " + config.convertFilterToMessages());
    };

    // set up click listeners
    config.filterControl.live("click", function() {
        if (config.filterContainer.is(":visible")) {
            hideFilters();
        } else {
            config.filterContainer.show();
        }
    });

    $(window).bind("click", function(e) {
        // if filter is visible and the target element clicked is not filter or its control then hide filter
        if (config.filterContainer.is(":visible")
                && !$(e.target).is(".noticewidget_filter_control")
                && !$(e.target).parents().is(".noticewidget_filter")) {
            hideFilters();
        }
    });

    updateFilterStatus();

    return that;
};

/**
 * Formats a date to "mm/dd" format
 * @param {Object} date UTF string
 */
sakai.myb.noticewidgets.formatDateAsString = function(date) {
    if (!date) return null;
    date = sakai.api.Util.parseSakaiDate(date);
    return date.getMonth() + 1 + "/" + date.getDate();
};

sakai.myb.noticewidgets.i18n = function(widgetname, key) {
    return sakai.api.i18n.Widgets.getValueForKey(widgetname, "default", key);
};

sakai.mytasks = function(tuid) {
    var rootContainer = $("#" + tuid);
    var tasksListContainer = $(".tasks_list", rootContainer);
    var template = "mytasks_template";
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
    var filterContainer = $(".noticewidget_filter", rootContainer);
    var filterControl = $(".noticewidget_filter_control", rootContainer);
    var widgetname = "mytasks";

    var filterSelectionToMessage = function() {
        var itemStatus = $("input[name=mytasks_item_status]:radio:checked", filterContainer).val();
        var dateRange = $("input[name=mytasks_date_range]:radio:checked", filterContainer).val();
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
        return sakai.myb.noticewidgets.i18n(widgetname, msgs[itemStatus][dateRange]);
    };

    var doInit = function() {
        var taskWidget = sakai.myb.noticewidgets.widget({
            widgetname : widgetname,
            dataURL : dataURL,
            template : template,
            container : tasksListContainer,
            filterControl : filterControl,
            filterContainer : filterContainer,
            convertFilterToMessages : filterSelectionToMessage
        });
        taskWidget.getNotices();
    };

    doInit();
};

sakai.myevents = function(tuid) {
    var rootContainer = $("#" + tuid);
    var tasksListContainer = $(".events_list", rootContainer);
    var template = "myevents_template";
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
    var filterContainer = $(".noticewidget_filter", rootContainer);
    var filterControl = $(".noticewidget_filter_control", rootContainer);
    var widgetname = "myevents";

    var filterSelectionToMessage = function() {
        var itemStatus = $("input[name=myevents_item_status]:radio:checked", filterContainer).val();
        var dateRange = $("input[name=myevents_date_range]:radio:checked", filterContainer).val();
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
        return sakai.myb.noticewidgets.i18n(widgetname, msgs[itemStatus][dateRange]);
    };

    var doInit = function() {
        var eventWidget = sakai.myb.noticewidgets.widget({
            widgetname : widgetname,
            dataURL : dataURL,
            template : template,
            container : tasksListContainer,
            filterControl : filterControl,
            filterContainer : filterContainer,
            convertFilterToMessages : filterSelectionToMessage
        });
        eventWidget.getNotices();
    };

    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("mytasks");
sakai.api.Widgets.widgetLoader.informOnLoad("myevents");