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

sakai.myb.noticewidgets.Widget = function(cfgObject) {
    var that = function() {
    };
    that.config = cfgObject;
    that.data = null;
    that.sortBy = null;
    that.sortOrder = "asc";

    that.init = function() {
        attachFilterListeners();
        attachSortListeners();
        updateFilterStatus();
    };

    that.getNotices = function() {
        $.ajax({
            url: that.config.dataURL,
            cache: false,
            success: function(data) {
                if (data.results) {
                    that.data = data;
                    that.config.container.html($.TemplateRenderer(that.config.template, that.data));
                }
            },
            error: function(xhr, textStatus, thrownError) {
                alert("Getting notices failed for:\n" + url + "\ncategory=reminders and taskstate=" + taskState + " with status=" + textStatus +
                        " and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        })
    };

    var hideFilters = function() {
        that.config.filterContainer.hide();
        updateFilterStatus();
    };

    var updateFilterStatus = function() {
        that.config.filterControl.html(translate("FILTER") + " " + translate(that.config.convertFilterStateToMessage()));
    };

    var translate = function(key) {
        return sakai.api.i18n.Widgets.getValueForKey(that.config.widgetName, "default", key);
    };

    var attachFilterListeners = function() {
        that.config.filterControl.live("click", function() {
            if (that.config.filterContainer.is(":visible")) {
                hideFilters();
            } else {
                that.config.filterContainer.show();
            }
        });

        $(window).bind("click", function(e) {
            // if filter is visible and the target element clicked is not filter or its control then hide filter
            if (that.config.filterContainer.is(":visible")
                    && !$(e.target).is("#" + that.config.widgetName + "_filter_control")
                    && !$(e.target).parents().is("#" + that.config.widgetName + "_filter")) {
                hideFilters();
            }
        });
    };

    var attachSortListeners = function() {
        $(".noticewidget_listing_sort", that.config.rootContainer).live("click", function(event) {
            var oldSortBy = that.sortBy;
            that.sortBy = event.target.id.replace(/\w+_sortby_/gi, "");
            if ( oldSortBy != that.sortBy ) {
                that.sortOrder = "asc";
            } else {
                that.sortOrder = that.sortOrder === "asc" ? "desc" : "asc";
            }
            // clear old sort arrows
            $(".noticewidget_sort_indicator_asc", that.config.rootContainer).removeClass("noticewidget_sort_indicator_asc");
            $(".noticewidget_sort_indicator_desc", that.config.rootContainer).removeClass("noticewidget_sort_indicator_desc");
            // set the new sort arrow
            $("#" + event.target.id).addClass("noticewidget_sort_indicator_" + that.sortOrder);
        });
    };

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

sakai.mytasks = function(tuid) {
    var rootContainer = $("#" + tuid);
    var tasksListContainer = $(".tasks_list", rootContainer);
    var template = "mytasks_template";
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
    var filterContainer = $("#mytasks_filter", rootContainer);
    var filterControl = $("#mytasks_filter_control", rootContainer);
    var widgetName = "mytasks";

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
        return msgs[itemStatus][dateRange];
    };

    var doInit = function() {
        var taskWidget = sakai.myb.noticewidgets.Widget({
            rootContainer : rootContainer,
            widgetName : widgetName,
            dataURL : dataURL,
            template : template,
            container : tasksListContainer,
            filterControl : filterControl,
            filterContainer : filterContainer,
            convertFilterStateToMessage : filterSelectionToMessage
        });
        taskWidget.init();
        taskWidget.getNotices();
    };

    doInit();
};

sakai.myevents = function(tuid) {
    var rootContainer = $("#" + tuid);
    var tasksListContainer = $(".events_list", rootContainer);
    var template = "myevents_template";
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
    var filterContainer = $("#myevents_filter", rootContainer);
    var filterControl = $("#myevents_filter_control", rootContainer);
    var widgetName = "myevents";

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
        return msgs[itemStatus][dateRange];
    };

    var doInit = function() {
        var eventWidget = sakai.myb.noticewidgets.Widget({
            rootContainer : rootContainer,
            widgetName : widgetName,
            dataURL : dataURL,
            template : template,
            container : tasksListContainer,
            filterControl : filterControl,
            filterContainer : filterContainer,
            convertFilterStateToMessage : filterSelectionToMessage
        });
        eventWidget.init();
        eventWidget.getNotices();
    };

    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("mytasks");
sakai.api.Widgets.widgetLoader.informOnLoad("myevents");