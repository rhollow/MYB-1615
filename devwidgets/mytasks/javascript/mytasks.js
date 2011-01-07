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

/**
 * Generic constructor for noticewidgets (of which tasks is one type, and events is another).
 * @param config
 */
sakai.myb.noticewidgets.Widget = function(config) {
    var that = function() {
    };
    var model = null;
    var sortOn = config.defaultSortOn;
    var sortOrder = "ascending";
    var filterControl = $(".noticewidget_filter_control", config.rootContainer);
    var filterContainer = $(".noticewidget_filter", config.rootContainer);
    var filterControlIndicator = $(".noticewidget_filter_control_indicator", config.rootContainer);
    var currentNotice = 0;
              
    that.init = function() {
        attachFilterListeners();
        attachSortListeners();
        attachDetailListeners();
        attachCompletedCheckboxListeners();
        updateFilterStatus();
    };

    that.getNotices = function(callback) {
        $.ajax({
            url: config.dataURL + "&sortOn=" + sortOn + "&sortOrder=" + sortOrder,
            cache: false,
            success: function(data) {
                if (data.results) {
                    model = data;
                    config.container.html($.TemplateRenderer(config.template, model));
                    if ($.isFunction(callback)) {
                        callback();
                    }
                }
            },
            error: function(xhr, textStatus, thrownError) {
                alert("Getting notices failed for:\n" + url + "\ncategory=reminders with status=" + textStatus +
                        " and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        })
    };

    var hideFilters = function() {
        filterContainer.hide();
        filterControlIndicator.removeClass("open");
        filterControlIndicator.addClass("closed");
        updateFilterStatus();
    };

    var updateFilterStatus = function() {
        filterControl.html(translate("FILTER") + " " + translate(config.convertFilterStateToMessage()));
    };

    var translate = function(key) {
        return sakai.api.i18n.Widgets.getValueForKey(config.widgetName, "default", key);
    };

    var attachFilterListeners = function() {
        filterControl.live("click", function() {
            if (filterContainer.is(":visible")) {
                hideFilters();
            } else {
                filterContainer.show();
                filterControlIndicator.removeClass("closed");
                filterControlIndicator.addClass("open");
            }
        });

        $(window).bind("click", function(e) {
            // if filter is visible and the target element clicked is not filter or its control then hide filter
            if (filterContainer.is(":visible")
                    && !$(e.target).is(".noticewidget_filter_control")
                    && !$(e.target).parents().is("#" + config.widgetName + "_filter")) {
                hideFilters();
            }
        });
    };

    var attachSortListeners = function() {
        $(".noticewidget_listing_sort", config.rootContainer).live("click", function() {
            var newSortCol = $(this);
            var oldSortOn = sortOn;
            sortOn = newSortCol.get()[0].id.replace(/\w+_sortOn_/gi, "");
            if (oldSortOn != sortOn) {
                sortOrder = "ascending";
            } else {
                sortOrder = sortOrder === "ascending" ? "descending" : "ascending";
            }

            that.getNotices(function() {
                // clear old sort arrows
                var arrow = $(".noticewidget_listing thead span", config.rootContainer);
                arrow.removeClass("descending");
                // set the new sort arrow state
                arrow.addClass(sortOrder);
                // move arrow span to new sort col
                arrow.remove();
                arrow.appendTo(newSortCol);
            });
        });
    };

    var attachDetailListeners = function() {
        $(".noticewidget_listing td.detailTrigger", config.rootContainer).live("click", function() {
            currentNotice = this.id.replace(/\w+_/gi, "");
            showCurrentDetail();
            toggleDetailMode();
        });
        $(".return_to_list_container", config.rootContainer).live("click", function() {
            toggleDetailMode();
        });
        $(".next", config.rootContainer).live("click", function() {
            if (currentNotice < model.results.length - 1) {
                currentNotice++;
                showCurrentDetail();
            }
        });
        $(".prev", config.rootContainer).live("click", function() {
            if (currentNotice > 0) {
                currentNotice--;
                showCurrentDetail();
            }
        });
    };

    var attachCompletedCheckboxListeners = function() {
        $(".task-completed-checkbox", config.rootContainer).live("click", function() {
            var rowIndex = this.id.replace(/\w+_/gi, "");
            var rowData = model.results[rowIndex];
            var newTaskState = rowData["sakai:taskState"] === "created" ? "completed" : "created";
            model.results[rowIndex]["sakai:taskState"] = newTaskState;
            postNotice(
                    model.results[rowIndex]["jcr:path"],
            { "sakai:taskState": newTaskState },
                      function() {
                      }
                    );
        });
    };

    var showCurrentDetail = function() {
        $(".noticewidget_detail", config.rootContainer).html($.TemplateRenderer(config.detailTemplate,
        {
            detail : model.results[currentNotice],
            index : currentNotice
        }));
        if ( currentNotice < model.results.length - 1 ) {
            $(".nextArrow", config.rootContainer).removeClass("disabled");
        } else {
            $(".nextArrow", config.rootContainer).addClass("disabled");
        }
        if (currentNotice > 0 ) {
            $(".prevArrow", config.rootContainer).removeClass("disabled");
        } else {
            $(".prevArrow", config.rootContainer).addClass("disabled");
        }
    };

    var toggleDetailMode = function() {
        var detailViewContainer = $(".noticewidget_detail_view", config.rootContainer);
        var listViewContainer = $(".noticewidget_list_view", config.rootContainer);
        if (detailViewContainer.is(":visible")) {
            listViewContainer.show();
            detailViewContainer.hide();
        } else {
            listViewContainer.hide();
            detailViewContainer.show();
        }
    };

    var postNotice = function (url, props, callback) {
        $.ajax({
            type: 'POST',
            url: url,
            data: props,
            success: function() {
                if ($.isFunction(callback)) {
                    callback();
                }
            },
            error: function(xhr, textStatus, thrownError) {
                alert("POST to " + url + " failed for " + props + " with status =" + textStatus +
                        " and thrownError = " + thrownError + "\n" + xhr.responseText);
            },
            dataType: 'json'
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

/*
 * Begin the definition of specific widgets, which instantiate the generic Widget defined above.
 */

sakai.mytasks = function(tuid) {
    var rootContainer = $("#" + tuid);
    var tasksListContainer = $(".tasks_list", rootContainer);
    var template = "mytasks_template";
    var detailTemplate = "mytasks_detail_template";
    var dataURL = "/var/message/notice/tasks.json";
    var widgetName = "mytasks";

    var filterSelectionToMessage = function() {
        var itemStatus = $("input[name=mytasks_item_status]:radio:checked", rootContainer).val();
        var dateRange = $("input[name=mytasks_date_range]:radio:checked", rootContainer).val();
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
            detailTemplate : detailTemplate,
            convertFilterStateToMessage : filterSelectionToMessage,
            defaultSortOn : "sakai:dueDate"
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
    var detailTemplate = "myevents_detail_template";
    var dataURL = "/var/message/notice/events.json";
    var widgetName = "myevents";

    var filterSelectionToMessage = function() {
        var itemStatus = $("input[name=myevents_item_status]:radio:checked", rootContainer).val();
        var dateRange = $("input[name=myevents_date_range]:radio:checked", rootContainer).val();
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
            detailTemplate : detailTemplate,
            convertFilterStateToMessage : filterSelectionToMessage,
            defaultSortOn : "sakai:eventDate"
        });
        eventWidget.init();
        eventWidget.getNotices();
    };

    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("mytasks");
sakai.api.Widgets.widgetLoader.informOnLoad("myevents");