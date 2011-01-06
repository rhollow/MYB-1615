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
 * @param cfgObject
 */
sakai.myb.noticewidgets.Widget = function(cfgObject) {
    var that = function() {
    };
    that.config = cfgObject;
    that.data = null;
    var sortOn = that.config.defaultSortOn;
    var sortOrder = "ascending";
    var filterControl = $(".noticewidget_filter_control", that.config.rootContainer);
    var filterContainer = $(".noticewidget_filter", that.config.rootContainer);
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
            url: that.config.dataURL + "&sortOn=" + sortOn + "&sortOrder=" + sortOrder,
            cache: false,
            success: function(data) {
                if (data.results) {
                    that.data = data;
                    that.config.container.html($.TemplateRenderer(that.config.template, that.data));
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
        updateFilterStatus();
    };

    var updateFilterStatus = function() {
        filterControl.html(translate("FILTER") + " " + translate(that.config.convertFilterStateToMessage()));
    };

    var translate = function(key) {
        return sakai.api.i18n.Widgets.getValueForKey(that.config.widgetName, "default", key);
    };

    var attachFilterListeners = function() {
        filterControl.live("click", function() {
            if (filterContainer.is(":visible")) {
                hideFilters();
            } else {
                filterContainer.show();
            }
        });

        $(window).bind("click", function(e) {
            // if filter is visible and the target element clicked is not filter or its control then hide filter
            if (filterContainer.is(":visible")
                    && !$(e.target).is(".noticewidget_filter_control")
                    && !$(e.target).parents().is("#" + that.config.widgetName + "_filter")) {
                hideFilters();
            }
        });
    };

    var attachSortListeners = function() {
        $(".noticewidget_listing_sort", that.config.rootContainer).live("click", function(event) {
            var newSortCol = $(event.target);
            var oldSortOn = sortOn;
            sortOn = newSortCol.get()[0].id.replace(/\w+_sortOn_/gi, "");
            if ( oldSortOn != sortOn ) {
                sortOrder = "ascending";
            } else {
                sortOrder = sortOrder === "ascending" ? "descending" : "ascending";
            }

            that.getNotices(function() {
                // clear old sort arrows
                $(".noticewidget_sort_indicator_ascending", that.config.rootContainer).removeClass("noticewidget_sort_indicator_ascending");
                $(".noticewidget_sort_indicator_descending", that.config.rootContainer).removeClass("noticewidget_sort_indicator_descending");
                // set the new sort arrow
                newSortCol.addClass("noticewidget_sort_indicator_" + sortOrder);
            });
        });
    };

    var attachDetailListeners = function() {
        $(".noticewidget_listing td.detailTrigger", that.config.rootContainer).live("click", function() {
            var rowIndex = this.id.replace(/\w+_/gi, "");
            currentNotice = rowIndex;
            showCurrentDetail();
            toggleDetailMode();
        });
        $(".return_to_list", that.config.rootContainer).live("click", function() {
            toggleDetailMode();
        });
        $(".next", that.config.rootContainer).live("click", function() {
            if ( currentNotice < that.data.results.length - 1 ) {
                currentNotice++;
                showCurrentDetail();
            }
        });
        $(".prev", that.config.rootContainer).live("click", function() {
            if ( currentNotice > 0) {
                currentNotice--;
                showCurrentDetail();
            }
        });

    };

    var attachCompletedCheckboxListeners = function() {
        $(".task-completed-checkbox", that.config.rootContainer).live("click", function() {
            var rowIndex = this.id.replace(/\w+_/gi, "");
            var rowData = that.data.results[rowIndex];
            var newTaskState = rowData["sakai:taskState"] === "created" ? "completed" : "created";
            that.data.results[rowIndex]["sakai:taskState"] = newTaskState;
            postNotice(
                    that.data.results[rowIndex]["jcr:path"],
                    { "sakai:taskState": newTaskState },
                    function() {}
            );
        });
    };

    var showCurrentDetail = function() {
        $(".noticewidget_detail", that.config.rootContainer).html($.TemplateRenderer(that.config.detailTemplate,
            {
                detail : that.data.results[currentNotice],
                index : currentNotice
            }));
    };

    var toggleDetailMode = function() {
        var detailViewContainer = $(".noticewidget_detail_view", that.config.rootContainer);
        var listViewContainer = $(".noticewidget_list_view", that.config.rootContainer);
        if ( detailViewContainer.is(":visible")) {
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
              success: function(){
                  if ($.isFunction(callback)) {
                      callback();
                  }
              },
              error: function(xhr, textStatus, thrownError) {
                alert("POST to " + url + " failed for " + propname + " = " + propvalue + " with status =" + textStatus +
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
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
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
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
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