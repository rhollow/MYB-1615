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

sakai.myb.noticewidgets.DATE_FORMAT_ISO8601 = "yyyy-MM-ddTHH:mm:ss.000zzz";
sakai.myb.noticewidgets.ONE_DAY = 24 * 60 * 60 * 1000;
sakai.myb.noticewidgets.BEGINNING_OF_TIME = new Date(2000, 0, 1, 0, 0, 0, 0);
sakai.myb.noticewidgets.END_OF_TIME = new Date(3000, 0, 1, 0, 0, 0, 0);

/**
 * Formats a date to "mm/dd" format
 * @param {Object} date UTF string
 */
sakai.myb.noticewidgets.formatDate = function(date, format) {
    if (!date) return null;
    date = sakai.api.Util.parseSakaiDate(date);
    return Globalization.format(date, format);
};

sakai.myb.noticewidgets.announceError = function() {
    sakai.api.Util.notification.show("", translate("AN_ERROR_OCCURRED_CONTACTING_THE_SERVER"),
            sakai.api.Util.notification.type.ERROR, false);
};

/**
 * Generic constructor for noticewidgets (of which tasks is one type, and events is another).
 * @param config
 */
sakai.myb.noticewidgets.Widget = function(config) {
    var that = function() {
    };
    var model = {
        data : null,
        archiveMode : false,
        detailMode : false,
        currentNotice : 0,
        filterSettings : {
            sortOn : config.defaultSortOn,
            sortOrder : "ascending",
            dateRange : config.getDateRange(),
            itemStatus : config.getItemStatus()
        }
    };

    var filterContainer = $(".noticewidget_filter_container", config.rootContainer);
    var filterControl = $(".noticewidget_filter_control", config.rootContainer);
    var filterControlContainer = $(".noticewidget_filter", config.rootContainer);
    var filterControlIndicator = $(".noticewidget_filter_control_indicator", config.rootContainer);
    var loadingIndicator = $(".noticewidget_listing_loading", config.rootContainer);
    var listingTable = $("table.noticewidget_listing", config.rootContainer);

    that.init = function() {
        setupListeners();
    };

    that.start = function(callback) {
        sakai.api.Server.loadJSON(config.filterSettingsURL, function (success, data) {
            if (success) {
                // user has saved filter prefs, use 'em
                model.filterSettings = data;
                updateFilterControls();
                that.getNotices(callback);
            } else {
                // user has no stored filter settings, so save the default ones
                that.saveFilterSettingsAndGetNotices(function() {
                    that.getNotices(callback);
                });
            }
        });
    };

    that.saveFilterSettingsAndGetNotices = function(callback) {
        model.filterSettings.dateRange = config.getDateRange();
        model.filterSettings.itemStatus = config.getItemStatus();
        sakai.api.Server.saveJSON(config.filterSettingsURL, model.filterSettings, function() {
            updateFilterControls();
            that.getNotices(callback);
        });
    };

    that.getNotices = function(callback) {

        var dataURL = model.archiveMode ? config.archiveDataURL : config.dataURL;
        var url = dataURL + "?sortOn=" + model.filterSettings.sortOn + "&sortOrder=" + model.filterSettings.sortOrder
                    + config.buildExtraQueryParams(model.archiveMode);
        loadingIndicator.show();
        listingTable.hide();
        $.ajax({
            url: url,
            cache: false,
            success: function(data) {
                loadingIndicator.hide();
                listingTable.show();
                if (data.results) {
                    model.data = data;
                    model.currentNotice = 0;
                    config.container.html(sakai.api.Util.TemplateRenderer(config.template, model.data));
                    that.updateUI();
                    if ($.isFunction(callback)) {
                        callback();
                    }
                } else {
                    sakai.myb.noticewidgets.announceError();
                    window.debug.error("There are no results in the returned data. Data dump:", data);
                }
            },
            error: function(xhr, textStatus, thrownError) {
                sakai.myb.noticewidgets.announceError();
                window.debug.error("Getting notices failed for:\n" + url + "\ncategory=reminders with status=" + textStatus +
                        " and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        });
    };

    var translate = function(key) {
        return sakai.api.i18n.Widgets.getValueForKey(config.widgetName, "default", key);
    };

    var setupListeners = function() {

        var filters = function() {
            filterControl.live("click", function() {
                if (filterControlContainer.is(":visible")) {
                    filterControlContainer.hide();
                    filterControlIndicator.removeClass("open");
                    filterControlIndicator.addClass("closed");
                } else {
                    filterControlContainer.show();
                    filterControlIndicator.removeClass("closed");
                    filterControlIndicator.addClass("open");
                }
            });

            $("input:radio", config.rootContainer).live("click", function() {
                that.saveFilterSettingsAndGetNotices();
            });
        };

        var sortControls = function() {
            $(".noticewidget_listing_sort", config.rootContainer).live("click", function() {
                var newSortCol = $(this);
                var oldSortOn = model.filterSettings.sortOn;
                model.filterSettings.sortOn = newSortCol.get()[0].id.replace(/\w+_sortOn_/gi, "");
                if (oldSortOn != model.filterSettings.sortOn) {
                    model.filterSettings.sortOrder = "ascending";
                } else {
                    model.filterSettings.sortOrder = model.filterSettings.sortOrder === "ascending" ? "descending" : "ascending";
                }

                that.saveFilterSettingsAndGetNotices();

            });
        };

        var detailControls = function() {
            $(".noticewidget_listing td.detailTrigger", config.rootContainer).live("click", function() {
                model.currentNotice = this.id.replace(/\w+_/gi, "");
                model.detailMode = true;
                that.updateUI();
            });
            $(".return_to_list_container", config.rootContainer).live("click", function() {
                model.detailMode = false;
                that.updateUI();
            });
            $(".next", config.rootContainer).live("click", function() {
                if (model.currentNotice < model.data.results.length - 1) {
                    model.currentNotice++;
                    that.updateUI();
                }
            });
            $(".prev", config.rootContainer).live("click", function() {
                if (model.currentNotice > 0) {
                    model.currentNotice--;
                    that.updateUI();
                }
            });
        };

        var completedCheckboxes = function() {
            $(".task-completed-checkbox", config.rootContainer).live("click", function() {
                var rowIndex = this.id.replace(/\w+_/gi, "");
                var rowData = model.data.results[rowIndex];
                var newTaskState = rowData["sakai:taskState"] === "created" ? "completed" : "created";
                model.data.results[rowIndex]["sakai:taskState"] = newTaskState;
                postNotice(
                        model.data.results[rowIndex]["jcr:path"],
                { "sakai:taskState": newTaskState },
                          function() {
                              // update UI so it reflects the new model state
                              $.each($(".task-completed-checkbox", config.rootContainer).get(), function(index, element) {
                                  var checkboxIndex = this.id.replace(/\w+_/gi, "");
                                  if (checkboxIndex === rowIndex) {
                                      element.checked = rowData["sakai:taskState"] === "completed";
                                  }
                              });
                              that.updateUI();
                          }
                        );
            });
        };


        var archiveControls = function() {
            $(".noticewidget_view_task_archive", config.rootContainer).live("click", function() {
                model.archiveMode = !model.archiveMode;
                model.detailMode = false;
                that.getNotices(function() {
                    if (model.archiveMode) {
                        filterContainer.hide();
                    } else {
                        filterContainer.show();
                    }
                });
            });
            $(".noticewidget_archive_tasks_button", config.rootContainer).live("click", function() {
                if ($(this).is(".s3d-disabled")) {
                    // don't attempt to archive a task whose archive button is disabled
                    return;
                }
                if (model.detailMode) {
                    model.detailMode = false;
                    var row = model.data.results[model.currentNotice];
                    var postData = model.archiveMode ? { "sakai:archived@Delete": true } : { "sakai:archived": "archived" };
                    postNotice(
                            row["jcr:path"],
                            postData,
                              function() {
                                  that.getNotices();
                              }
                            );
                    return;
                }

                var requests = [];
                if (model.archiveMode) {
                    $.each(model.data.results, function(index, row) {
                        var selectionBox = $("#mytaskstdselect_" + index + " input");
                        if (selectionBox.get()[0].checked) {
                            requests[requests.length] = {
                                url : row["jcr:path"],
                                method : "POST",
                                parameters : { "sakai:archived@Delete": true }
                            };
                        }
                    });
                } else {
                    $.each(model.data.results, function(index, row) {
                        if (row["sakai:taskState"] === "completed") {
                            requests[requests.length] = {
                                url : row["jcr:path"],
                                method : "POST",
                                parameters : { "sakai:archived": "archived" }
                            };
                        }
                    });
                }
                postNotice(sakai.config.URL.BATCH, {
                    requests: $.toJSON(requests)
                }, that.getNotices);
            });
        };

        filters();
        sortControls();
        detailControls();
        completedCheckboxes();
        archiveControls();

    };

    that.updateUI = function() {

        var archiveControls = function() {
            var archiveTasksButtonText = $(".noticewidget_archive_tasks_button span", config.rootContainer);
            var viewArchiveButton = $(".noticewidget_view_task_archive", config.rootContainer);
            var noTasksMessage = $(".empty_list td:first", config.rootContainer);
            var selectorCells = $(".noticewidget_task_selector", config.rootContainer);

            if (model.archiveMode) {
                archiveTasksButtonText.html(translate("MOVE_SELECTED_BACK_TO_LIST"));
                viewArchiveButton.html(translate(config.buttonMessages.viewArchiveButton.archiveMode));
                noTasksMessage.html(translate(config.buttonMessages.noItemsMessage.archiveMode));
                selectorCells.show();
            } else {
                archiveTasksButtonText.html(translate("ARCHIVE_COMPLETED_TASKS"));
                viewArchiveButton.html(translate(config.buttonMessages.viewArchiveButton.listMode));
                noTasksMessage.html(translate(config.buttonMessages.noItemsMessage.listMode));
                selectorCells.hide();
            }
            var enabled = model.data.results.length > 0;
            if (model.detailMode) {
                if (model.archiveMode) {
                    archiveTasksButtonText.html(translate("MOVE_THIS_TASK_BACK_TO_LIST"));
                } else {
                    archiveTasksButtonText.html(translate("ARCHIVE_THIS_TASK"));
                }

                if (model.data.results[model.currentNotice]) {
                    var isCurrentTaskRequired = model.data.results[model.currentNotice]["sakai:required"];
                    if (isCurrentTaskRequired) {
                        enabled = model.data.results[model.currentNotice]["sakai:taskState"] === "completed";
                    } else {
                        enabled = true;
                    }
                }
            }
            var parent = archiveTasksButtonText.parent();
            if (enabled) {
                parent.removeClass("s3d-disabled");
                parent.addClass("s3d-button-primary");
            } else {
                parent.addClass("s3d-disabled");
                parent.removeClass("s3d-button-primary");
            }
        };

        var scroller = function() {
            var tbody = $("table.noticewidget_listing tbody", config.rootContainer);
            if (tbody[0].clientHeight > 150) {
                tbody.addClass("scroller");
            } else {
                tbody.removeClass("scroller");
            }
        };

        var subjectLines = function() {
            $("td.subjectLine", config.rootContainer).ThreeDots({
                max_rows : 1
            });
        };

        var filterStatus = function() {
            filterControl.html(translate("FILTER") + " " + translate(config.convertFilterStateToMessage()));
        };

        var showCurrentDetail = function() {
            $(".noticewidget_detail", config.rootContainer).html(sakai.api.Util.TemplateRenderer(config.detailTemplate,
            {
                detail : model.data.results[model.currentNotice],
                index : model.currentNotice
            }));
            if (model.currentNotice < model.data.results.length - 1) {
                $(".nextArrow", config.rootContainer).removeClass("disabled");
            } else {
                $(".nextArrow", config.rootContainer).addClass("disabled");
            }
            if (model.currentNotice > 0) {
                $(".prevArrow", config.rootContainer).removeClass("disabled");
            } else {
                $(".prevArrow", config.rootContainer).addClass("disabled");
            }
        };

        var detailMode = function() {
            var detailViewContainer = $(".noticewidget_detail_view", config.rootContainer);
            var listViewContainer = $(".noticewidget_list_view", config.rootContainer);
            if (model.detailMode) {
                showCurrentDetail();
                listViewContainer.hide();
                detailViewContainer.show();
            } else {
                listViewContainer.show();
                detailViewContainer.hide();
            }
        };

        detailMode();
        archiveControls();
        scroller();
        subjectLines();
        filterStatus();
    };

    var updateFilterControls = function() {
        // move the sort arrow to the current sort column
        var currentSortCol = $("#" + config.widgetName + "_sortOn_" + model.filterSettings.sortOn.replace(/:/gi, "\\:"));
        var arrow = $(".noticewidget_listing thead span", config.rootContainer);
        arrow.removeClass("descending");
        arrow.addClass(model.filterSettings.sortOrder);
        arrow.remove();
        arrow.appendTo(currentSortCol);

        // update the radio buttons
        var dateRangeRadio = $("#" + config.widgetName + "_date_range_" + model.filterSettings.dateRange);
        var itemStatusRadio = $("#" + config.widgetName + "_item_status_" + model.filterSettings.itemStatus);
        dateRangeRadio[0].checked = true;
        itemStatusRadio[0].checked = true;
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
                sakai.myb.noticewidgets.announceError();
                window.debug.error("POST to " + url + " failed for " + props + " with status =" + textStatus +
                        " and thrownError = " + thrownError + "\n" + xhr.responseText);
            },
            dataType: 'json'
        });
    };

    return that;
};

/*
 * Begin the definition of specific widgets, which instantiate the generic Widget defined above.
 */

sakai.mytasks = function(tuid) {
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
            startDate = sakai.myb.noticewidgets.BEGINNING_OF_TIME;
            endDate = sakai.myb.noticewidgets.END_OF_TIME;
        } else {
            startDate = today;
            switch (getDateRange()) {
                case "all" :
                    startDate = sakai.myb.noticewidgets.BEGINNING_OF_TIME;
                    endDate = sakai.myb.noticewidgets.END_OF_TIME;
                    break;
                case "next7" :
                    endDate.setTime(today.getTime() + 7 * sakai.myb.noticewidgets.ONE_DAY);
                    break;
                case "next30" :
                    endDate.setTime(today.getTime() + 30 * sakai.myb.noticewidgets.ONE_DAY);
                    break;
                case "overdue" :
                    startDate = sakai.myb.noticewidgets.BEGINNING_OF_TIME;
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
        return "&startDate=" + Globalization.format(startDate, sakai.myb.noticewidgets.DATE_FORMAT_ISO8601)
                + "&endDate=" + Globalization.format(endDate, sakai.myb.noticewidgets.DATE_FORMAT_ISO8601)
                + "&excludeRequiredState=" + excludeRequiredState;
    };

    var checkForOverdueTasks = function() {
        // TODO when KERN-1471 is fixed, bundle this request into batch request with the main search
        var overdueTaskSearchURL = "/var/notices/tasks.json?startDate=" +
                Globalization.format(sakai.myb.noticewidgets.BEGINNING_OF_TIME, sakai.myb.noticewidgets.DATE_FORMAT_ISO8601)
                + "&endDate=" + Globalization.format(new Date(), sakai.myb.noticewidgets.DATE_FORMAT_ISO8601) +
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
                sakai.myb.noticewidgets.announceError();
                window.debug.error("Checking overdue tasks failed for:\n" + overdueTaskSearchURL + "\ncategory=reminders with status=" + textStatus +
                        " and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        });
    };

    var doInit = function() {
        var taskWidget = sakai.myb.noticewidgets.Widget({
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

sakai.myevents = function(tuid) {
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
            startDate = sakai.myb.noticewidgets.BEGINNING_OF_TIME;
            endDate.setTime(today.getTime());
        } else {
            startDate = today;
            switch (getDateRange()) {
                case "all" :
                    endDate = sakai.myb.noticewidgets.END_OF_TIME;
                    break;
                case "next7" :
                    endDate.setTime(today.getTime() + 7 * sakai.myb.noticewidgets.ONE_DAY);
                    break;
                case "next30" :
                    endDate.setTime(today.getTime() + 30 * sakai.myb.noticewidgets.ONE_DAY);
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
        return "&startDate=" + Globalization.format(startDate, sakai.myb.noticewidgets.DATE_FORMAT_ISO8601)
                + "&endDate=" + Globalization.format(endDate, sakai.myb.noticewidgets.DATE_FORMAT_ISO8601)
                + "&excludeRequiredState=" + excludeRequiredState;
    };

    var doInit = function() {
        var eventWidget = sakai.myb.noticewidgets.Widget({
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