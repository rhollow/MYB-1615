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
    var that = function() {};
    that.config = config;
    that.data = null;

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
                && !$(e.target).is(".mytasks_filter_control")
                && !$(e.target).parents().is(".mytasks_filter")) {
            hideFilters();
        }
    });

    that.getNotices = function() {

        $.ajax({
            url: config.dataURL,
            cache: false,
            success: function(data) {
                if (data.results) {
                    that.data = data;
                    config.formatter(data);
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
        config.filterControl.html("Filter: " + config.toEnglishFunction());
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
 * Initialize the My Tasks widget
 * @param {String} tuid unique id of the widget
 * @param {Boolean} showSettings show the settings of the widget or not
 */
sakai.mytasks = function(tuid) {

    var $rootel = $("#" + tuid);
    var $tasksList = $(".tasks_list", $rootel);
    var template = "mytasks_template";
    var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=created";
    var filterContainer = $(".mytasks_filter", $rootel);
    var filterControl = $(".mytasks_filter_control", $rootel);

    var formatTasks = function(data) {
        $tasksList.html($.TemplateRenderer(template, data));
    };

    var filterSelectionToEnglish = function() {
        var itemStatus = $("input:radio:checked", filterContainer).val();
        return itemStatus;
    };

    var doInit = function() {
        var taskWidget = sakai.myb.noticewidgets.widget({
            dataURL : dataURL,
            formatter : formatTasks,
            filterControl : filterControl,
            filterContainer : filterContainer,
            toEnglishFunction : filterSelectionToEnglish
        });
        taskWidget.getNotices();
    };


    doInit();
};

sakai.myevents = function(tuid) {

    var doInit = function() {

    };

    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("mytasks");
sakai.api.Widgets.widgetLoader.informOnLoad("myevents");