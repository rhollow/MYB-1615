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

    sakai_global.mytasks = function(tuid) {
        var rootContainer = $("#" + tuid);
        var tasksListContainer = $(".tasks_list", rootContainer);
        var template = "mytasks_template";
        var detailTemplate = "mytasks_detail_template";
        var dataURL = "/system/myberkeley/caldav?type=VTODO";
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
            // Translate every possible combo of the 2 radio buttons to a human readable message using
            // an associative array instead of a giant switch-case statement, since it's prettier this way.
            var msgs = {
                all : {
                    all : "ALL_TASKS",
                    overdue : "OVERDUE_TASKS",
                    next7 : "TASKS_DUE_NEXT_7_DAYS",
                    next30 : "TASKS_DUE_NEXT_30_DAYS"
                },
                required : {
                    all : "REQUIRED_TASKS",
                    overdue : "REQUIRED_OVERDUE_TASKS",
                    next7 : "REQUIRED_TASKS_DUE_NEXT_7_DAYS",
                    next30 : "REQUIRED_TASKS_DUE_NEXT_30_DAYS"
                },
                unrequired : {
                    all : "UNREQUIRED_TASKS",
                    overdue : "UNREQUIRED_OVERDUE_TASKS",
                    next7 : "UNREQUIRED_TASKS_DUE_NEXT_7_DAYS",
                    next30 : "UNREQUIRED_TASKS_DUE_NEXT_30_DAYS"
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
                startDate = noticeWidgets.BEGINNING_OF_TIME;
                endDate = noticeWidgets.END_OF_TIME;
            } else {
                startDate = today;
                switch (getDateRange()) {
                    case "all" :
                        startDate = noticeWidgets.BEGINNING_OF_TIME;
                        endDate = noticeWidgets.END_OF_TIME;
                        break;
                    case "next7" :
                        startDate = new Date();
                        endDate.setTime(today.getTime() + 7 * noticeWidgets.ONE_DAY);
                        break;
                    case "next30" :
                        startDate = new Date();
                        endDate.setTime(today.getTime() + 30 * noticeWidgets.ONE_DAY);
                        break;
                    case "overdue" :
                        startDate = noticeWidgets.BEGINNING_OF_TIME;
                        endDate = new Date();
                        break;
                }
            }

            return "&start_date=" + Globalization.format(startDate, noticeWidgets.DATE_FORMAT_ISO8601)
                    + "&end_date=" + Globalization.format(endDate, noticeWidgets.DATE_FORMAT_ISO8601)
        };
    
        var checkForOverdueTasks = function() {

            // TODO remove return when done debugging myb-417
            return;


            // TODO when KERN-1471 is fixed, bundle this request into batch request with the main search
            var overdueTaskSearchURL = "/var/notices/tasks.json?startDate=" +
                    Globalization.format(noticeWidgets.BEGINNING_OF_TIME, noticeWidgets.DATE_FORMAT_ISO8601)
                    + "&endDate=" + Globalization.format(new Date(), noticeWidgets.DATE_FORMAT_ISO8601) +
                    "&excludeRequiredState=completed&items=1";
            $.ajax({
                url: overdueTaskSearchURL,
                cache: false,
                success: function(data) {
                    if ($.isArray(data.results) && data.results.length > 0) {
                        $(".mytasks_overdue_tasks_msg", rootContainer).show();
                        $(rootContainer).addClass("mytasks_overdue_tasks_exist");
                    } else {
						
						if($(".mytasks_overdue_tasks_msg", rootContainer).is(":visible")){
							$(".mytasks_overdue_tasks_msg", rootContainer).hide();	
						}
						if($(rootContainer).is(".mytasks_overdue_tasks_exist")){
							$(rootContainer).removeClass("mytasks_overdue_tasks_exist");	
						}						
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
    
		var onModelChange = function (model) {
			if(!model.archiveMode){
				checkForOverdueTasks();
			}
		};
		
	
        var doInit = function() {
            var taskWidget = noticeWidgets.Widget({
                rootContainer : rootContainer,
                widgetName : widgetName,
                dataURL : dataURL,
                filterSettingsURL : filterSettingsURL,
                template : template,
                container : tasksListContainer,
                detailTemplate : detailTemplate,
                convertFilterStateToMessage : filterSelectionToMessage,
                defaultSortOn : "sakai:dueDate",
                buttonMessages : buttonMessages,
                buildExtraQueryParams : buildExtraQueryParams,
                getDateRange : getDateRange,
                getItemStatus : getItemStatus,
				onModelChange: onModelChange
            });
            taskWidget.init();
            taskWidget.start();
        };    
        doInit();
    };

    sakai.api.Widgets.widgetLoader.informOnLoad("mytasks");

});
