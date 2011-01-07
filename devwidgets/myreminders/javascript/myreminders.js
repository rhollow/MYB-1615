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

// additional URLs that myberkeley uses
 sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE = "/var/message/notice/reminder_taskstate.json";

/**
 * Initialize the My Reminders widget
 * @param {String} tuid unique id of the widget
 * @param {Boolean} showSettings show the settings of the widget or not
 */
sakai.myreminders = function(tuid, showSettings){

    // Page Elements
    var $rootel = $("#" + tuid);
    var $remindersList = $(".reminders_list", $rootel);

    // Template
    var myremindersTemplate = "myreminders_template";

    /**
     * Formats a date to something like Mon 1/1/10
     * @param {Object} date UTF string
     */
    sakai.myreminders.getDateString = function(date){
        if (!date) return null;
        var days_short = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

        var date = sakai.api.Util.parseSakaiDate(date);
        var year = "" + date.getFullYear();
        var dateString = days_short[date.getDay()] + " " + (date.getMonth() + 1) + "/" + date.getDate() + "/" + year.substring(2, 4);
        return dateString;
    }

    /**
     * Checks whether a date has passed, and returns a class name
     * @param {Object} date UTF string
     */
    /*sakai.myreminders.compareDates = function(date){
        var dueDate = new Date(date);
        var today = new Date();

        var sameDay = (today.getYear()==dueDate.getYear()) && (today.getMonth()==dueDate.getMonth()) && (today.getDate()==dueDate.getDate());
        return ((today > dueDate) && !sameDay) ? "pastDue" : "";
    }*/
   
   /**
     * Checks whether the due date is within 24 hours or if it has passed, and returns a class name
     * @param {Object} date UTF string
     */
   sakai.myreminders.compareDates = function(date){
        var dueDate = sakai.api.Util.parseSakaiDate(date);
        var today = new Date();
        var tomorrow = today.setDate(today.getDate() + 1);
        tomorrow = sakai.api.Util.parseSakaiDate(tomorrow);
        return (tomorrow > dueDate)? "pastDue" : "";
    }

    var lastShown = null;

    /**
     * Toggles the showing of a reminder's snippet (only allows one snippet to be shown at a time)
     * @param {Object} id the id of the reminder whose snippet is being toggled
     */
    var showSnippet = function(id){
        if ($("#snippetDiv_" + id).is(":visible")) {
            $("#li_" + id).removeClass("reminder_expanded");
            $("#showSnippetDiv_" + id).attr("title", "Show snippet");
            $("#snippetDiv_" + id).slideUp("normal");
            lastShown = null;
        } else {
            if (lastShown) {
                $("#li_" + lastShown).removeClass("reminder_expanded");
                $("#showSnippetDiv_" + lastShown).attr("title", "Show snippet");
                $("#snippetDiv_" + lastShown).slideUp("normal");
            }
            lastShown = id;
            $("#li_" + id).addClass("reminder_expanded");
            $("#showSnippetDiv_" + id).attr("title", "Hide snippet");
            $("#snippetDiv_" + id).slideDown("normal");
        }
    }
    
    var visibleReminders; // initialized in CreateRemindersList
    
    /**
     * Checks if all reminders have been completed, and displays a message if that's the case
     */
    var checkIfAllDone = function() {
        visibleReminders--;
        if(visibleReminders < 1) {
            $(".reminders_list").css("display","none");
            $(".reminders_div").append("<div class='noReminders'>You have no reminders</div>");
        }
    }

    /**
     * Updates specified property of a reminder with the specified value
     * @param {Object} url jcr:path of the reminder being updated
     * @param {Object} JSON obj containing name/value pairs to be updated, ex. {"taskState":"completed"}
     * @param {Object} callback Function to be called on completion
     */
    var updateReminder = function (url, props, callback) {
        $.ajax({
              type: 'POST',
              url: url,
              data: props,
              success: function(data, textStatus, xhr){
                  if ($.isFunction(callback)) {
                      callback();
                  }
              },
              error: function(xhr, textStatus, thrownError) {
                alert("Updating " + url + " failed for " + propname + " = " + propvalue + " with status =" + textStatus +
                    " and thrownError = " + thrownError + "\n" + xhr.responseText);
              },
              dataType: 'json'
        });
    };
    
    /**
     * Listens for a checkbox being marked to indicate that the reminder has been completed
     * @param {Object} evt click
     */
    $(".s3s-reminder-checkbox").live("click", function(evt){
        var id = evt.target.id;
        id = id.split("_");

        var reminderDiv = $("#div_" + id[id.length - 1]);
        var reminderData = reminderDiv.data("data");
        var jcr_path = reminderData["jcr:path"];
        var propertyToUpdate = {
            "sakai:taskState": "completed"
        };
        updateReminder(jcr_path, propertyToUpdate, function(){
            reminderDiv.slideUp("normal", function(){
                reminderDiv.remove;
            });
        });
                
        checkIfAllDone();
    });

    /**
     * Listens for a click to show/hide a reminder's snippet
     * @param {Object} evt click
     */
    $(".slideButton").live("click", function(evt){
        var id = evt.target.id;
        id = id.split("_");

        showSnippet(id[id.length - 1]);
    });
    
    /**
     * Attaches data to each reminder node, styles according to read/unread/past due status, sets column widths
     * @param {Object} data JSON containing an array called "results" which contains the reminders
     */
    var formatReminders = function(data) {
        var results_length = data.results.length;
        visibleReminders = data.results.length;
        for (var i = 0; i < results_length; i++) {
            $("#div_" + data.results[i].id).data("data", data.results[i]);
            $("#li_" + data.results[i].id).addClass("read_" + data.results[i]["sakai:read"]);
            $("#snippetDiv_" + data.results[i].id).addClass("read_" + data.results[i]["sakai:read"]);

            var pastDue = "" + sakai.myreminders.compareDates(data.results[i]["sakai:dueDate"]);
            $("#date_pastDue_" + data.results[i].id + ", " + "#subject_pastDue_" + data.results[i].id).addClass(pastDue);
        }
        
        if (results_length > 0) {
            var totalWidth = $("#li_" + data.results[0].id).width();
            var subjectWidth = totalWidth - 25 - 100 - 20 - 30; // 25 for checkbox, 100 for due date, 20 for slide button, 30 for misc.
            $(".subjectLine").css("width", subjectWidth);
        }
    }
    
    /**
     * Calls trimpath to populate the widget
     * @param {Object} data JSON containing an array called "results" which contains the reminders
     */
    var createRemindersList = function(data){
        $remindersList.html($.TemplateRenderer(myremindersTemplate, data));

        formatReminders(data);
    };

    /**
     * Fetches the data used to populate the widget
     * @param {Object} taskState Query value for a reminder's taskState property
     * @param {Object} callback Function to be called on completion
     */
    var getRemindersList = function(taskState, callback){
        var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=" + taskState;
        
        // replacing top line with bottom line possible solve the problem of having reminders show up in sender's widget
        //var dataURL = sakai.config.URL.MESSAGE_BOXCATEGORY_SERVICE + "?box=" + box + "&category=" + cats + "&items=" + messagesPerPage + "&page=" + currentPage;

        $.ajax({
            url: dataURL,
            cache: false,
            success: function(data) {
                if (data.results) {
                    createRemindersList(data);
                }
                if ($.isFunction(callback)) {
                    callback();
                }
            },
            error: function(xhr, textStatus, thrownError) {
                alert("Getting Reminders failed for:\n" + url + "\ncategory=reminders and taskstate=" + taskState + " with status=" + textStatus +
                    " and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        })
    };

    /**
     * Initial function
     */
    var doInit = function(){
        getRemindersList("created");
    };

    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("myreminders");