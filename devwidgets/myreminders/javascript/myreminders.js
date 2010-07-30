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
/*global $, Config, jQuery, sakai, sdata */

var sakai = sakai || {};

// move this to the global config.js ??
// the query at this URL will get sakai:type='notice' of sakai:category='reminder' with taskState as a request parameter
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
        var days_short = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

        var d = new Date(date);
        var year = "" + d.getFullYear();
        var dateString = days_short[d.getDay()] + " " + (d.getMonth() + 1) + "/" + d.getDate() + "/" + year.substring(2, 4);
        return dateString;
    }

    /**
     * Checks whether a date has passed, and returns a class name
     * @param {Object} date UTF string
     */
    sakai.myreminders.compareDates = function(date){
        var dueDate = new Date(date);
        var today = new Date();

        var sameDay = (today.getYear()==dueDate.getYear()) && (today.getMonth()==dueDate.getMonth()) && (today.getDate()==dueDate.getDate());
        return ((today > dueDate) && !sameDay) ? "pastDue" : "";
    }

    var mockreminders = {
        "items": 25,
        "total": 5,
        "results": [{
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-08-30T06:22:46-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:type": "internal",
            "sakai:messagebox": "inbox",
            "sakai:category": "reminder",
            "sakai:id": "37683cf926ee703a2a7a2a19bf298845c24a7712",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "5th week deadline is approaching",
            "sakai:body": "Dear CED Undergraduate Student, \n\n This message is to remind you that FRIDAY, FEBRUARY 20th is the deadline for undergraduates in the College",
            "sakai:read": true,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-09-12T06:22:46-07:00",
            "sakai:completeDate": "",
            "_charset_": "utf-8",
            "id": "37683cf926ee703a2a7a2a19bf298845c24a7712",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "Eli",
                "lastName": "Cochran",
                "picture": false,
                "user": "eli",
                "sakai:status": "online",
                "sakai:location": "none"
            }],
            "userFrom": [{
                "userid": "wombat",
                "hash": "/w/wo/wombat",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }, {
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-06-30T06:39:51-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:previousmessage": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
            "sakai:messagebox": "inbox",
            "sakai:type": "internal",
            "sakai:category": "reminder",
            "sakai:id": "c250e0204224c7ff234579f2282128359ddb89c8",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "CED Commencement tickets",
            "sakai:body": "Dear CED seniors, \n\n <b>Beginning Monday, April</b> students who have RSVPed for CED's commencement may pick up four complimentary tickets and",
            "sakai:read": true,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-02-10T06:22:46-07:00",
            "sakai:completeDate": "",
            "_charset_": "utf-8",
            "id": "c250e0204224c7ff234579f2282128359ddb89c8",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "Eli",
                "lastName": "Cochran",
                "picture": false,
                "user": "eli",
                "sakai:status": "online",
                "sakai:location": "none"
            }],
            "userFrom": [{
                "userid": "wombat",
                "hash": "/w/wo/wombat",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }, {
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-06-30T06:39:51-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:type": "internal",
            "sakai:messagebox": "inbox",
            "sakai:category": "reminder",
            "sakai:id": "c250e0204224c7ff234579f2282128359ddb89c9",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "Peer Adviser Workshop: Experience at Cal",
            "sakai:body": "Wondering why our curriculum is the way it is? \n How your studio projects relate to professional work? \n How to make the most of your time here?",
            "sakai:read": false,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-08-10T06:22:46-07:00",
            "sakai:completeDate": "",
            "_charset_": "utf-8",
            "id": "c250e0204224c7ff234579f2282128359ddb89c9",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "Eli",
                "lastName": "Cochran",
                "picture": false,
                "user": "eli",
                "sakai:status": "online",
                "sakai:location": "none"
            }],
            "userFrom": [{
                "userid": "wombat",
                "hash": "/w/wo/wombat",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }, {
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-06-30T06:39:51-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:previousmessage": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
            "sakai:messagebox": "inbox",
            "sakai:type": "internal",
            "sakai:category": "reminder",
            "sakai:id": "c250e0204224c7ff234579f2282128359ddb88c8",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "Getting too many emails?",
            "sakai:body": "Dear ARCH undergraduates, \n\n Following are some helpful hints on managing the emails you receive through the CED and ARCH events listserve",
            "sakai:read": true,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-11-30T06:22:46-07:00",
            "sakai:completeDate": "",
            "_charset_": "utf-8",
            "id": "c250e0204224c7ff234579f2282128359ddb88c8",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "Eli",
                "lastName": "Cochran",
                "picture": false,
                "user": "eli",
                "sakai:status": "online",
                "sakai:location": "none"
            }],
            "userFrom": [{
                "userid": "wombat",
                "hash": "/w/wo/wombat",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }, {
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-06-30T06:39:23-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:previousmessage": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
            "sakai:messagebox": "inbox",
            "sakai:type": "internal",
            "sakai:category": "reminder",
            "sakai:id": "963b9e35158e98a289a5ddf646f892033bd8aae5",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "April 23 deadline for CED commencement",
            "sakai:body": "Dear CED seniors, \n\n If you would like to participate in CED's commencement ceremony this spring, YOU MUST RSVP by Friday, APRIL 23rd \n\n To RSVP",
            "sakai:read": false,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-04-23T06:22:46-07:00",
            "sakai:completeDate": "",
            "_charset_": "utf-8",
            "id": "963b9e35158e98a289a5ddf646f892033bd8aae5",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "Eli",
                "lastName": "Cochran",
                "picture": false,
                "user": "eli",
                "sakai:status": "online",
                "sakai:location": "none"
            }],
            "userFrom": [{
                "userid": "wombat",
                "hash": "/w/wo/wombat",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }, {
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-06-30T06:39:23-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:previousmessage": "s49022e929f5cdcf1531867fb2e84ee18d6ced9",
            "sakai:messagebox": "inbox",
            "sakai:type": "internal",
            "sakai:category": "reminder",
            "sakai:id": "ss3b9e35158e98a289a5ddf646f892033bd8aae5",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "REMINDER",
            "sakai:body": "Dear CED seniors, \n\n If you would like to participate in CED's commencement ceremony this spring, YOU MUST RSVP by Friday, APRIL 23rd \n\n To RSVP",
            "sakai:read": false,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-05-23T06:22:46-07:00",
            "sakai:completeDate": "",
            "_charset_": "utf-8",
            "id": "ss3b9e35158e98a289a5ddf646f892033bd8aae5",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "Eli",
                "lastName": "Cochran",
                "picture": false,
                "user": "eli",
                "sakai:status": "online",
                "sakai:location": "none"
            }],
            "userFrom": [{
                "userid": "wombat",
                "hash": "/w/wo/wombat",
                "jcr:path": "",
                "jcr:name": "",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }]
    };

    var lastShown = null;

    /**
     * Toggles the showing of a reminder's snippet
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
                  if (typeof callback !== "undefined") {
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
        var propertyToUpdate = {"taskState":"completed"};
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

    var results_length;
    
    /**
     * Attaches data to each reminder node, styles according to read/unread/past due status, sets column widths
     * @param {Object} data JSON containing an array called "results" which contains the reminders
     */
    var formatReminders = function(data) {
        results_length = data.results.length;
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
     * Merges two JSON arrays, appending the second array's results to the first
     * @param {Object} reminders1 First JSON array with three properties: items (int), total (int), results (array of reminders)
     * @param {Object} reminders2 Second JSON array with three properties: items (int), total (int), results (array of reminders)
     */
    var merge = function(reminders1, reminders2){
        if (typeof reminders1 !== "undefined") {
            if (typeof reminders2 !== "undefined") {
                reminders1.items = reminders1.items + reminders2.items;
                reminders1.total = reminders1.total + reminders2.total;
                reminders1.results.concat(reminders2.results);
                return reminders1;
            }
        }
        else {
            alert("Got no reminders to merge");
        }
    };

    /**
     * Fetches the data used to populate the widget
     * @param {Object} taskState Query value for a reminder's taskState property
     * @param {Object} callback Function to be called on completion
     */
    var getRemindersList = function(taskState, callback){
        var dataURL = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=" + taskState;

        $.ajax({
            url: dataURL,
            cache: false,
            success: function(data) {
                if (data.results) {
                    createRemindersList(data);
                }
                else {
                    createRemindersList(mockreminders);
                }
                if (typeof callback !== "undefined") {
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