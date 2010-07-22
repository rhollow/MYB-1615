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
    
    var mockreminders = {
        items: 25,
        total: 3,
        results: [{
            jcr__path: "",
            jcr__name: "",
            sakai__created: "2010-08-30T06:22:46-07:00",
            sling__resourceType: "sakai/message",
            sakai__type: "internal",
            sakai__messagebox: "inbox",
            sakai__category: "reminder",
            sakai__id: "70896405574174eb091b85ee6be93d4f70558454",
            jcr__primaryType: "",
            sakai__from: "Susan Hagstrom",
            "sakai:subject": "5th week deadline is approaching",
            sakai__body: "Dear CED Undergraduate Student,",
            sakai__read: true,
            sakai__sendstate: "notified",
            "sakai:taskstate": "created",
            sakai__to: "internal:eli",
            "sakai:dueDate": "2010-09-12T06:22:46-07:00",
            sakai__completeDate: "",
            _charset_: "utf-8",
            id: "70896405574174eb091b85ee6be93d4f70558454",
            userTo: [{
                userid: "eli",
                hash: "/e/el/eli/eli",
                jcr__path: "",
                jcr__name: "",
                firstName: "Eli",
                lastName: "Cochran",
                picture: false,
                user: "eli",
                sakai__status: "online",
                sakai__location: "none"
            }],
            userFrom: [{
                userid: "wombat",
                hash: "/w/wo/wombat",
                jcr__path: "",
                jcr__name: "",
                firstName: "wombat t.",
                lastName: "firefly",
                picture: false,
                user: "wombat",
                sakai__status: "offline",
                sakai__location: "none"
            }]
        }, {
            jcr__path: "",
            jcr__name: "",
            sakai__created: "2010-06-30T06:39:51-07:00",
            sling__resourceType: "sakai/message",
            sakai__previousmessage: "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
            sakai__messagebox: "inbox",
            sakai__type: "internal",
            sakai__category: "reminder",
            sakai__id: "c250e0204224c7ff234579f2282128359ddb89c8",
            jcr__primaryType: "",
            sakai__from: "Susan Hagstrom",
            "sakai:subject": "CED Commencement tickets",
            sakai__body: "Dear CED seniors, \n\n <b>Beginning Monday, April</b>",
            sakai__read: true,
            sakai__sendstate: "notified",
            "sakai:taskstate": "created",
            sakai__to: "internal:eli",
            "sakai:dueDate": "2010-02-10T06:22:46-07:00",
            sakai__completeDate: "",
            _charset_: "utf-8",
            id: "c250e0204224c7ff234579f2282128359ddb89c8",
            userTo: [{
                userid: "eli",
                hash: "/e/el/eli/eli",
                jcr__path: "",
                jcr__name: "",
                firstName: "Eli",
                lastName: "Cochran",
                picture: false,
                user: "eli",
                sakai__status: "online",
                sakai__location: "none"
            }],
            userFrom: [{
                userid: "wombat",
                hash: "/w/wo/wombat",
                jcr__path: "",
                jcr__name: "",
                firstName: "wombat t.",
                lastName: "firefly",
                picture: false,
                user: "wombat",
                sakai__status: "offline",
                sakai__location: "none"
            }]
        }, {
            jcr__path: "",
            jcr__name: "",
            sakai__created: "2010-06-30T06:39:51-07:00",
            sling__resourceType: "sakai/message",
            sakai__previousmessage: "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
            sakai__messagebox: "inbox",
            sakai__type: "internal",
            sakai__category: "reminder",
            sakai__id: "c250e0204224c7ff234579f2282128359ddb89c9",
            jcr__primaryType: "",
            sakai__from: "Susan Hagstrom",
            "sakai:subject": "Peer Adviser Workshop: Experience at Cal",
            sakai__body: "Wondering why our curriculum is the way it is?",
            sakai__read: true,
            sakai__sendstate: "notified",
            "sakai:taskstate": "created",
            sakai__to: "internal:eli",
            "sakai:dueDate": "2010-08-10T06:22:46-07:00",
            sakai__completeDate: "",
            _charset_: "utf-8",
            id: "c250e0204224c7ff234579f2282128359ddb89c9",
            userTo: [{
                userid: "eli",
                hash: "/e/el/eli/eli",
                jcr__path: "",
                jcr__name: "",
                firstName: "Eli",
                lastName: "Cochran",
                picture: false,
                user: "eli",
                sakai__status: "online",
                sakai__location: "none"
            }],
            userFrom: [{
                userid: "wombat",
                hash: "/w/wo/wombat",
                jcr__path: "",
                jcr__name: "",
                firstName: "wombat t.",
                lastName: "firefly",
                picture: false,
                user: "wombat",
                sakai__status: "offline",
                sakai__location: "none"
            }]
        }, {
            jcr__path: "",
            jcr__name: "",
            sakai__created: "2010-06-30T06:39:51-07:00",
            sling__resourceType: "sakai/message",
            sakai__previousmessage: "5e49022e929f5cdcf1531867fb2e84ee18d6ce9",
            sakai__messagebox: "inbox",
            sakai__type: "internal",
            sakai__category: "reminder",
            sakai__id: "c250e0204224c7ff234579f2282128359ddb88c8",
            jcr__primaryType: "",
            sakai__from: "Susan Hagstrom",
            "sakai:subject": "Getting too many emails?",
            sakai__body: "Dear ARCH undergraduates, \n\n Following are some helpful",
            sakai__read: true,
            sakai__sendstate: "notified",
            "sakai:taskstate": "created",
            sakai__to: "internal:eli",
            "sakai:dueDate": "2010-11-30T06:22:46-07:00",
            sakai__completeDate: "",
            _charset_: "utf-8",
            id: "c250e0204224c7ff234579f2282128359ddb88c8",
            userTo: [{
                userid: "eli",
                hash: "/e/el/eli/eli",
                jcr__path: "",
                jcr__name: "",
                firstName: "Eli",
                lastName: "Cochran",
                picture: false,
                user: "eli",
                sakai__status: "online",
                sakai__location: "none"
            }],
            userFrom: [{
                userid: "wombat",
                hash: "/w/wo/wombat",
                jcr__path: "",
                jcr__name: "",
                firstName: "wombat t.",
                lastName: "firefly",
                picture: false,
                user: "wombat",
                sakai__status: "offline",
                sakai__location: "none"
            }]
        }, {
            jcr__path: "",
            jcr__name: "",
            sakai__created: "2010-06-30T06:39:23-07:00",
            sling__resourceType: "sakai/message",
            sakai__type: "internal",
            sakai__messagebox: "inbox",
            sakai__category: "reminder",
            sakai__id: "963b9e35158e98a289a5ddf646f892033bd8aae5",
            jcr__primaryType: "",
            sakai__from: "Susan Hagstrom",
            "sakai:subject": "April 23 deadline for CED commencement",
            sakai__body: "Dear CED seniors, \n\n If you would like to participate",
            sakai__read: true,
            sakai__sendstate: "notified",
            "sakai:taskstate": "created",
            sakai__to: "internal:eli",
            "sakai:dueDate": "2010-04-23T06:22:46-07:00",
            sakai__completeDate: "",
            _charset_: "utf-8",
            id: "963b9e35158e98a289a5ddf646f892033bd8aae5",
            userTo: [{
                userid: "eli",
                hash: "/e/el/eli/eli",
                jcr__path: "",
                jcr__name: "",
                firstName: "Eli",
                lastName: "Cochran",
                picture: false,
                user: "eli",
                sakai__status: "online",
                sakai__location: "none"
            }],
            userFrom: [{
                userid: "wombat",
                hash: "/w/wo/wombat",
                jcr__path: "",
                jcr__name: "",
                firstName: "wombat t.",
                lastName: "firefly",
                picture: false,
                user: "wombat",
                sakai__status: "offline",
                sakai__location: "none"
            }]
        }]
    };
    
//    function test() {
//        alert("test");
//    }    
    var fetchData = function(){
        return mockreminders;
    };
    
    var createRemindersList = function(data){
    	var htmlStr = $.TemplateRenderer("myreminders_template", data);
        $remindersList.html(htmlStr);
    };
    
    var getRemindersList = function(taskState, callback){
    	var url = sakai.config.URL.MYREMINDERS_TASKSTATE_SERVICE + "?taskState=" + taskState;
        $.ajax({
            url: url,
            cache: false,
            success: function(data) {
                if (data.results) {
                	createRemindersList(merge(data, mockreminders));
                }
                else {
                	createRemindersList(mockreminders);
                }
                if (typeof callback !== "undefined") {
                    callback();
                }

            },
            error: function(xhr, textStatus, thrownError) {
            	alert("Getting Reminders failed for category=reminders and taskstate=" + taskState + " with status=" + textStatus + 
            			" and thrownError=" + thrownError + "\n" + xhr.responseText);
            }
        });
    };

    var merge = function(reminders1, reminders2) {
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
    
    sakai.myreminders.taskDone = function(id, url) {
      updateReminder(url, "taskState", "completed", function(){
    	  $("#"+ id).slideUp("normal", function(){$(this).remove;});
      });
    }
    
    var updateReminder = function (url, propname, propvalue, callback) {
    	var data = {};
    	data[propname] = propvalue;
    	$.ajax({
    		  type: 'POST',
    		  url: url,
    		  data: data,
    		  success: function(data, textStatus, xhr){
    			  alert("updated " + propname + " to " + propvalue + "\n" + xhr.responseText);
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
    
    var doInit = function(){
        getRemindersList("created");
    };
    
    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("myreminders");
