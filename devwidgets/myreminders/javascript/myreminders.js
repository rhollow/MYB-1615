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
    
    var reminders = {
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
            sakai__subject: "5th week deadline is approaching",
            sakai__body: "Dear CED Undergraduate Student,",
            sakai__read: true,
            sakai__sendstate: "notified",
            sakai__to: "internal:eli",
            sakai__dueDate: "2010-09-12T06:22:46-07:00",
            sakai__completeDate: "2010-06-30T06:22:46-07:00",
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
            sakai__subject: "CED Commencement tickets",
            sakai__body: "Dear CED seniors, \n\n <b>Beginning Monday, April</b>",
            sakai__read: true,
            sakai__sendstate: "notified",
            sakai__to: "internal:eli",
            sakai__dueDate: "2010-02-10T06:22:46-07:00",
            sakai__completeDate: "2010-06-30T06:22:46-07:00",
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
            sakai__subject: "Peer Adviser Workshop: Experience at Cal",
            sakai__body: "Wondering why our curriculum is the way it is?",
            sakai__read: true,
            sakai__sendstate: "notified",
            sakai__to: "internal:eli",
            sakai__dueDate: "2010-08-10T06:22:46-07:00",
            sakai__completeDate: "2010-06-30T06:22:46-07:00",
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
            sakai__subject: "Getting too many emails?",
            sakai__body: "Dear ARCH undergraduates, \n\n Following are some helpful",
            sakai__read: true,
            sakai__sendstate: "notified",
            sakai__to: "internal:eli",
            sakai__dueDate: "2010-11-30T06:22:46-07:00",
            sakai__completeDate: "2010-06-30T06:22:46-07:00",
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
            sakai__subject: "April 23 deadline for CED commencement",
            sakai__body: "Dear CED seniors, \n\n If you would like to participate",
            sakai__read: true,
            sakai__sendstate: "notified",
            sakai__to: "internal:eli",
            sakai__dueDate: "2010-04-23T06:22:46-07:00",
            sakai__completeDate: "2010-06-30T06:22:46-07:00",
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
    
    var fetchData = function(){
        return reminders;
    };
    
    var createRemindersList = function(data){
        $remindersList.html($.TemplateRenderer(myremindersTemplate, data));
    };
    
    var getRemindersList = function(){
        sakai.api.Widgets.loadWidgetData(tuid, function(success, data){
            if (success) {
                // load the user's reminders
                createRemindersList(data);
            }
            else {
                //alert("Error: Couldn't load reminders list from json [getRemindersList]");
                var mockdata = fetchData();
                createRemindersList(mockdata);
            }
        });
    };
    
    var doInit = function(){
        getRemindersList();
    };
    
    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("myreminders");
