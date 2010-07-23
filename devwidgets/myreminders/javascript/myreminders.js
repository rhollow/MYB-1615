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
    
    var formatDate = function(datetime) {
        var months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
        var date = months[(parseInt(datetime.substring(5,7),10))-1]+" "+datetime.substring(8,10)+", "+datetime.substring(0,4)+" "+datetime.substring(11,19);
        var d = new Date(date);
        return d;
    }

    var getDateString = function(datetime){
        var days_short = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

        var d = formatDate(datetime);
        var dstring = days_short[d.getDay()] + " " + (d.getMonth() + 1) + "/" + d.getDate() + "/" + datetime.substring(2,4);
        return dstring;
    }

    sakai.myreminders.compareDates = function(dueDate) {
        var dueDate = formatDate(dueDate);
        var today = new Date();

        return (today > dueDate) ? "pastDue" : "";
    }


    var reminders = {
        "items": 25,
        "total": 3,
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
    
    sakai.myreminders.getDateString = function(date){
        var days_short = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
        
        var d = new Date(date);
        // something wrong with grabbing substring of year
        var dateString = days_short[d.getDay()] + " " + (d.getMonth() + 1) + "/" + d.getDate() + "/" + d.getFullYear();
        return dateString;
    }
    
    sakai.myreminders.compareDates = function(date){
        var dueDate = new Date(date);
        var today = new Date();
        
        return (today > dueDate) ? "pastDue" : "";
    }
    
    
    var lastShown = null;
    var showSnippet = function(id){
        if ($("#snippetDiv_" + id).is(":visible")) {
            $("#li_" + id).removeClass("slideUpButton");
            $("#showSnippetDiv_" + id).attr("title", "Show snippet");
            $("#snippetDiv_" + id).slideUp("normal");
            lastShown = null;
        }
        else {
            if (lastShown) {
                $("#li_" + lastShown).removeClass("slideUpButton");
                $("#showSnippetDiv_" + lastShown).attr("title", "Show snippet");
                $("#snippetDiv_" + lastShown).slideUp("normal");
            }
            lastShown = id;
            $("#li_" + id).addClass("slideUpButton");
            $("#showSnippetDiv_" + id).attr("title", "Hide snippet");
            $("#snippetDiv_" + id).slideDown("normal");
        }
    }
    
    $(".s3s-reminder-checkbox").live("click", function(evt){
        var id = evt.target.id;
        id = id.split("_");
        
        var reminderDiv = $("#div_" + id[id.length - 1]);
        var reminderData = reminderDiv.data("data");
        var jcr_path = reminderData["jcr:path"];
        
        reminderDiv.slideUp("normal", function(){
            reminderDiv.remove;
        });
    })
    
    $(".slideDownButton").live("click", function(evt){
        var id = evt.target.id;
        id = id.split("_");
        
        showSnippet(id[id.length - 1]);
    })
    
    var createRemindersList = function(data){
        $remindersList.html($.TemplateRenderer(myremindersTemplate, data));
        
        var results_length = data.results.length;
        for (var i = 0; i < results_length; i++) {
            $("#div_" + data.results[i].id).data("data", data.results[i]);
        }
    };
    
    var fetchData = function(){
        return reminders;
    };
    
    var getRemindersList = function(){
        sakai.api.Widgets.loadWidgetData(tuid, function(success, data){
            if (success) {
                // load the user's reminders
                createRemindersList(data);
            }
            else {
                //alert("Error: Couldn't load reminders list from json [getRemindersList]");
                var mockreminders = fetchData();
                createRemindersList(mockreminders);
            }
        });
    };
    
    var doInit = function(){
        getRemindersList();
    };
    
    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("myreminders");
