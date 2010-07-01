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
    var rootel = $("#" + tuid);
    var $remindersList = $(".reminders_list", rootel);
    
    // Template
    var myremindersTemplate = "myreminders_template";
    
    var mockdata = {
        "items": 25,
        "total": 5,
        "results": [{
            "jcr:path": "",
            "jcr:name": "",
            "sakai:created": "2010-06-28T01:10:49-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:type": "internal",
            "sakai:messagebox": "inbox",
            "sakai:category": "reminder",
            "sakai:id": "70896405574174eb091b85ee6be93d4f70558454",
            "jcr:primaryType": "",
            "sakai:from": "Susan Hagstrom",
            "sakai:subject": "IMPORTANT: CED Commencement tickets",
            "sakai:body": "Dear CED seniors, \n\n <b>Beginning Monday, April 26th,</b> students who have RSVPed for CED's commencement may pick up four complimentary tickets and purchase unlimited additional tickets for $5 each (you can use check or money order payable to UC Regents or your CalOne ID card). \n\n Tickets will be distributed in the Card Key Office (477 Wurster) to those who are on the RSVP list. You must show ID. \n\n <b>The RSVP deadline is today.</b> To RSVP, complete the senior survey. \n\n Graduation: <a href='http://www.ced.berkeley.edu/advising/continuingstudents/graduation'>http://www.ced.berkeley.edu/advising/continuingstudents/graduation</a> \n\n Commencement ceremony: <a href='http://www.ced.berkeley.edu/events/calendar/commencement'>http://www.ced.berkeley.edu/events/calendar/commencement</a> \n\n <b>IMPORTANT: HAVE YOU ADDED YOURSELF TO THE DEGREE LIST?</b> THIS IS DIFFERENT FROM RSVP-ING FOR THE CEREMONY! See <a href='http://www.ced.berkeley.edu/advising/continuingstudents/graduation'>http://www.ced.berkeley.edu/advising/continuingstudents/graduation</a> for more information. \n\n Congratulations graduates! \n\n Susan \n\n -- \n\n <h1>Susan Hagstrom</h1> \n\n Undergraduate Services Manager \n\n College of Environmental Design \n\n 232 Wurster \n\n UC Berkeley \n\n Berkeley, CA 94720-1800 \n\n 510-642-0408 \n\n <a href='http://www.ced.berkeley.edu/advising/'>http://www.ced.berkeley.edu/advising/</a>",
            "sakai:read": true,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "sakai:dueDate": "2010-06-28T01:10:49-07:00",
            "sakai:completeDate": "2010-06-28T01:10:49-07:00",
            "sakai:to": "internal:eli",
            "_charset_": "utf-8",
            "id": "70896405574174eb091b85ee6be93d4f70558454",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "/_user/e/el/eli/eli/public/authprofile",
                "jcr:name": "authprofile",
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
                "jcr:path": "/_user/w/wo/wombat/public/authprofile",
                "jcr:name": "authprofile",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }, {
            "jcr:path": "/_user/e/el/eli/eli/message/c2/50/e0/20/c250e0204224c7ff234579f2282128359ddb89c8",
            "jcr:name": "c250e0204224c7ff234579f2282128359ddb89c8",
            "sakai:created": "2010-06-30T06:39:51-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:previousmessage": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
            "sakai:messagebox": "inbox",
            "sakai:type": "internal",
            "sakai:category": "message",
            "sakai:id": "c250e0204224c7ff234579f2282128359ddb89c8",
            "jcr:primaryType": "nt:unstructured",
            "sakai:from": "wombat",
            "sakai:subject": "Re: Re: wombat t. firefly has invited you to become a connection",
            "sakai:body": "This is another reply",
            "sakai:read": true,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "_charset_": "utf-8",
            "id": "c250e0204224c7ff234579f2282128359ddb89c8",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "/_user/e/el/eli/eli/public/authprofile",
                "jcr:name": "authprofile",
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
                "jcr:path": "/_user/w/wo/wombat/public/authprofile",
                "jcr:name": "authprofile",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }],
            "previousMessage": {
                "jcr:path": "/_user/e/el/eli/eli/message/5e/49/02/2e/5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
                "jcr:name": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
                "sakai:created": "2010-06-30T06:33:47-07:00",
                "sling:resourceType": "sakai/message",
                "sakai:previousmessage": "70896405574174eb091b85ee6be93d4f70558454",
                "sakai:messagebox": "outbox",
                "sakai:type": "internal",
                "sakai:category": "message",
                "sakai:id": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
                "jcr:primaryType": "nt:unstructured",
                "sakai:from": "eli",
                "sakai:subject": "Re: wombat t. firefly has invited you to become a connection",
                "sakai:body": "thanks buddy",
                "sakai:read": "true",
                "sakai:sendstate": "notified",
                "sakai:to": "internal:wombat",
                "_charset_": "utf-8",
                "id": "5e49022e929f5cdcf1531867fb2e84ee18d6ced9",
                "userTo": [{
                    "userid": "wombat",
                    "hash": "/w/wo/wombat",
                    "jcr:path": "/_user/w/wo/wombat/public/authprofile",
                    "jcr:name": "authprofile",
                    "firstName": "wombat t.",
                    "lastName": "firefly",
                    "picture": false,
                    "user": "wombat",
                    "sakai:status": "offline",
                    "sakai:location": "none"
                }],
                "userFrom": [{
                    "userid": "eli",
                    "hash": "/e/el/eli/eli",
                    "jcr:path": "/_user/e/el/eli/eli/public/authprofile",
                    "jcr:name": "authprofile",
                    "firstName": "Eli",
                    "lastName": "Cochran",
                    "picture": false,
                    "user": "eli",
                    "sakai:status": "online",
                    "sakai:location": "none"
                }],
                "previousMessage": {
                    "jcr:path": "/_user/e/el/eli/eli/message/70/89/64/05/70896405574174eb091b85ee6be93d4f70558454",
                    "jcr:name": "70896405574174eb091b85ee6be93d4f70558454",
                    "sakai:created": "2010-06-30T06:22:46-07:00",
                    "sling:resourceType": "sakai/message",
                    "sakai:type": "internal",
                    "sakai:messagebox": "inbox",
                    "sakai:category": "invitation",
                    "sakai:id": "70896405574174eb091b85ee6be93d4f70558454",
                    "jcr:primaryType": "nt:unstructured",
                    "sakai:from": "wombat",
                    "sakai:subject": "wombat t. firefly has invited you to become a connection",
                    "sakai:body": "Hi, \n\n wombat t. firefly has invited you to become a connection. \nHe/She has also left the following message: \n\n I would like to invite you to become a member of my network on Sakai.\n\n- wombat t. \n\nTo accept this invitation, please click on the accept button. \n\nKind regards,\n\nThe Sakai Team",
                    "sakai:read": true,
                    "sakai:sendstate": "notified",
                    "sakai:to": "internal:eli",
                    "_charset_": "utf-8",
                    "id": "70896405574174eb091b85ee6be93d4f70558454",
                    "userTo": [{
                        "userid": "eli",
                        "hash": "/e/el/eli/eli",
                        "jcr:path": "/_user/e/el/eli/eli/public/authprofile",
                        "jcr:name": "authprofile",
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
                        "jcr:path": "/_user/w/wo/wombat/public/authprofile",
                        "jcr:name": "authprofile",
                        "firstName": "wombat t.",
                        "lastName": "firefly",
                        "picture": false,
                        "user": "wombat",
                        "sakai:status": "offline",
                        "sakai:location": "none"
                    }]
                }
            }
        }, {
            "jcr:path": "/_user/e/el/eli/eli/message/96/3b/9e/35/963b9e35158e98a289a5ddf646f892033bd8aae5",
            "jcr:name": "963b9e35158e98a289a5ddf646f892033bd8aae5",
            "sakai:created": "2010-06-30T06:39:23-07:00",
            "sling:resourceType": "sakai/message",
            "sakai:type": "internal",
            "sakai:messagebox": "inbox",
            "sakai:category": "message",
            "sakai:id": "963b9e35158e98a289a5ddf646f892033bd8aae5",
            "jcr:primaryType": "nt:unstructured",
            "sakai:from": "wombat",
            "sakai:subject": "This is a message",
            "sakai:body": "A message",
            "sakai:read": true,
            "sakai:sendstate": "notified",
            "sakai:to": "internal:eli",
            "_charset_": "utf-8",
            "id": "963b9e35158e98a289a5ddf646f892033bd8aae5",
            "userTo": [{
                "userid": "eli",
                "hash": "/e/el/eli/eli",
                "jcr:path": "/_user/e/el/eli/eli/public/authprofile",
                "jcr:name": "authprofile",
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
                "jcr:path": "/_user/w/wo/wombat/public/authprofile",
                "jcr:name": "authprofile",
                "firstName": "wombat t.",
                "lastName": "firefly",
                "picture": false,
                "user": "wombat",
                "sakai:status": "offline",
                "sakai:location": "none"
            }]
        }]
    };
    
    var activateNotes = function(){
        var supernote = new SuperNote('supernote', {});
        
        // Optional custom note "close" button handler extension used in this example.
        // This picks up click on CLASS="note-close" elements within CLASS="snb-pinned"
        // notes, and closes the note when they are clicked.
        // It can be deleted if you're not using it.
        addEvent(document, 'click', function(evt){
            var elm = evt.target || evt.srcElement, closeBtn, note;
            
            while (elm) {
                if ((/note-close/).test(elm.className)) {
                    closeBtn = elm;
                }
                if ((/snb-pinned/).test(elm.className)) {
                    note = elm;
                    break;
                }
                elm = elm.parentNode;
            }
            
            if (closeBtn && note) {
                var noteData = note.id.match(/([a-z_\-0-9]+)-note-([a-z_\-0-9]+)/i);
                for (var i = 0; i < SuperNote.instances.length; i++) {
                    if (SuperNote.instances[i].myName == noteData[1]) {
                        setTimeout('SuperNote.instances[' + i + '].setVis("' + noteData[2] +
                        '", false, true)', 100);
                        cancelEvent(evt);
                    }
                }
            }
        });
    };
    
    var createRemindersList = function(data){
        $remindersList.html($.TemplateRenderer(myremindersTemplate, data));
        activateNotes();
    };
    
    
    var getRemindersList = function(){
        sakai.api.Widgets.loadWidgetData(tuid, function(success, data){
            if (success) {
                // load the user's reminders
                createRemindersList(json);
            }
            else {
                alert("error");
            }
        });
    };
    
    
    var doInit = function(){
        getRemindersList();
    };
    
    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("myreminders");
