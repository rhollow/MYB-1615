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

sakai.myreminders = function(tuid,showSettings){

    /////////////////////////////
    // Configuration variables //
    /////////////////////////////

    var rootel = $("#" + tuid);

    var doInit = function(){
        //var supernote = new SuperNote('supernote', {});
        //
        //// Optional custom note "close" button handler extension used in this example.
        //// This picks up click on CLASS="note-close" elements within CLASS="snb-pinned"
        //// notes, and closes the note when they are clicked.
        //// It can be deleted if you're not using it.
        //addEvent(document, 'click', function(evt){
        //    var elm = evt.target || evt.srcElement, closeBtn, note;
        //
        //    while (elm) {
        //        if ((/note-close/).test(elm.className))
        //            closeBtn = elm;
        //        if ((/snb-pinned/).test(elm.className)) {
        //            note = elm;
        //            break;
        //        }
        //        elm = elm.parentNode;
        //    }
        //
        //    if (closeBtn && note) {
        //        var noteData = note.id.match(/([a-z_\-0-9]+)-note-([a-z_\-0-9]+)/i);
        //        for (var i = 0; i < SuperNote.instances.length; i++)
        //            if (SuperNote.instances[i].myName == noteData[1]) {
        //                setTimeout('SuperNote.instances[' + i + '].setVis("' + noteData[2] +
        //                '", false, true)', 100);
        //                cancelEvent(evt);
        //            }
        //    }
        //});
    };
    doInit();
};

sakai.api.Widgets.widgetLoader.informOnLoad("myreminders");

