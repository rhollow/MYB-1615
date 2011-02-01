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

/**
 * Initialize the Library Reference Chat widget
 * @param {String} tuid unique id of the widget
 * @param {Boolean} showSettings show the settings of the widget or not
 */
sakai.libraryreferencechat = function(tuid, showSettings){
    var QPWVERSION = 2;
    var QPBootstrap = {
        embed: function(){
            var i, g;
            function d(n){
                var l = d.options, e = l.parser[l.strictMode ? "strict" : "loose"].exec(n), k = {}, j = 14;
                while (j--) {
                    k[l.key[j]] = e[j] || ""
                }
                k[l.q.name] = {};
                k[l.key[12]].replace(l.q.parser, function(o, m, p){
                    if (m) {
                        k[l.q.name][m] = p
                    }
                });
                return k
            }
            d.options = {
                strictMode: true,
                key: ["source", "protocol", "authority", "userInfo", "user", "password", "host", "port", "relative", "path", "directory", "file", "query", "anchor"],
                q: {
                    name: "queryKey",
                    parser: /(?:^|&)([^&=]*)=?([^&]*)/g
                },
                parser: {
                    strict: /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
                    loose: /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/
                }
            };
            function f(e){
                var o = {};
                if (e.length == 0) {
                    return
                }
                e = e.replace(/\+/g, " ");
                var k = e.split("&");
                for (var l = 0; l < k.length; l++) {
                    var n = k[l].split("=");
                    var j = decodeURIComponent(n[0]);
                    var m = (n.length == 2) ? decodeURIComponent(n[1]) : j;
                    o[j] = m
                }
                return o
            }
            function c(e){
                return e.protocol + "://" + e.host + (((e.port != null) && (e.port != "")) ? ":" + e.port : "")
            }
            try {
				var h = $("#qp.bootstrap"); // changed to use jquery selector instead of getElementById - Nicole
				h.src = makeSource(); // hard-coded to get it to work - Nicole
				var g = d(h.src);
				var a = f(g.query);
				var b = document.getElementById("qpchatwidget");
				switch (a.size) {
					case ("fill"):
						b.style.width = "100%";
						b.style.height = "100%";
						break;
					case ("small"):
						b.style.width = "160px";
						b.style.height = "250px";
						break;
					case ("standard"):
					default:
						b.style.width = "190px";
						b.style.height = "275px";
						break;
				}
				b.innerHTML += "<iframe id='livechat' style='background:transparent;overflow:hidden;border:0px;height:100%;width:100%' frameborder='0' border='0' marginwidth='0' marginheight='0' width='100%' height='100%' allowTransparency='true' src='" + c(g) + "/crs/qwidget/qwidget.jsp?" + g.query + "'></iframe>"; 
			} catch (h) {
			}
        }
    };
	
	// dynamically generate url string - Nicole
	var makeSource = function() {
		var src = "http://www.questionpoint.org/crs/js/qwidget/qp.bootstrap.js?langcode=1&instid=12566&skin=black&size=fill&customSkin=";
		var protocol = location.protocol;
		var port = "";
		if (protocol === "http:") {
			port = ":" + location.port;
		} else if (protocol != "https:") {
			alert("Error: Site protocol not recognized.");
			return;
		}
		src += protocol + "//" + location.hostname + port + escape("/devwidgets/libraryreferencechat/css");
		return src;
	}
	
    QPBootstrap.onload = window.onload;
    // original code; window.onload doesn't fire when this script is embedded in a widget.
	// added an init function instead to do the same thing. - Nicole
	/*window.onload = function(){
        QPBootstrap.embed();
        if (QPBootstrap.onload != null) {
            QPBootstrap.onload()
        }
    };*/
	
	var init = function() {
		QPBootstrap.embed();
        if (QPBootstrap.onload != null) {
            QPBootstrap.onload();
        }
	}
	init();
}

sakai.api.Widgets.widgetLoader.informOnLoad("libraryreferencechat");
