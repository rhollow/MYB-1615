var sakai = sakai || {};

sakai.config = sakai.config || {};

// Custom config settings unique to myBerkeley go in this file.

// This is our custom CAS log in information.
sakai.config.Authentication = {
    "internal": sakai.isDev, 
    "external": [
        {
          label: "LOGIN_TITLE",
          login_btn: "LOGIN_BTN",
          url: "/system/sling/cas/login?resource=/dev/index.html",
          description: "CAS_NOTE"
        }
    ]
};

// Page titles for myB-unique pages
sakai.config.PageTitles.pages["/dev/inboxnotifier.html"] = "NOTIFICATIONS";
sakai.config.PageTitles.pages["/dev/listpage.html"] = "MY_DYNAMIC_LISTS";
sakai.config.PageTitles.pages["/dev/links.html"] = "LINKS";

// Custom CSS Files to load in
sakai.config.skinCSS = ["/dev/skins/default/skin.css"];

// set our own default widget arrangement
sakai.widgets.defaults.personalportal.columns = [["mylinks", "recentmessages"], ["myreminders"]];

// conditional link for notification authoring page
sakai.config.Navigation[sakai.config.Navigation.length] = {
    "url" : "/dev/inboxnotifier.html",
    "label" : "NOTIFICATION_MANAGER",
    "requiresAdvisorMembership" : true
};