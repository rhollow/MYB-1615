var sakai = sakai || {};

sakai.config = sakai.config || {};

// Insert custom configuration here

// myberkeley custom begin

// This is our custom CAS log in information.
sakai.config.Authentication.internal = sakai.isDev;
sakai.config.Authentication.external = [
    {
      label: "Login using your CalNet ID",
      login_btn: "LOGIN_BTN",
      url: "/system/sling/cas/login?resource=/dev/index.html",
      description: "CAS_NOTE"
    }
];
sakai.config.Authentication.allowInternalAccountCreation = false;
sakai.config.Authentication.hideLoginOn = [
    "/dev/create_new_account.html"
];

// Page titles for myB-unique pages
sakai.config.PageTitles.pages["/dev/inboxnotifier.html"] = "NOTIFICATIONS";
sakai.config.PageTitles.pages["/dev/listpage.html"] = "MY_DYNAMIC_LISTS";
sakai.config.PageTitles.pages["/dev/links.html"] = "LINKS";

// set our own default widget arrangement
sakai.widgets.defaults.personalportal.columns = [["mylinks", "recentmessages"], ["myreminders","mygroups"]];

// conditional link for notification authoring page
sakai.config.Navigation[sakai.config.Navigation.length] = {
    "url" : "/dev/inboxnotifier.html",
    "label" : "NOTIFICATION_MANAGER",
    "requiresAdvisorMembership" : true
};

// myberkeley custom end
