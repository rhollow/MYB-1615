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

// Overriding Sakai profile
// Basic Information --------------
sakai.config.Profile.configuration.defaultConfig.basic.elements = {
    "firstName": {
        "label": "__MSG__PROFILE_BASIC_FIRSTNAME_LABEL__",
        "required": true,
        "display": true,
		"readonly": true,
        "limitDisplayLength": 50
    },
    "lastName": {
        "label": "__MSG__PROFILE_BASIC_LASTNAME_LABEL__",
        "required": true,
        "display": true,
		"readonly": true,
        "limitDisplayLength": 50
    },
    "picture": {
        "label": "__MSG__PROFILE_BASIC_PICTURE_LABEL__",
        "required": false,
        "display": false
    },
    "preferredName": {
        "label": "__MSG__PROFILE_BASIC_PREFERREDNAME_LABEL__",
        "required": false,
        "display": true
    },
    "email": {
        "label": "__MSG__PROFILE_BASIC_EMAIL_LABEL__",
        "required": false,
        "display": true,
		"readonly": true,
        "type": "email"
    },
    "status": {
        "label": "__MSG__PROFILE_BASIC_STATUS_LABEL__",
        "required": false,
        "display": false
    },
    "role": {
        "label": "__MSG__PROFILE_BASIC_ROLE_LABEL__",
        "required": false,
        "display": true,
		"readonly": true
    },
    "department": {
        "label": "__MSG__PROFILE_BASIC_DEPARTMENT_LABEL__",
        "required": false,
        "display": true,
		"readonly": true
    },
    "college": {
        "label": "__MSG__PROFILE_BASIC_COLLEGE_LABEL__",
        "required": false,
        "display": true,
		"readonly": true
    },
	"major": {
        "label": "__MSG__PROFILE_MAJORORPROGRAM_LABEL__",
        "required": false,
        "display": true,	            
		"readonly": true	            
    }
};
// myberkeley custom end
