define(["/dev/configuration/config.js", "/dev/configuration/env.js"], function(config) {

    // Insert custom configuration here

    // myberkeley custom begin

    // This is our custom CAS log in information.
    config.Authentication.internal = config.isDev;
    config.Authentication.external = [
        {
          label: "Login using your CalNet ID",
          login_btn: "LOGIN_BTN",
          url: "/system/sling/cas/login?resource=/dev/index.html",
          description: "CAS_NOTE"
        }
    ];
    config.Authentication.allowInternalAccountCreation = false;
    config.Authentication.hideLoginOn = [
        "/dev/create_new_account.html"
    ];

    // Page titles for myB-unique pages
    config.PageTitles.pages["/dev/inboxnotifier.html"] = "NOTIFICATIONS";
    config.PageTitles.pages["/dev/listpage.html"] = "MY_DYNAMIC_LISTS";
    config.PageTitles.pages["/dev/links.html"] = "LINKS";

    // set our own default widget arrangement
    config.widgets.defaults.personalportal.columns = [["mytasks", "mylinks", "recentmessages"], ["myevents","mygroups"]];

    // conditional link for notification authoring page
    config.Navigation[config.Navigation.length] = {
        "url" : "/dev/inboxnotifier.html",
        "label" : "NOTIFICATION_MANAGER",
        "requiresAdvisorMembership" : true
    };

    // Overriding Sakai profile
    // Basic Information --------------
    config.Profile.configuration.defaultConfig.basic.elements = {
        "firstName": {
            "label": "__MSG__PROFILE_BASIC_FIRSTNAME_LABEL__",
            "required": true,
            "display": true,
            "editable": false,
            "limitDisplayLength": 50
        },
        "lastName": {
            "label": "__MSG__PROFILE_BASIC_LASTNAME_LABEL__",
            "required": true,
            "display": true,
            "editable": false,
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
            "editable": false,
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
            "editable": false
        },
        "department": {
            "label": "__MSG__PROFILE_BASIC_DEPARTMENT_LABEL__",
            "required": false,
            "display": true
        },
        "college": {
            "label": "__MSG__PROFILE_BASIC_COLLEGE_LABEL__",
            "required": false,
            "display": true,
            "editable": false
        },
        "major": {
            "label": "__MSG__PROFILE_MAJORORPROGRAM_LABEL__",
            "required": false,
            "display": true,
            "editable": false
        }
    };

    // so that user gets redirected to CAS logout
    config.followLogoutRedirects = true;

    return config;

});
