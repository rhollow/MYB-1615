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
    "preferredName": {
        "label": "__MSG__PROFILE_BASIC_PREFERREDNAME_LABEL__",
        "required": false,
        "display": true
    }
};

// Email address (new category)
sakai.config.Profile.configuration.defaultConfig.email = {
    "label": "__MSG__PROFILE_EMAIL_LABEL__",
    "required": true,
    "display": true,
    "access": "everybody",
    "modifyacl": true,
    "elements": {
		"email": {
            "label": "__MSG__PROFILE_BASIC_EMAIL_LABEL__",
            "required": false,
            "display": true,
			"readonly": true,
            "type": "email"
        }
	}
};

// Institutional information (new category)
sakai.config.Profile.configuration.defaultConfig.institutionalInfo = {
    "label": "__MSG__PROFILE_INSTITUTIONALINFO_LABEL__",
    "required": true,
    "display": true,
    "access": "contacts",
    "modifyacl": true,
    "elements": {
		"role": {
	        "label": "__MSG__PROFILE_BASIC_ROLE_LABEL__",
	        "required": false,
	        "display": true,
			"readonly": true,
	        "type": "select",
	        "select_elements": {
	            "academic_related_staff":"__MSG__PROFILE_BASIC_ROLE_ACADEMIC_RELATED_STAFF_LABEL__",
	            "academic_staff":"__MSG__PROFILE_BASIC_ROLE_ACADEMIC_STAFF_LABEL__",
	            "assistent_staff":"__MSG__PROFILE_BASIC_ROLE_ASSISTENT_STAFF_LABEL__",
	            "graduate_student":"__MSG__PROFILE_BASIC_ROLE_GRADUATE_STUDENT_LABEL__",
	            "undergraduate_student":"__MSG__PROFILE_BASIC_ROLE_UNDERGRADUATE_STUDENT_LABEL__",
	            "non_academic_staff":"__MSG__PROFILE_BASIC_ROLE_NON_ACADEMIC_STAFF_LABEL__",
	            "postgraduate_student":"__MSG__PROFILE_BASIC_ROLE_POSTGRADUATE_STUDENT_LABEL__",
	            "research_staff":"__MSG__PROFILE_BASIC_ROLE_RESEARCH_STAFF_LABEL__",
	            "other":"__MSG__PROFILE_BASIC_ROLE_OTHER_LABEL__"
	        }
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
	            "type": "select",
				"readonly": true,
	            "select_elements": {
	                "academic_related_staff":"__MSG__PROFILE_BASIC_ROLE_ACADEMIC_RELATED_STAFF_LABEL__",
	                "academic_staff":"__MSG__PROFILE_BASIC_ROLE_ACADEMIC_STAFF_LABEL__",
	                "assistent_staff":"__MSG__PROFILE_BASIC_ROLE_ASSISTENT_STAFF_LABEL__",
	                "graduate_student":"__MSG__PROFILE_BASIC_ROLE_GRADUATE_STUDENT_LABEL__",
	                "undergraduate_student":"__MSG__PROFILE_BASIC_ROLE_UNDERGRADUATE_STUDENT_LABEL__",
	                "non_academic_staff":"__MSG__PROFILE_BASIC_ROLE_NON_ACADEMIC_STAFF_LABEL__",
	                "postgraduate_student":"__MSG__PROFILE_BASIC_ROLE_POSTGRADUATE_STUDENT_LABEL__",
	                "research_staff":"__MSG__PROFILE_BASIC_ROLE_RESEARCH_STAFF_LABEL__",
	                "other":"__MSG__PROFILE_BASIC_ROLE_OTHER_LABEL__"
	            }
        }
	}
};


// Rearranging the sections
sakai.config.Profile.configuration.defaultConfig = {
	 "basic": sakai.config.Profile.configuration.defaultConfig.basic,
	 "email": sakai.config.Profile.configuration.defaultConfig.email,
	 "institutionalInfo": sakai.config.Profile.configuration.defaultConfig.institutionalInfo,
	 "aboutme": sakai.config.Profile.configuration.defaultConfig.aboutme,
	 "publications": sakai.config.Profile.configuration.defaultConfig.publications
};




// myberkeley custom end
