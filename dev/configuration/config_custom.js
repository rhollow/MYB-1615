define(["/dev/configuration/config.js", "/dev/configuration/env.js"], function(config) {

    // Insert custom configuration here

    // myberkeley custom begin

    // This is our custom CAS log in information.
    config.Authentication.internal = config.isDev;
    config.Authentication.external = [
        {
          label: "Login using your CalNet ID",
          login_btn: "LOGIN_BTN",
          url: "/system/sling/cas/login?resource=/dev/me.html",
          description: "CAS_NOTE"
        }
    ];
    config.Authentication.allowInternalAccountCreation = false;
    config.Authentication.hideLoginOn = [
        "/dev/create_new_account.html"
    ];
    config.allowPasswordChange = false;

    // set our own default widget arrangement
    config.defaultprivstructure.id546341435.dashboard.columns.column1[1] = {
      "uid": "id7813904133752",
      "visible": "block",
      "name": "mytasks"
    };

    config.defaultprivstructure.id546341435.dashboard.columns.column2[1] = {
        "uid": "id12893445620912",
        "visible": "block",
        "name": "myevents"
    };

    config.defaultprivstructure.id546341435.dashboard.columns.column3[1] = {
        "uid": "id63754673110789",
        "visible": "block",
        "name": "mylinks"
    };

    /*
     * These links are displayed in the 403 and 404 error pages.
     */
    config.ErrorPage = {
        Links: {
            whatToDo: [
                {
                    "title": "EXPLORE_MYBERKELEY",
                    "url": "/"
                },
                {
                    "title": "BROWSE_MYBERKELEY_CATEGORIES",
                    "url": "/dev/allcategories.html"
                },
                {
                    "title": "GO_TO_BERKELEY_EDU",
                    "url": "http://berkeley.edu/"
                }
            ],
            getInTouch: [
                {
                    "title": "SEND_US_YOUR_FEEDBACK",
                    "url": "mailto:portal@berkeley.edu"
                },
                {
                    "title": "CONTACT_SUPPORT",
                    "url": "mailto:portal@berkeley.edu"
                }
            ]
        }
    };

    // so that user gets redirected to CAS logout
    config.followLogoutRedirects = true;

    // remove the SIGN UP feature for anonymous users
    delete(config.Navigation[4]);

    config.Navigation[0].label = "ME";

    // Add My Notification and My Dynamic Lists to Navigation
    config.Navigation[0].subnav.splice(2,0,
        {
            "url": "/dev/me.html#l=notifications/drafts",
            "id": "subnavigation_notifications_link",
            "label": "MY_NOTIFICATIONS",
            "requiresAdviserMembership": true
        }
    );
    config.Navigation[0].subnav.splice(3,0,
        {
            "url": "/dev/me.html#l=dynlists",
            "id": "subnavigation_dynlists_link",
            "label": "MY_DYNAMIC_LISTS",
            "requiresAdviserMembership": true
        }
    );

	config.Directory = {
		ced: {
			title: "Environmental Design",
			children: {
				architecture: {
					title: "Architecture"
				},
				cityregionalplanning: {
					title: "City &amp; Regional Planning"
				},
				landscapearchitecture: {
					title: "Landscape Architecture & Environmental Planning"
				},
				urbandesign: {
					title: "Urban Design"
				}
			}
		},
		cnr: {
			title: "Natural Resources",
			children: {
				agriculturaleconomics: {
					title: "Agricultural and Resource Economics"
				},
				environmentalscience: {
					title: "Environmental Science, Policy, &amp; Management"
				},
				nutritionalscience: {
					title: "Nutritional Science &amp; Toxicology"
				},
				plantmicrobialbiology: {
					title: "Plant &amp; Microbial Biology"
				}
			}
		},
		studentservices: {
			title: "Student Groups",
			children: {
				stg_academic: {
					title: "Academic"
				},
				stg_arts: {
					title: "Arts"
				},
				stg_cultural: {
					title: "Cultural"
				},
				stg_political: {
					title: "Political"
				},
				stg_professional: {
					title: "Professional"
				},
				stg_sport: {
					title: "Sport"
				},
				stg_religious: {
					title: "Religious"
				},
				stg_service: {
					title: "Service"
				},
				stg_other: {
					title: "Other"
				}
			}
		},
		studentgroups: {
			title: "Student Services",
			children: {
				ss_academic: {
					title: "Academic"
				},
				sd_career: {
					title: "Career"
				},
				ss_financial: {
					title: "Financial"
				},
				ss_campuslife: {
					title: "Campus Life"
				}
			}
		}
	};

    config.Profile = {
        /*
         * This is a collection of profile configuration functions and settings
         * The structure of the config object is identical to the storage object
         * When system/me returns profile data for the logged in user the profile_config and profile_data objects could be merged
         * "label": the internationalizable message for the entry label in HTML
         * "required": {Boolean} Whether the entry is compulsory or not
         * "display": {Boolean} Show the entry in the profile or not
         * "editable": {Boolean} Whether or not the entry is editable
         * For a date entry field use "date" as the type for MM/dd/yyyy and "dateITA" as the type for dd/MM/yyyy
         *
         */
        configuration: {

            defaultConfig: {

                "basic": {
                    "label": "__MSG__PROFILE_BASIC_LABEL__",
                    "required": true,
                    "display": true,
                    "access": "everybody",
                    "modifyacl": false,
                    "order": 0,
                    "elements": {
                        "firstName": {
                            "label": "__MSG__PROFILE_BASIC_FIRSTNAME_LABEL__",
                            "required": true,
                            "display": true,
                            "limitDisplayLength": 50,
                            "editable": false
                        },
                        "lastName": {
                            "label": "__MSG__PROFILE_BASIC_LASTNAME_LABEL__",
                            "required": true,
                            "display": true,
                            "limitDisplayLength": 50,
                            "editable": false
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
                        "status": {
                            "label": "__MSG__PROFILE_BASIC_STATUS_LABEL__",
                            "required": false,
                            "display": false
                        }
                    }
                },
                "email" :{
                    "label": "Email Address",
                    "required": true,
                    "display": true,
                    "access": "everybody",
                    "modifyacl": false,
                    "order": 1,
                    "elements": {
                        "email": {
                            "label": "Email",
                            "required": true,
                            "display": true,
                            "limitDisplayLength": 50,
                            "editable": false
                        }
                    }
                },
                "institutional" : {
                    "label": "Institutional Information",
                    "required": true,
                    "display": true,
                    "access": "everybody",
                    "modifyacl": false,
                    "order": 2,
                    "elements": {
                        "role": {
                            "label": "Role/position",
                            "required": true,
                            "display": true,
                            "limitDisplayLength": 50,
                            "editable": false
                        },
                        "college": {
                            "label": "College",
                            "required": true,
                            "display": true,
                            "limitDisplayLength": 50,
                            "editable": false
                        },
                        "major": {
                            "label": "Major",
                            "required": true,
                            "display": true,
                            "limitDisplayLength": 50,
                            "editable": false
                        }
                    }
                },
                "aboutme": {
                    "label": "__MSG__PROFILE_ABOUTME_LABEL__",
                    "required": true,
                    "display": true,
                    "access": "everybody",
                    "modifyacl": true,
                    "order": 3,
                    "elements": {
                        "aboutme": {
                            "label": "__MSG__PROFILE_ABOUTME_LABEL__",
                            "required": false,
                            "display": true,
                            "type": "textarea"
                        },
                        "academicinterests": {
                            "label": "__MSG__PROFILE_ABOUTME_ACADEMICINTERESTS_LABEL__",
                            "required": false,
                            "display": true,
                            "type": "textarea"
                        },
                        "personalinterests": {
                            "label": "__MSG__PROFILE_ABOUTME_PERSONALINTERESTS_LABEL__",
                            "required": false,
                            "display": true,
                            "type": "textarea"
                        },
                        "hobbies": {
                            "label": "__MSG__PROFILE_ABOUTME_HOBBIES_LABEL__",
                            "required": false,
                            "display": true
                        },
                        "tags": {
                            "label": "__MSG__TAGS__",
                            "required": false,
                            "display": true,
                            "type": "textarea",
                            "tagField": true
                        }
                    }
                },
                "locations": {
                    "label": "__MSG__PROFILE_LOCATIONS_LABEL__",
                    "required": false,
                    "display": true,
                    "access": "everybody",
                    "modifyacl": true,
                    "multiple": true,
                    "directory": true,
                    "multipleLabel": "__MSG__PROFILE_LOCATION_LABEL__",
                    "order": 4,
                    "elements": {
                        "locationtitle": {
                            "label": "__MSG__PROFILE_LOCATION_LABEL__",
                            "required": true,
                            "display": true,
                            "type": "location"
                        }
                    }
                },
                "publications": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_LABEL__",
                    "required": false,
                    "display": true,
                    "access": "everybody",
                    "modifyacl": true,
                    "multiple": true,
                    "multipleLabel": "__MSG__PROFILE_PUBLICATION_LABEL__",
                    "order": 5,
                    //"template": "profile_section_publications_template",
                    "elements": {
                        "maintitle": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_MAIN_TITLE__",
                            "required": true,
                            "display": true,
                            "example": "__MSG__PROFILE_PUBLICATIONS_MAIN_TITLE_EXAMPLE__"
                        },
                        "mainauthor": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_MAIN_AUTHOR__",
                            "required": true,
                            "display": true
                        },
                        "coauthor": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_CO_AUTHOR__",
                            "required": false,
                            "display": true,
                            "example": "__MSG__PROFILE_PUBLICATIONS_CO_AUTHOR_EXAMPLE__"
                        },
                        "publisher": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_PUBLISHER__",
                            "required": true,
                            "display": true
                        },
                        "placeofpublication": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_PLACE_OF_PUBLICATION__",
                            "required": true,
                            "display": true
                        },
                        "volumetitle": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_VOLUME_TITLE__",
                            "required": false,
                            "display": true
                        },
                        "volumeinformation": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_VOLUME_INFORMATION__",
                            "required": false,
                            "display": true,
                            "example": "__MSG__PROFILE_PUBLICATIONS_VOLUME_INFORMATION_EXAMPLE__"
                        },
                        "year": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_YEAR__",
                            "required": true,
                            "display": true
                        },
                        "number": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_NUMBER__",
                            "required": false,
                            "display": true
                        },
                        "series title": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_SERIES_TITLE__",
                            "required": false,
                            "display": true
                        },
                        "url": {
                            "label": "__MSG__PROFILE_PUBLICATIONS_URL__",
                            "required": false,
                            "display": true,
                            "validation": "appendhttp url"
                        }
                    }
                }
            }
        },
        /*
         * set what name to display where only the first name is used
         */
        userFirstNameDisplay: "firstName",

        /*
         * set how the user's name is displayed across the entire system
         * - values can be compound, like "firstName lastName" or singular like "displayName"
         */
        userNameDisplay: "firstName lastName",

        /*
         * the default, if the user doesn't have the userNameDisplay property set in their
         * profile, use this one.
         * Note: the value for userNameDisplay and this value can be the same.
         *       If neither exists, nothing will show
         */
        userNameDefaultDisplay: "firstName lastName",

        /*
         * Set the user's short description to appear underneath their name
         * in search results
         */
        userShortDescription: "${role} in ${department} at ${college}",
        groupShortDescription: "asdf"
    };

    return config;

});
