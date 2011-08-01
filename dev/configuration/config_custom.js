define(["config/config", "config/env"], function(config) {

    // Insert custom configuration here

    // myberkeley custom begin

    // This is our custom CAS log in information.
    config.Authentication.internal = config.isDev;
    config.Authentication.external = [
        {
          label: "Login using your CalNet ID",
          login_btn: "LOGIN_BTN",
          url: "/system/sling/cas/login?resource=/me",
          description: "CAS_NOTE"
        }
    ];
    config.Authentication.allowInternalAccountCreation = false;
    config.Authentication.hideLoginOn = [
        "/dev/create_new_account.html"
    ];
    config.allowPasswordChange = false;

    // set our own default widget arrangement
    config.defaultprivstructure["${refid}5"].dashboard.columns.column1[1] = {
      "uid": "id7813904133752",
      "visible": "block",
      "name": "mytasks"
    };

    config.defaultprivstructure["${refid}5"].dashboard.columns.column2[1] = {
        "uid": "id12893445620912",
        "visible": "block",
        "name": "myevents"
    };

    config.defaultprivstructure["${refid}5"].dashboard.columns.column3[1] = {
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
        agriculture_and_related_sciences: {
            title: "Agriculture & Related Sciences",
            children: {
                agriculture: {
                    title: "Agriculture"
                },
                soil_chemistry: {
                    title: "Soil Chemistry and Physics"
                }
            }
        },
        architecture: {
            title: "Architecture, Landscape & Planning",
            children: {
                architecture: {
                    title: "Architecture"
                },
                city_and_urban_planning: {
                    title: "City/Urban, Community & Regional Planning"
                },
                landscape_architecture: {
                    title: "Landscape Architecture"
                }
            }
        },
        bio_med_sciences: {
            title: "Biological & Biomedical Sciences",
            children: {
                biochemistry: {
                    title: "Biochemistry"
                },
                bioinformatics: {
                    title: "Bioinformatics"
                },
                biology: {
                    title: "Biology"
                },
                Biotechnology: {
                    title: "Biotechnology"
                },
                botany_plant_biology: {
                    title: "Botany/Plant Biology"
                },
                cell_and_molecular_biology: {
                    title: "Cell & Molecular Biology"
                },
                conservation_biology: {
                    title: "Conservation Biology"
                },
                ecology: {
                    title: "Ecology"
                },
                entomology: {
                    title: "Entomology"
                },
                environmental_biology: {
                    title: "Environmental Biology"
                },
                genetics: {
                    title: "Genetics"
                },
                marine_biology_and_biological_oceanography: {
                    title: "Marine Biology & Biological Oceanography"
                },
                microbiology: {
                    title: "Microbiology"
                },
                nutrition: {
                    title: "Nutrition"
                },
                physiology: {
                    title: "Physiology"
                },
                toxicology: {
                    title: "Toxicology"
                },
                virology: {
                    title: "Virology"
                },
                zoology_and_animal_biology: {
                    title: "Zoology/Animal Biology"
                }
            }
        },
        engineering: {
            title: "Engineering",
            children: {}
        },
        law: {
            title: "Law",
            children: {}
        },
        medicine_and_related_clinical_sciences: {
            title: "Medicine & Related Clinical Sciences",
            children: {
                environmental_health: {
                    title: "Environmental Health"
                },
                medicine: {
                    title: "Medicine"
                },
                nursing: {
                    title: "Nursing"
                },
                optometry: {
                    title: "Optometry"
                },
                pharmacy: {
                    title: "Pharmacy"
                },
                physical_therapy: {
                    title: "Physical Therapy"
                },
                public_health: {
                    title: "Public Health"
                },
                veterinary_medicine: {
                    title: "Veterinary Medicine"
                }
            }
        },
        natural_resources_and_conservation: {
            title: "Natural Resources & Conservation",
            children: {
                environmental_science: {
                    title: "Environmental Science"
                },
                environmental_studies: {
                    title: "Environmental Studies"
                },
                forestry: {
                    title: "Forestry"
                },
                land_use_planning_and_management: {
                    title: "Land Use Planning & Management/Development"
                },
                natural_resources_management_policy: {
                    title: "Natural Resources Management & Policy"
                },
                wildlife_and_wetlands_science: {
                    title: "Wildlife & Wetlands Science & Management"
                },
                urban_forestry: {
                    title: "Urban Forestry"
                }
            }
        },
        physical_sciences: {
            title: "Physical Sciences",
            children: {
                hydrology_and_water_resources: {
                    title: "Hydrology & Water Resources Science"
                },
                atmospheric_chemistry_and_climatology: {
                    title: "Atmospheric Chemistry & Climatology"
                }
            }
        },
        social_sciences: {
            title: "Social Sciences",
            children: {
                economics: {
                    title: "Economics"
                },
                international_and_global_studies: {
                    title: "International & Global Studies"
                },
                sociology: {
                    title: "Sociology"
                },
                urban_studies_and_affairs: {
                    title: "Urban Studies/Affairs"
                }
            }
        },
        visual_and_performing_arts: {
            title: "Visual & Performing Arts",
            children: {
                drawing: {
                    title: "Drawing"
                },
                photography: {
                    title: "Photography"
                }
            }
        },
        departments: {
            title: "Colleges & Departments",
            children: {
                ced: {
                    title: "College of Environmental Design"
                },
                cnr: {
                    title: "College of Natural Resources"
                }
            }
        },
        col_groups: {
            title: "Collaborative Groups",
            children: {}
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

    // we only want the 'simple group' template
    config.worldTemplates = config.worldTemplates.slice(0,1);

    // Custom CSS Files to load in
    config.skinCSS = ["/dev/skins/myb/skin.css"];


    // CalCentral custom Relationships
    config.Relationships = {
        /*
         * Relationships used by the add contacts widget to define what relationship the contacts can have
         */
        "contacts": [{
            "name": "Shares Interests",
            "definition": "shares an interest with me",
            "selected": true
        }, {
            "name": "Friend",
            "definition": "is my friend",
            "selected": false
        }, {
            "name": "Contact",
            "definition": "is my contact",
            "selected": false
        }, {
            "name": "Classmate",
            "definition": "is my classmate",
            "selected": false
        }, {
            "name": "Professor",
            "inverse": "Student",
            "definition": "is my professor",
            "selected": false
        }, {
            "name": "Student",
            "inverse": "Professor",
            "definition": "is my student",
            "selected": false
        }, {
            "name": "Colleague",
            "definition": "is my colleague",
            "selected": false
        }]
    };

    // End custom configuration

    return config;

});
