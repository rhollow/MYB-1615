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
define(["config/config", "config/env"], function(config) {

    // Insert custom configuration here
    // CalCentral custom begin
    
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
    
    // Show mylinks dashboard widget by default for new users
    config.defaultprivstructure["${refid}0"]["${refid}5"].dashboard.columns.column3[1] = {
        "uid": "id63754673110789",
        "visible": "block",
        "name": "mylinks"
    };
    
    
    // Show walktime dashboard widget by default for new users
    config.defaultprivstructure["${refid}0"]["${refid}5"].dashboard.columns.column1[2] = {
        "uid": "id46750934593210789",
        "visible": "block",
        "name": "walktime"
    };    

    /*
    Temporarily using this approach to disabling the carousel - may change in future.
     */
    enabledCarousel: false;


    /*
    Inject DynamicLists and Notifications into menus;
    provide default JSON template for same.
     */

    // Add these items only for advisers
    if(!myb.api.security.isUserAnAdviser()) {
        return;
    }

    // My notifications
    config.defaultprivstructure.structure0.notifications = {
        "_title": "__MSG__MY_NOTIFICATIONS__",
        "_ref": "id1165301022",
        "_order": 2,
        "_canEdit": true,
        "_canSubedit": true,
        "_nonEditable": true,
        "drafts": {
            "_ref": "id1307490615",
            "_order": 0,
            "_title": "Drafts",
            "_canEdit": true,
            "_canSubedit": true,
            "_nonEditable": true
        },
        "queue": {
            "_ref": "id1307490615",
            "_order": 1,
            "_title": "Queue",
            "_canEdit": true,
            "_canSubedit": true,
            "_nonEditable": true
        },
        "archive": {
            "_ref": "id1307490615",
            "_order": 2,
            "_title": "Archive",
            "_canEdit": true,
            "_canSubedit": true,
            "_nonEditable": true
        },
        "trash": {
            "_ref": "id1307490615",
            "_order": 3,
            "_title": "Trash",
            "_canEdit": true,
            "_canSubedit": true,
            "_nonEditable": true
        }
    };

    // Generates Notifications widget for/with Content Authoring meta-widget
    config.defaultprivstructure.id1307490615 = {
        'rows': [{
            'id': 'id9583409539834883',
            'columns': [{
                'width': 1,
                'elements': [{
                    'id': 'idid1307490671',
                    'type': 'groupnotificationmanager'
                }, {
                    'id': 'id1307666089',
                    'type': 'composenotification'
                }]
            }]
        }]
    };


    // My dynamic lists (nav menu)
    config.defaultprivstructure.structure0.dynlists = {
            "_title": "__MSG__MY_DYNAMIC_LISTS__",
            "_ref": "id1307386414",
            "_order": 3,
            "_canEdit": true,
            "_canSubedit": true,
            "_nonEditable": true
    };


    // Generates Dynamic Lists widget for/with Content Authoring meta-widget
    config.defaultprivstructure.id1307386414 = {
        'rows': [{
            'id': 'id8965114838384',
            'columns': [{
                'width': 1,
                'elements': [{
                    'id': 'id1307386502',
                    'type': 'dynamiclistmanager'
                }, {
                    'id': 'id1307390214',
                    'type': 'dynamiclisteditor'
                }]
            }]
        }]
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
                    "url": "/categories"
                },
                {
                    "title": "GO_TO_BERKELEY_EDU",
                    "url": "http://berkeley.edu/"
                }
            ],
            getInTouch: [
                {
                    "title": "SEND_US_YOUR_FEEDBACK",
                    "url": "http://ets.berkeley.edu/calcentral-feedback"
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

    /* CalCentral specific footer links */
    config.Footer.leftLinks = [
        {
            "title": "__MSG__HELP__",
            "href": "/~Help-and-Support"
        }, {
            "title": "__MSG__FEEDBACK__",
            "href": "http://ets.berkeley.edu/calcentral-feedback",
            "newWindow": true
        }, {
            "title": "__MSG__ACKNOWLEDGEMENTS__",
            "href": "/acknowledgements"
        //}, {
        //    "title": "__MSG__SUGGEST_AN_IMPROVEMENT__",
        //    "href": "http://sakaioae.idea.informer.com/",
        //    "newWindow": true
        }
    ];
    
    // Add My Notification and My Dynamic Lists to TOP Navigation 
    config.Navigation[0].subnav.splice(2,0, 
        { 
            "url": "/me#l=notifications/drafts", 
            "id": "subnavigation_notifications_link", 
            "label": "MY_NOTIFICATIONS", 
            "requiresAdviserMembership": true 
        } 
    ); 
        config.Navigation[0].subnav.splice(3,0, 
        { 
            "url": "/me#l=dynlists", 
            "id": "subnavigation_dynlists_link", 
            "label": "MY_DYNAMIC_LISTS", 
            "requiresAdviserMembership": true 
        } 
    );

    config.Directory = {
        topic_divider: {
            divider: true,
            title: "By Topic",
            cssClass: "myb-cat-by-topic"
        },
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
                environmental_design: {
                    title: "Environmental Design"
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
                    title: "Land Use Planning, Management and Development"
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
        by_org_divider: {
            divider: true,
            title: "By Berkeley Campus Organization",
            cssClass: "myb-cat-by-org"
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


    /*
     * CalCentral Custom Profile settings
     * CalCentral uses different fields and field settings for the Personal Profile than OAE
     * "label": the internationalizable message for the entry label in HTML
     * "required": {Boolean} Whether the entry is compulsory or not
     * "display": {Boolean} Show the entry in the profile or not
     * "editable": {Boolean} Whether or not the entry is editable
     * For a date entry field use "date" as the type for MM/dd/yyyy and "dateITA" as the type for dd/MM/yyyy
     *
     */

    config.Profile.configuration.defaultConfig = {
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
            "permission": "private",
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
            "permission": "everyone",
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
            "permission": "everyone",
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
                    "label": "__MSG__TAGS_AND_CATEGORIES__",
                    "required": false,
                    "display": true,
                    "type": "tags",
                    "tagField": true
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
            "permission": "everyone",
            //"template": "profile_section_publications_template",
            "elements": {
                "maintitle": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_MAIN_TITLE__",
                    "required": true,
                    "editable": true,
                    "display": true,
                    "example": "__MSG__PROFILE_PUBLICATIONS_MAIN_TITLE_EXAMPLE__"
                },
                "mainauthor": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_MAIN_AUTHOR__",
                    "required": true,
                    "editable": true,
                    "display": true
                },
                "coauthor": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_CO_AUTHOR__",
                    "required": false,
                    "editable": true,
                    "display": true,
                    "example": "__MSG__PROFILE_PUBLICATIONS_CO_AUTHOR_EXAMPLE__"
                },
                "publisher": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_PUBLISHER__",
                    "required": true,
                    "editable": true,
                    "display": true
                },
                "placeofpublication": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_PLACE_OF_PUBLICATION__",
                    "required": true,
                    "editable": true,
                    "display": true
                },
                "volumetitle": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_VOLUME_TITLE__",
                    "required": false,
                    "editable": true,
                    "display": true
                },
                "volumeinformation": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_VOLUME_INFORMATION__",
                    "required": false,
                    "editable": true,
                    "display": true,
                    "example": "__MSG__PROFILE_PUBLICATIONS_VOLUME_INFORMATION_EXAMPLE__"
                },
                "year": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_YEAR__",
                    "required": true,
                    "editable": true,
                    "display": true
                },
                "number": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_NUMBER__",
                    "required": false,
                    "editable": true,
                    "display": true
                },
                "series title": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_SERIES_TITLE__",
                    "required": false,
                    "editable": true,
                    "display": true
                },
                "url": {
                    "label": "__MSG__PROFILE_PUBLICATIONS_URL__",
                    "required": false,
                    "editable": true,
                    "display": true,
                    "validation": "appendhttp url"
                }
            }
        }
    };
    
    // Custom CSS Files to load in
    // config.skinCSS = ["/dev/skins/default/skin.css"];
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
    
    // Hybrid
    
    config.showSakai2=false;
    config.useLiveSakai2Feeds=false;
    
    // adds the Sakai 2 Sites item to the left hand nav
    //config.defaultprivstructure["${refid}5"].dashboard.columns.column1.push({
    //    "uid": "${refid}1234",
    //    "visible": "block",
    //    "name": "mysakai2"
    //});
    //
    //config.defaultprivstructure.structure0["sakai2sites"] =  {
    //    "_ref": "${refid}2345",
    //    "_title": "My bSpace sites",
    //    "_order": 2,
    //    "_canEdit": true,
    //    "_reorderOnly": true,
    //    "_nonEditable": true,
    //    "main": {
    //        "_ref": "${refid}2345",
    //        "_order": 0,
    //        "_title": "My bSpace sites"
    //    }
    //};
    //
    //config.defaultprivstructure["${refid}2345"] = {
    //    "page": "<div id='widget_searchsakai2_${refid}2346' class='widget_inline'></div>"
    //};
    //
    //// adds the My Sakai 2 Sites menu item to the Me menu
    //config.Navigation[0].subnav.splice(2,0, {
    //    "url": "/me#l=sakai2sites",
    //    "id": "subnavigation_sakai2_link",
    //    "label": "MY_SAKAI2_SITES"
    //});

    // End CalCentral custom

    /**
     * Kaltura Settings
     */
    config.kaltura = {
        enabled: false, // Enable/disable Kaltura player
        serverURL:  "http://www.kaltura.com", //INSERT_KALTURA_INSTALLATION_URL_HERE
        partnerId:  100, //INSERT_YOUR_PARTNER_ID_HERE
        playerId: 100 //INSERT_YOUR_PLAYER_ID_HERE
    };

    return config;
});
