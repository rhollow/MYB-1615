/*global $, setTitles, window */

var sakai = sakai || {};

var directory = {
    "featured" : ["bear_facts", "bspace", "calmail"],
    "links" : [
        {
            "id" : "bear_facts",
            "name" : "Bear Facts",
            "url" : "https://bearfacts.berkeley.edu/bearfacts/student/studentMain.do?bfaction=welcome",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "bspace",
            "name" : "bSpace",
            "url" : "http://bspace.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "calmail",
            "name" : "CalMail",
            "url" : "http://calmail.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "schedule_of_classes",
            "name" : "Schedule of Classes",
            "url" : "http://schedule.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "tele-bears",
            "name" : "Tele-BEARS",
            "url" : "http://telebears.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "campus_textbook_store",
            "name" : "Campus Textbook Store",
            "url" : "http://www.bkstr.com/CategoryDisplay/10001-9604-10433-1",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "how_to_pick_classes",
            "name" : "How to Pick Classes / Plan Your Schedule",
            "url" : "http://asuc.org/newsite/scheduleplanning",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "public_service",
            "name" : "Public Service",
            "url" : "http://calcorps.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "asuc",
            "name" : "ASUC (Student Government)",
            "url" : "http://www.asuc.org",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "decal",
            "name" : "DeCal Courses",
            "url" : "http://www.decal.org",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "student_groups",
            "name" : "Student Groups",
            "url" : "http://students.berkeley.edu/osl",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "campus_map",
            "name" : "Campus Map",
            "url" : "http://berkeley.edu/map",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "career_center",
            "name" : "Career Center (Jobs and Internships)",
            "url" : "http://career.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "campus_events_calendar",
            "name" : "Campus Events Calendar",
            "url" : "http://events.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "finaid_scholarships",
            "name" : "Financial Aid and Scholarships",
            "url" : "http://students.berkeley.edu/finaid",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "future_campus_calendars",
            "name" : "Future Campus Calendars",
            "url" : "http://opa.berkeley.edu/AcademicCalendar",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "course_popup_descriptions",
            "name" : "Course Catalog / popup_descriptions",
            "url" : "http://catalog.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "registrar",
            "name" : "Office of the Registrar",
            "url" : "http://registrar.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "parking_transportation",
            "name" : "Parking and Transportation",
            "url" : "http://pt.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "rsf",
            "name" : "Recreational Sports Facility (RSF)",
            "url" : "http://www.recsports.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "rescomp",
            "name" : "Residential Computing (ResComp)",
            "url" : "http://rescomp.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "library",
            "name" : "Library",
            "url" : "http://www.lib.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "campus_safety",
            "name" : "Campus Safety / Police",
            "url" : "http://police.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "uhs",
            "name" : "University Health Services (UHS)",
            "url" : "http://uhs.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "student_sites",
            "name" : "Student-sorted Sites",
            "url" : "http://www.berkeley.edu/students",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "resource_guide",
            "name" : "Resource Guide for Students",
            "url" : "http://resource.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "cal1card",
            "name" : "Cal-1-Card",
            "url" : "http://services.housing.berkeley.edu/c1c/static/index.htm",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "caldining",
            "name" : "CalDining",
            "url" : "http://caldining.berkeley.edu",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        },
        {
            "id" : "site_directory",
            "name" : "Site Directory (A-Z)",
            "url" : "http://www.berkeley.edu/a-z/a.shtml",
            "popup_description" : "Needs popup_description.",
            "featured_description" : "Needs featured_description.",
            "audience" : [
                {
                    "name" : "student",
                    "required" : "true"
                }
            ]
        }
    ]
}

function setTitles(){
    var all_labels = document.getElementsByTagName("label");
    for (k = 0; k < all_labels.length; k++) {
        all_labels[k].setAttribute("title", "Add to myLinks.");
    }
    var selected_labels = document.getElementsByClassName("LabelSelected");
    for (i = 0; i < selected_labels.length; i++) {
        selected_labels[i].setAttribute("title", "Remove from myLinks.");
    }
}

function setStars(){
    $(".CheckBoxClass").change(function(){
        if ($(this).is(":checked")) {
            $(this).next("label").addClass("LabelSelected");
        }
        else {
            $(this).next("label").removeClass("LabelSelected");
        }
        setTitles();
    })
}

function doInit(){
    setStars();
    setTitles();   
}

