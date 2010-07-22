var sakai = sakai || {};

sakai.links = function(){
    
    // page elements
    var $suggestedSites = $(".suggested_sites"); 
    var $allSites = $(".all_sites"); 
    
    // templates
    var suggested_sites_template = "suggestedsites_links_template"; 
    var all_sites_template = "allsites_links_template"; 
    
    // data files and paths
    var userLinks = "my_links";
    var linksDataNode = "/_user" + sakai.data.me.profile.path + "/private/" + userLinks;
    
    var directory = {
        colset: [1, 2, 3, 4], // use single integer
        featured: ["calmail", "dars", "cal1card"],
        links: [{
            id: "asuc",
            name: "ASUC",
            url: "http://www.asuc.org",
            popup_description: "UC Berkeley's student government home.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "bear_facts",
            name: "Bear Facts",
            url: "https://bearfacts.berkeley.edu/bearfacts/student/studentMain.do?bfaction=welcome",
            popup_description: "See your registration and financial info, grades, online bill, and personal data.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: true
            }]
        }, {
            id: "atoz_sites",
            name: "Berkeley Sites (A-Z)",
            url: "http://www.berkeley.edu/a-z/a.shtml",
            popup_description: "Comprehensive list of official campus websites.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: true
            }]
        }, {
            id: "student_services",
            name: "Student Services",
            url: "http://www.berkeley.edu/students",
            popup_description: "Several student sites grouped by function.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "bspace",
            name: "bSpace",
            url: "http://bspace.berkeley.edu",
            popup_description: "Homework assignments, lecture slides, syllabi, and class resources.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: true
            }]
        }, {
            id: "cal1card",
            name: "Cal-1-Card",
            url: "http://services.housing.berkeley.edu/c1c/static/aboutc1c.htm",
            popup_description: "Manage and learn about your Cal 1 Card.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "caldining",
            name: "CalDining",
            url: "http://caldining.berkeley.edu",
            popup_description: "View daily menus and check your meal points.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "calmail",
            name: "CalMail",
            url: "http://calmail.berkeley.edu",
            popup_description: "Read and manage your university email.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "campus_calendar",
            name: "Campus Events Calendar",
            url: "http://events.berkeley.edu",
            popup_description: "View upcoming events on campus.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "campus_map",
            name: "Campus Map",
            url: "http://berkeley.edu/map",
            popup_description: "View campus maps and find buildings using the interactive map.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "campus_safety",
            name: "Campus Safety / Police",
            url: "http://police.berkeley.edu",
            popup_description: "Learn about UCPD services.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "campus_bookstore",
            name: "Campus Bookstore",
            url: "http://www.bkstr.com/CategoryDisplay/10001-9604-10433-1",
            popup_description: "Find and purchase textbooks by course.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "course_catalog",
            name: "Course Catalog",
            url: "http://sis.berkeley.edu/catalog/gcc_search_menu",
            popup_description: "Detailed course descriptions.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "dars",
            name: "DARS",
            url: "https://marin.berkeley.edu/darsweb/servlet/ListAuditsServlet",
            popup_description: "Track progress toward a major.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "decal",
            name: "DeCal Courses",
            url: "http://www.decal.org",
            popup_description: "Explore student-taught courses.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "finaid",
            name: "Financial Aid",
            url: "http://students.berkeley.edu/finaid",
            popup_description: "Learn about financial aid and scholarships.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "future_campus_calendars",
            name: "Future Campus Calendars",
            url: "http://opa.berkeley.edu/AcademicCalendar",
            popup_description: "Look at campus calendars for upcoming semesters.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "uhs",
            name: "University Health Services",
            url: "http://uhs.berkeley.edu",
            popup_description: "Health and medical services for students.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "career_center",
            name: "Career Center",
            url: "http://career.berkeley.edu",
            popup_description: "Find jobs, internships, and learn about career paths.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "library",
            name: "Library",
            url: "http://www.lib.berkeley.edu",
            popup_description: "Search for materials in the UC library system.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "parking",
            name: "Parking",
            url: "http://pt.berkeley.edu/park",
            popup_description: "Find and pay for parking.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "public_service",
            name: "Public Service",
            url: "http://calcorps.berkeley.edu",
            popup_description: "Get involved in public service.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "rsf",
            name: "Recreational Sports Facility",
            url: "http://www.recsports.berkeley.edu",
            popup_description: "Explore RSF programs and intramural sports.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "rescomp",
            name: "Residential Computing",
            url: "http://rescomp.berkeley.edu",
            popup_description: "Computing help for the residence halls.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "class_schedule",
            name: "Schedule of Classes",
            url: "http://schedule.berkeley.edu",
            popup_description: "See current and upcoming courses.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "schedule_planning",
            name: "Schedule Planning Tools",
            url: "http://asuc.org/newsite/scheduleplanning",
            popup_description: "Tools to help you plan your class schedule.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "student_groups",
            name: "Student Groups and Programs",
            url: "http://students.berkeley.edu/osl",
            popup_description: "Student organizations, leadership programs, and GenEq.",
            featured_description: "Needs featured_description.",
            selected: false,
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "resource_guide",
            name: "Resource Guide for Students",
            url: "http://resource.berkeley.edu",
            popup_description: "Information about campus resources.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: false
            }]
        }, {
            id: "tele-bears",
            "name": "Tele-BEARS",
            url: "http://telebears.berkeley.edu",
            popup_description: "Register for classes.",
            featured_description: "Needs featured_description.",
            audience: [{
                name: "student",
                required: true
            }]
        }]
    };

    
   /**
     * Write the users links to JCR.
     * @param {object} the current state of the users list
    */
    var saveLinkList = function (updatedList) {
        sakai.api.Server.saveJSON(linksDataNode, updatedList);
    };      
    
    /**
     * Sets the titles for the checkboxes' tooltips, based on
     * whether its label's class specifies that it is checked
     * or unchecked.
     */
    var setTitles = function(){
        var all_labels = document.getElementsByTagName("label");
        for (var k=0;k<all_labels.length;k++) {
            if (all_labels[k].className !== "LabelRequired") {
                all_labels[k].setAttribute("title", "Add to myLinks.");
            }
        }
        var selected_labels = document.getElementsByClassName("LabelSelected");
        for (var i=0;i<selected_labels.length;i++) {
                selected_labels[i].setAttribute("title", "Remove from myLinks.");
        }
    };
    
    /** 
     * Pre-sets appropriate label class, then as user checks or unchecks
     * boxes, will toggle the label class as needed. Calls setTitles() after
     * every change to ensure tooltips are always correct. Also updates 
     * the user's data for the widget link list.
     * @param {Object} the current state of the users list
     */
    var setStars = function(data){
        var listObj = data;        
        var allLinks = getDirectory();         
        $(".CheckBoxClass").change(function(data){
            var id = $(this).next("label").attr("id");                   
            if ($(this).is(":checked")) {              
                $(this).next("label").addClass("LabelSelected");                
                for(var i=0;i<allLinks.links.length;i++){
                    if(id===allLinks.links[i].id || id===allLinks.links[i].id+"_label"){
                        var toAdd = {
                            id :   allLinks.links[i].id,
                            name : allLinks.links[i].name,
                            url :  allLinks.links[i].url
                        };
                        listObj.links.push(toAdd);
                    }
                }
            }
            else {
                $(this).next("label").removeClass("LabelSelected");               
                var newList = [];       
                for(var i=0;i<listObj.links.length;i++){
                    if(id===listObj.links[i].id || id===listObj.links[i].id+"_label"){
                        // do nothing...
                    }                                      
                    else{
                        var toAdd = {
                            id :   listObj.links[i].id,
                            name : listObj.links[i].name,
                            url :  listObj.links[i].url
                        };                        
                        newList.push(toAdd);
                    }
                }
                listObj.links = newList;         
                setUnchecked(listObj);       
            }          
            setTitles();                          
            saveLinkList(listObj); 
            setChecked(listObj);           
        });
    };   

    /**
     * Goes through the user's widget list and checks the appropriate stars
     * on the page. Calls setTitles at the end to ensure updated tooltips.
     * Also used to help keep featured links and all links section in sync.
     * @param {Object} the current state of the users list
     */
    var setChecked = function(data){
        $("input[type=checkbox]").each(function(){
            var id = $(this).next("label").attr("id");
            for (var i = 0; i < data.links.length; i++) {
                if (id === data.links[i].id || id === data.links[i].id + "_label") {
                    $(this).attr("checked", true);
                    if ($(this).next("label").attr("class") !== "LabelRequired") {
                        $(this).next("label").addClass("LabelSelected");
                    }
                }
            }
        });
        setTitles();
    };
    
    /**
     * Is only called when a star is unchecked to ensure the featured links 
     * and all links sections are constantly synced.
     * @param {Object} data
     */
    var setUnchecked = function(data){
        $("input[type=checkbox]").each(function(){
            var id = $(this).next("label").attr("id");
            if ($(this).next("label").attr("class") !== "LabelRequired") {
                $(this).next("label").removeClass("LabelSelected");
                $(this).attr("checked", false);
            }
        });
    };
    
    var getDirectory = function () {
         return directory;
    };
    
    /**
     * Runs through the trimpath functions to populate the page with
     * the proper data.
     * @param {Object} all links
     * @param {Object} users data for the widget
     */
    var createDirectory = function (directory, data) {     
        $suggestedSites.html($.TemplateRenderer(suggested_sites_template, directory));
        $allSites.html($.TemplateRenderer(all_sites_template, directory));
        setChecked(data);
        setTitles();
        setStars(data);
    };    
    
    /**
     * Retreives the current list of links for the current user
     * if the user does have any links, returns the default list
     * @param {Boolean} whether the loadJSON got it's data
     * @param {Object} contains the users links record
     */
    // If data is empty, then throw an error.
    var loadLinksList = function (success, data) {
        var allLinks = getDirectory();
        createDirectory(allLinks, data); 
    };

    // First get user's link list, then populate directory with static directory data.
    var doInit = function(){
        sakai.api.Server.loadJSON(linksDataNode, loadLinksList); 
    };
    
    doInit();
    
};

sakai.api.Widgets.Container.registerForLoad("sakai.links");

