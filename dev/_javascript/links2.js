var sakai = sakai || {};

sakai.links = function(){
    
    // page elements
    var $suggestedSites = $(".suggested_sites"); 
    var $allSites = $(".all_sites"); 
    
    // templates
    var suggested_sites_template = "suggestedsites_links_template"; 
    var all_sites_template = "allsites_links_template"; 
    
    // data files and paths--same path as widget so always in sync
    var userLinks = "my_links"
    var linksDataNode = "/_user" + sakai.data.me.profile.path + "/private/" + userLinks;
    
   /**
     * write the users links to JCR
     * @param {object} the current state of the users list
    */
    var saveLinkList = function (updatedList) {
        sakai.api.Server.saveJSON(linksDataNode, updatedList);
    };
    
    var saveInit = function () {
        sakai.api.Server.loadJSON(linksDataNode, getChecked); 
    }    
    
    /**
     * Sets the titles for the checkboxes' tooltips, based on
     * whether its label's class specifies that it is checked
     * or unchecked.
     */
    var setTitles = function(){
        var all_labels = document.getElementsByTagName("label");
        for (k = 0; k < all_labels.length; k++) {
            if (all_labels[k].className != "LabelRequired") {
                all_labels[k].setAttribute("title", "Add to myLinks.");
            }
        }
        var selected_labels = document.getElementsByClassName("LabelSelected");
        for (i = 0; i < selected_labels.length; i++) {
                selected_labels[i].setAttribute("title", "Remove from myLinks.");
        }
    };
    
    /** 
     * Pre-sets appropriate label class, then as user checks or unchecks
     * boxes, will toggle the label class as needed. Calls setTitles() after
     * every change to ensure tooltips are correct.
     */
    var setStars = function(data){
        var listObj=data;
        // Pre-sets label class of all checked checkboxes besides required ones.
        $("input[type=checkbox][checked]").each( 
            function() {
               if($(this).next("label").attr("class")!="LabelRequired"){
                   $(this).next("label").addClass("LabelSelected");
                   setTitles();
               }
            } 
        );
        // Toggles label class of checkboxes as user checks or unchecks them.
        // Also toggles the state of selected attribute in the links list.
        $(".CheckBoxClass").change(function(data){                   
            if ($(this).is(":checked")) {              
                $(this).next("label").addClass("LabelSelected");
                var id = $(this).next("label").attr("id");
                for(i=0;i<listObj.links.length;i++){
                    if(id==listObj.links[i].id || id==listObj.links[i].id+"_label"){
                        listObj.links[i].selected=true;
                    }
                }
            }
            else {
                $(this).next("label").removeClass("LabelSelected");
                var id = $(this).next("label").attr("id");
                for(i=0;i<listObj.links.length;i++){
                    if(id==listObj.links[i].id || id==listObj.links[i].id+"_label"){
                        listObj.links[i].selected=false;                       
                    }
                }
            } 
            // Reset titles for proper tooltip.           
            setTitles();
            // Updating the widget.
            list = [];
            for(i=0;i<listObj.links.length;i++){
                if(listObj.links[i].selected){                
                    var toAdd = {
                        id :   listObj.links[i].id,
                        name : listObj.links[i].name,
                        url :  listObj.links[i].url
                    }
                    list.push(toAdd);
                }
            }
            listObj.widget=list;
            saveLinkList(listObj);
        })
    };
    
    /**
     * Prechecks any links that are currently present on the user's widget.
     * @param {Object} data contains the user's link record
     */
    var setChecked = function(data) {
        for(i=0; i<data.widget.length; i++){
            for(j=0; j<data.links.length; j++){
                if(data.widget[i].id==data.links[j].id){
                    data.links[j].selected=true;
                }
            }
        }
        return data;
    }
    
    var createLinkList = function (data, isUserList) {     
        $suggestedSites.html($.TemplateRenderer(suggested_sites_template, data));
        $allSites.html($.TemplateRenderer(all_sites_template, data));
        setTitles();
        setStars(data);
    };    
    
  

    /**
     * Retreives the current list of links for the current user
     * if the user does have any links, returns the default list
     * @param {Boolean} whether the loadJSON got it's data
     * @param {Object} contains the users links record
     */
    var loadLinksList = function (success, data) {
        // load the users link list from data
        createLinkList(setChecked(data), true);
    };

    var doInit = function(){
        sakai.api.Server.loadJSON(linksDataNode, loadLinksList);
    };
    
    doInit();
    
};

sakai.api.Widgets.Container.registerForLoad("sakai.links");

