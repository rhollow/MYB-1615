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
    var linksDataNode = "/~" + sakai.data.me.user.userid + "/private/" + userLinks;


    // Still working out some problems with the loading of default data from /var
    //var directoryLinksLocation = "/var/myberkeley/links-directory.json";
    var directoryLinksLocation = "/devwidgets/mylinks/default/links-directory.json";

    var directory = {};

    /**
     * Write the users links to JCR.
     * @param {object} the current state of the users list
     */
    var saveLinkList = function(updatedList){
        sakai.api.Server.saveJSON(linksDataNode, updatedList);
    };

    /**
     * Update the list according to whether a link was added or
     * removed by either adding or removing from the user's link list
     * array (based on the operation it is passed).
     * @param {Object} what operation to perform; either "add" or "remove"
     * @param {Object} the id of the link to perform the operation on
     * @param {Object} users data and widget link list
     */
    var updateLinkList = function(operation, id, data){
        var listObj = data;
        var toAdd;
        // User added a link to their data list.
        if (operation === "add") {
            for (var i = 0; i < directory.links.length; i++) {
                if (id === directory.links[i].id || id === directory.links[i].id + "_label") {
                    toAdd = {
                        id: directory.links[i].id,
                        name: directory.links[i].name,
                        url: directory.links[i].url
                    };
                    listObj.links.push(toAdd);
                }
            }
        }
        // User removed a link from their data list.
        if (operation === "remove") {
            var newList = [];
            for (var j = 0; j < listObj.links.length; j++) {
                if (id === listObj.links[j].id || id === listObj.links[j].id + "_label") {
                    // do nothing...
                }
                else {
                    toAdd = {
                        id: listObj.links[j].id,
                        name: listObj.links[j].name,
                        url: listObj.links[j].url
                    };
                    newList.push(toAdd);
                }
            }
            listObj.links = newList;
        }
        else {
            // throw an error...
        }
        saveLinkList(listObj);
    };

    /**
     * Sets the titles for the checkboxes' tooltips, based on
     * whether its label's class specifies that it is checked
     * or unchecked.
     */
    var setTitles = function(){
        var all_labels = document.getElementsByTagName("label");
        for (var k = 0; k < all_labels.length; k++) {
            if (all_labels[k].className !== "LabelRequired") {
                all_labels[k].setAttribute("title", "Add to myLinks.");
            }
        }
        var selected_labels = document.getElementsByClassName("LabelSelected");
        for (var i = 0; i < selected_labels.length; i++) {
            selected_labels[i].setAttribute("title", "Remove from myLinks.");
        }
    };

    /**
     * As user checks or unchecks stars, will toggle the label class as needed.
     * Calls setTitles() after every change to ensure tooltips are always correct.
     * Also makes a call to update the user's data for the widget link list.
     * Keeps featured and all link stars in sync.
     * @param {Object} the current state of the users list
     */
    var checkStars = function(data){
        var listObj = data;
        // Prechecking the stars which the user has saved to their data.
        $("input[type=checkbox]").each(function(){
            var id = $(this).next("label").attr("id");
            for (var i = 0; i < listObj.links.length; i++) {
                if (id === listObj.links[i].id || id === listObj.links[i].id + "_label") {
                    $(this).attr("checked", true);
                    if ($(this).next("label").attr("class") !== "LabelRequired") {
                        $(this).next("label").addClass("LabelSelected");
                    }
                }
            }
        });
        // For links in the featured section (id)...
        $(".FeaturedCheckBoxClass").change(function(data){
            var id = $(this).next("label").attr("id");
            if ($(this).is(":checked")) {
                $(this).next("label").addClass("LabelSelected");
                $('#' + id + '_box').attr("checked", true);
                $('#' + id + '_label').addClass("LabelSelected");
                updateLinkList("add", id, listObj);
            }
            else {
                $(this).next("label").removeClass("LabelSelected");
                $('#' + id + '_box').attr("checked", false);
                $('#' + id + '_label').removeClass("LabelSelected");
                updateLinkList("remove", id, listObj);
            }
            setTitles();
        });
        // For links in the all section (id_label)...
        $(".CheckBoxClass").change(function(data){
            var id = $(this).next("label").attr("id");
            var pos = id.indexOf("_label");
            var newID = id.slice(0, pos);
            var isFeatured = false;
            for (var k = 0; k < directory.featured.length; k++) {
                if (directory.featured[k] === newID) {
                    isFeatured = true;
                }
            }
            if ($(this).is(":checked")) {
                $(this).next("label").addClass("LabelSelected");
                if (isFeatured) {
                    $('#' + newID + '_checkbox').attr("checked", true);
                    $('#' + newID).addClass("LabelSelected");
                }
                updateLinkList("add", newID, listObj);
            }
            else {
                $(this).next("label").removeClass("LabelSelected");
                if (isFeatured) {
                    $('#' + newID + '_checkbox').attr("checked", false);
                    $('#' + newID).removeClass("LabelSelected");
                }
                updateLinkList("remove", newID, listObj);
            }
            setTitles();
        });
    };

    /**
     * Runs through the trimpath functions to populate the page with
     * the proper data.
     * @param {Object} all links
     * @param {Object} users data for the widget
     */
    var createDirectory = function(directory, userLinks){
        $suggestedSites.html($.TemplateRenderer(suggested_sites_template, directory));
        $allSites.html($.TemplateRenderer(all_sites_template, directory));
        checkStars(userLinks, directory);
        setTitles();
    };

    /**
     * A getter function that simply returns the directory of links.
     */
    var getAndCreateDirectory = function(userLinks){
        $.ajax({
            url: directoryLinksLocation,
            cache: false,
            success: function(data){
                directory = data;
                createDirectory(directory,userLinks);
            },
            error: function(xhr, textStatus, thrownError) {
                 //alert("An error has occured");
                return null;
            }
        });

    };

    /**
     * Retreives the current list of links for the current user
     * if the user does have any links, returns the default list
     * @param {Boolean} whether the loadJSON got it's data
     * @param {Object} contains the users links record
     */
    // If data is empty, then throw an error.
    var loadLinksList = function(success, data){
        getAndCreateDirectory(data);
    };

    // First get user's link list, then populate directory with static directory data.
    var doInit = function(){
        sakai.api.Server.loadJSON(linksDataNode, loadLinksList);
    };

    doInit();

};

sakai.api.Widgets.Container.registerForLoad("sakai.links");
