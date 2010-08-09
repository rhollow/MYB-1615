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
    var directoryLinksLocation = "/var/defaults/mylinks/links-directory.json";
 
    var directory = {};

    /**
     * Write the users links to JCR.
     * @param {object} updatedList The current state of the user's list.
     */
    var saveLinkList = function(updatedList){
        sakai.api.Server.saveJSON(linksDataNode, updatedList);
    };

    /**
     * Update the list according to whether a link was added or
     * removed by either adding or removing from the user's link list.
     * array (based on the operation it is passed).
     * @param {Object} operation What operation to perform; either "add" or "remove".
     * @param {Object} id The id of the link to perform the operation on.
     * @param {Object} data User's current data.
     */
    var updateLinkList = function(operation, toOperateOn, data){		
        var listObj = data;
        var toAdd;
        // User added a link to their data list.
        if (operation === "add") {
			toAdd = toOperateOn;
            listObj.links.push(toAdd);               
        }
        // User removed a link from their data list.
        if (operation === "remove") {
			var id = toOperateOn.id;
            var newList = [];
            for (var j = 0; j < listObj.links.length; j++) {
                if (id === listObj.links[j].id) {
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
     * Function for when user checks a link in the featured section.
     * @param {Object} toCheck Checkbox element that was checked.
     * @param {Object} data User's current data.
     */
    var checkFeatured = function(toCheck, data){
		var listObj = data;
		var toOperateOn;
        var id = toCheck.next("label").attr("id");
        if (toCheck.is(":checked")) {
            toCheck.next("label").addClass("LabelSelected");
			toCheck.next("label").attr("title", "Remove from myLinks.");
            $('#' + id + '_box').attr("checked", true);
            $('#' + id + '_label').addClass("LabelSelected");
			$('#' + id + '_label').attr("title", "Remove from myLinks.");
			toOperateOn = {
				id: id,
				name: $("#urlfor_"+id).attr("innerHTML"),
				url: $("#urlfor_"+id).attr("href")
			}
            updateLinkList("add", toOperateOn, listObj);
        }
        else {
            toCheck.next("label").removeClass("LabelSelected");
			toCheck.next("label").attr("title", "Add to myLinks.");
            $('#' + id + '_box').attr("checked", false);
            $('#' + id + '_label').removeClass("LabelSelected");
			$('#' + id + '_label').attr("title", "Add to myLinks.");
			toOperateOn = {
                id: id,
                name: $("#urlfor_"+id).attr("innerHTML"),
                url: $("#urlfor_"+id).attr("href")
            }
            updateLinkList("remove", toOperateOn, listObj);
        }
    };
	
	/**
	 * Function for when user checks a link in the general (all) section.
	 * @param {Object} toCheck Checkbox element that was checked.
	 * @param {Object} data User's current data.
	 */
    var checkGeneral = function(toCheck, data){
		var listObj = data;
        var id = toCheck.next("label").attr("id");
        var pos = id.indexOf("_label");
        var newID = id.slice(0, pos);
        var isFeatured = false;
		var toOperateOn;
        for (var k = 0; k < directory.featured.length; k++) {
            if (directory.featured[k] === newID) {
                isFeatured = true;
            }
        }
        if (toCheck.is(":checked")) {
            toCheck.next("label").addClass("LabelSelected");
			toCheck.next("label").attr("title", "Remove from myLinks.");
            if (isFeatured) {
                $('#' + newID + '_checkbox').attr("checked", true);
                $('#' + newID).addClass("LabelSelected");
				$('#' + newID).attr("title", "Remove from myLinks.");
            }
			toOperateOn = {
                id: id,
                name: $("#urlfor_"+newID).attr("innerHTML"),
                url: $("#urlfor_"+newID).attr("href")
            }
            updateLinkList("add", toOperateOn, listObj);
        }
        else {
            toCheck.next("label").removeClass("LabelSelected");
			toCheck.next("label").attr("title", "Add to myLinks.");
            if (isFeatured) {
                $('#' + newID + '_checkbox').attr("checked", false);
                $('#' + newID).removeClass("LabelSelected");
				$('#' + newID).attr("title", "Remove from myLinks.");
            }
			toOperateOn = {
                id: id,
                name: $("#urlfor_"+newID).attr("innerHTML"),
                url: $("#urlfor_"+newID).attr("href")
            }
            updateLinkList("remove", toOperateOn, listObj);
        }
    };
	
	/**
	 * Will react when the user causes a change to any of the checkboxes
	 * on the page. It then determines if that checkbox is featured or
	 * one of the general (all) links, and calls the appropriate function.
	 * @param {Object} data User's current data.
	 */
	var checkStars = function(data){
		var listObj = data;
		$(":checkbox").change(function(data){
		  if($(this).attr("class")=="FeaturedCheckBoxClass"){
		      checkFeatured($(this), listObj);
		  }
		  if($(this).attr("class")=="CheckBoxClass"){
		      checkGeneral($(this), listObj);
		  }	
		});		        
    };
	
	/**
	 * Pre-checking the stars of the links the user has saved in their data.
	 * @param {Object} data User's data and list of links.
	 */
	var preCheckStars = function(data){
		var listObj = data;
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
	}

    /**
     * Runs through the trimpath functions to populate the page with
     * the proper data.
     * @param {Object} directory All links.
     * @param {Object} userLinks User's current data.
     */
    var createDirectory = function(directory, userLinks){
        $suggestedSites.html($.TemplateRenderer(suggested_sites_template, directory));
        $allSites.html($.TemplateRenderer(all_sites_template, directory));
		preCheckStars(userLinks);
		setTitles();
        checkStars(userLinks);
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
     * Retrieves the current list of links for the current user
     * if the user does have any links, returns the default list
     * @param {Boolean} success Whether the loadJSON got its data.
     * @param {Object} data Contains the user's links record.
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
