/**
 * 
 */

sakai.upgradeDynamicLists = function() {
	var LIST_URL = "/private/dynamic_lists.infinity.json";
    var dryRun = true;
    
	var getAdvisors = function() {
		
	}
	
	var loadLists = function() {
		
	}
	
    var doInit = function() {
        var querystring = new Querystring();
        if ( querystring.contains("dryRun") && querystring.get("dryRun") === "false") {
            dryRun = false;
        }
        $("#upgrade_dynamiclists_button").live("click", function() {
            console.log("Running upgrade; dryRun=" + dryRun);
            loadLists();
        });
    };

    doInit();
};
sakai.api.Widgets.Container.registerForLoad("sakai.upgradeDynamicLists");