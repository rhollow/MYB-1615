
// load the master sakai object to access all Sakai OAE API methods
require(["jquery", "sakai/sakai.api.core"], function($, sakai) {

    
    sakai_global.template_test = function (tuid, showSettings) {

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////

        // trimpath Templates
        var $template = $("#template");
		
		var $view = $("#view");
		
		
		var $sectionC = $("#section_c");
		var $designateTermYear = $("#designate_term_year");
		
		var $undergradsGroup;
		var $gradsGroup;
			
		var $saveList = $("#save_list");
		
		var $outputJson = $("#output_json");
		
		
		var $includeUndergrads;
		var $includeGrads;

		var boolTemplateHasUndergradsData = false;
		var boolTemplateHasGradsData = false;

        
		
		var populateDesignateTermYear = function() {
			
			var d = new Date();
			var curr_year = d.getFullYear();
			var yearsArr = [];
			for(var i = curr_year-4;i <= curr_year; i++) {
				if (i != curr_year) {
					yearsArr.push("<option value='" + i + "'>" + i + "</option>");
				}
				else {
					yearsArr.push("<option value='" + i + "' selected='selected'>" + i + "</option>");
				}				
			}
			$designateTermYear.append(yearsArr.join(''));
		}
		
		/* Debug */
		var RealTypeOf = function(v) {
		  if (typeof(v) == "object") {
		    if (v === null) return "null";
		    if (v.constructor == (new Array).constructor) return "array";
		    if (v.constructor == (new Date).constructor) return "date";
		    if (v.constructor == (new RegExp).constructor) return "regex";
		    return "object";
		  }
		  return typeof(v);
		};
		
		var formatJSON = function(oData, sIndent) {
		    if (arguments.length < 2) {
		        var sIndent = "";
		    }
		    var sIndentStyle = "    ";
		    var sDataType = RealTypeOf(oData);
		
		    // open object
		    if (sDataType == "array") {
		        if (oData.length == 0) {
		            return "[]";
		        }
		        var sHTML = "[";
		    } else {
		        var iCount = 0;
		        $.each(oData, function() {
		            iCount++;
		            return;
		        });
		        if (iCount == 0) { // object is empty
		            return "{}";
		        }
		        var sHTML = "{";
		    }
		
		    // loop through items
		    var iCount = 0;
		    $.each(oData, function(sKey, vValue) {
		        if (iCount > 0) {
		            sHTML += ",";
		        }
		        if (sDataType == "array") {
		            sHTML += ("\n" + sIndent + sIndentStyle);
		        } else {
		            sHTML += ("\n" + sIndent + sIndentStyle + "\"" + sKey + "\"" + ": ");
		        }
		
		        // display relevant data type
		        switch (RealTypeOf(vValue)) {
		            case "array":
		            case "object":
		                sHTML += formatJSON(vValue, (sIndent + sIndentStyle));
		                break;
		            case "boolean":
		            case "number":
		                sHTML += vValue.toString();
		                break;
		            case "null":
		                sHTML += "null";
		                break;
		            case "string":
		                sHTML += ("\"" + vValue + "\"");
		                break;
		            default:
		                sHTML += ("TYPEOF: " + typeof(vValue));
		        }
		
		        // loop
		        iCount++;
		    });
		
		    // close object
		    if (sDataType == "array") {
		        sHTML += ("\n" + sIndent + "]");
		    } else {
		        sHTML += ("\n" + sIndent + "}");
		    }
		
		    // return
		    return sHTML;
		};
		/* Debug */
		
		
		
		
		
		var buildMajorsOrProgramsArray = function($allItemsOption, $rootGroup) {

			var selectedOptionsOR = [];
						
			if ($allItemsOption.is(':checked')) {
				
				selectedOptionsOR.push($allUndergradMajorsOption.val());
				
			} else {
			
				var $selectedOptions = $("input:checkbox:checked", $rootGroup);

				$selectedOptions.each(function(i, curOption) {
						selectedOptionsOR.push(curOption.value);
				});
			}
			
			return selectedOptionsOR;
		};
		
		var saveList = function() {
			
			
			//var jsonRequest = {};
			
			//TODO: check the case when checkboxes are not rendered
			
			var boolIncludeUndergrads = $includeUndergrads.length > 0 && $includeUndergrads.is(":checked");
			var boolIncludeGrads = $includeGrads.length > 0 && $includeGrads.is(":checked");  
			
			console.log("Include undergrads: " + boolIncludeUndergrads + " Include grads: " + boolIncludeGrads);
						
			var selectedUndergradMajorsOR = buildMajorsOrProgramsArray($("#undergrad_majors_all"), $undergradsGroup);
			var selectedGradProgramsOR = buildMajorsOrProgramsArray($("#grad_programs_all"), $gradsGroup);
			
			/*
			 if (!jsonRequest.hasOwnProperty("OR")) {
						jsonRequest.OR = [];
					}
			 */
			
			//var selectedGradPrograms = $("input:checkbox:checked", $gradsGroup);
			
			//console.log($selectedUndergradMajors);
			//console.log(selectedGradPrograms);
			//console.log(jsonRequest);
			//console.log(jsonRequest.OR.length);
			//$outputJson.val($.toJSON(jsonRequest));
			$outputJson.val(formatJSON({
				OR: [  {OR: selectedUndergradMajorsOR}, 
						{OR: selectedGradProgramsOR}
					]
			}));
		}
		
		
        /////////////////////////////
        // Initialization function //
        /////////////////////////////

        /**
         * Initialization function that is run when the widget is loaded. Determines
         * which mode the widget is in (settings or main), loads the necessary data
         * and shows the correct view.
         */
        var doInit = function () {
            
			// Loading template
            var template;
			$.ajax({
                    url: "nauth_ced.json", //sakai.config.URL.POOLED_CONTENT_ACTIVITY_FEED + "?p=" + content_path  + "&items=1000",
                    type: "GET",
                    "async":false,
                    "cache":false,
                    "dataType":"json",
                    success: function(data){
                        if (data) {
                           template = data;
                        }
                    },
					 error: function(xhr, textStatus, thrownError) {
                        sakai.api.Util.notification.show(errorText,"",sakai.api.Util.notification.type.ERROR);
                    }
            });
			
			boolTemplateHasUndergradsData = typeof(template.undergraduates) !== 'undefined' && template.undergraduates != null;
			boolTemplateHasGradsData = typeof(template.graduates) !== 'undefined' && template.graduates != null; 
			
			$view.html(sakai.api.Util.TemplateRenderer($template, template));
			
			$includeUndergrads = $("#include_undergrads");
			$includeGrads = $("#include_grads");
									
						
			// Define undergrad and grad groups AFTER template has been rendered
			$undergradsGroup = $(".undergrads_group");
			$gradsGroup = $(".grads_group");
			
			//Disabling all graduate and undergraduate controls
			if (boolUndergradAndGradCheckboxesVisible) {
				
				$undergradsGroup.addClass("disabled");
				$gradsGroup.addClass("disabled");
								
				$(".grads_group input", $gradsGroup).each(function(i, val){
					$(val).attr("disabled", "disabled");
				});
				$(".undergrads_group input", $undergradsGroup).each(function(i, val){
					$(val).attr("disabled", "disabled");
				});
					
						
				$includeGrads.click(function(){
						if($includeGrads.is(':checked')){
							$(".grads_group input", $gradsGroup).each(function(i, val) {							
								if(val.id !== "include_grads")$(val).removeAttr("disabled");
							});	
							$gradsGroup.removeClass("disabled");
						} else {
							$(".grads_group input", $gradsGroup).each(function(i, val) {							
								if(val.id !== "include_grads")$(val).attr("disabled", "disabled");
							});						
							$gradsGroup.addClass("disabled");
						}
											
				});
								
				$includeUndergrads.click(function(){
						if($includeUndergrads.is(':checked')){
							$(".undergrads_group input", $undergradsGroup ).each(function(i, val) {							
								$(val).removeAttr("disabled");
							});	
							$undergradsGroup.removeClass("disabled");						
						} else {
							$(".undergrads_group input", $undergradsGroup ).each(function(i, val) {							
								$(val).attr("disabled", "disabled");
							});
							$undergradsGroup.addClass("disabled");						
						}
											
				}); 
			}
			
			// Click handlers
			$('input[id^="undergrad_major_"]').click(function(){
					$("#undergrad_majors_selected_majors").click();
				});
				
			$('input[id^="grad_program_"]').click(function(){
					$("#grad_programs_selected_programs").click();
				});
            
			$('input[id^="reg_status_withdrawn_"]').click(function(){
					$("#reg_status_withdrawn").click();
				});
			$('input[id^="currency_status_current_"]').click(function(){
					$("#currency_status_current").click();
				});
			$('input[id^="special_program_one_or_more_"]').click(function(){
					$("#special_program_one_or_more").click();
				});
			$('input[id^="student_status_one_or_more_"]').click(function(){
					$("#student_status_one_or_more").click();
				});
			$('input[id^="undergrad_level_selected_levels_"]').click(function(){
					$("#undergrad_level_selected_levels").click();
				});
			
			populateDesignateTermYear();	
			
			
			// section C toggle button
			var $showMoreOrLess = $("#show_more_or_less");
			$showMoreOrLess.click(function () {
				
				if($sectionC.is(":visible")) {
					$showMoreOrLess.text("Show More");					
				} else {
					$showMoreOrLess.text("Show Less");					
				}
				$sectionC.toggle();				
			});
			
			$saveList.click(saveList);
			
						
		};

        // run the initialization function when the widget object loads
        doInit();
	
    };

	sakai.api.Widgets.Container.registerForLoad("template_test");	
});

