
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
						
			if ($allItemsOption != null && $allItemsOption.is(':checked')) {
				
				selectedOptionsOR.push($allItemsOption.val());
				
			} else {
			
				var $selectedOptions = $("input:checkbox:checked", $rootGroup);

				$selectedOptions.each(function(i, curOption) {
						selectedOptionsOR.push(curOption.value);
				});
			}
			
			return selectedOptionsOR;
		};
		
		
		var addToArrayAsOR = function(targetArray, arrayToAdd) {			
			var result = targetArray;
			if (arrayToAdd.length == 1) {					
				result = targetArray.concat(arrayToAdd); // no need for wrapping object in this case				
			} else if (arrayToAdd.length > 0) {
				targetArray.push({
					OR: arrayToAdd
				});
				
			}
			return result;
		};
		
		var addToArrayAsAND = function(targetArray, arrayToAdd) {			
			var result = targetArray;
			if (arrayToAdd.length == 1) {					
				result = targetArray.concat(arrayToAdd); // no need for wrapping object in this case				
			} else if (arrayToAdd.length > 0) {
				targetArray.push({
					AND: arrayToAdd
				});				
			}			
			return result;
		};
		
		var saveList = function() {
			
			
			//var jsonRequest = {};
			
						
			var boolIncludeUndergrads = false;
			var boolIncludeGrads = false;  
			
			if(boolTemplateHasUndergradsData && boolTemplateHasGradsData) {
				// Both checkboxes are available, need to check the statuses of the both checkboxes
				boolIncludeUndergrads = $includeUndergrads.length > 0 && $includeUndergrads.is(":checked");
				boolIncludeGrads = $includeGrads.length > 0 && $includeGrads.is(":checked");
								
			} else {
				if(boolTemplateHasUndergradsData) {
					// Only undergrads are available, no checkbox
					boolIncludeUndergrads = true;
				} else if(boolTemplateHasGradsData) {
					// Only grads are available, no checkbox
					boolIncludeGrads = true;
				}
			}
			
			console.log("Include undergrads: " + boolIncludeUndergrads + " Include grads: " + boolIncludeGrads);
			
			// For undergrads
			var undergradsAND = [];			
			
			if (boolIncludeUndergrads) {
				var selectedUndergradMajorsOR = buildMajorsOrProgramsArray($("#undergrad_majors_all"), $(".majors"));
				var selectedLevelsOR = buildMajorsOrProgramsArray($("#undergrad_level_all"), $(".levels"));
				var selectedAdmittedAs = $('input[name=undergrad_admitted_as]:checked').val(); // can be 'undefined'
				var selectedDeclared = $('input[name=undergrad_declared]:checked').val(); // can be 'undefined'


				undergradsAND = addToArrayAsOR(undergradsAND, selectedUndergradMajorsOR);
				
				/*if (selectedUndergradMajorsOR.length == 1) {					
					undergradsAND = undergradsAND.concat(selectedUndergradMajorsOR); // no need for wrapping object in this case
					
				} else if (selectedUndergradMajorsOR.length > 0) {
					undergradsAND.push({
						OR: selectedUndergradMajorsOR
					});
				}*/
				
				undergradsAND = addToArrayAsOR(undergradsAND, selectedLevelsOR);
				/*if (selectedLevelsOR.length == 1) {
					undergradsAND = undergradsAND.concat(selectedLevelsOR); // no need for wrapping object in this case
				} else if (selectedLevelsOR.length > 0) {
					undergradsAND.push({
						OR: selectedLevelsOR
					});
				}*/
				
				if (typeof(selectedAdmittedAs) !== 'undefined' && selectedAdmittedAs != "") {
					undergradsAND.push(selectedAdmittedAs);
				}
				if (typeof(selectedDeclared) !== 'undefined' && selectedDeclared != "") {
					undergradsAND.push(selectedDeclared);
				}
			}
			
			
			// For grads
			var gradsAND = [];
			
			if (boolIncludeGrads) {
				var selectedGradProgramsOR = buildMajorsOrProgramsArray($("#grad_programs_all"), $(".programs"));
				var selectedCertificates = buildMajorsOrProgramsArray(null, $(".certificates"));
				var selectedEmphases = buildMajorsOrProgramsArray(null, $(".emphases"));
				var selectedDegrees = $('input[name=grad_degrees]:checked').val(); // can be 'undefined'
				var selectedgsiGsr = buildMajorsOrProgramsArray(null, $(".gsiGsr"));
				
				
				gradsAND = addToArrayAsOR(gradsAND, selectedGradProgramsOR);
				/*if (selectedGradProgramsOR.length > 0) {
					gradsAND.push({
						OR: selectedGradProgramsOR
					});
				}*/
				gradsAND = addToArrayAsOR(gradsAND, selectedCertificates);
				/*if (selectedCertificates.length > 0) {
					gradsAND.push({
						OR: selectedCertificates
					});
				}*/
				gradsAND = addToArrayAsOR(gradsAND, selectedEmphases);
				/*if (selectedEmphases.length > 0) {
					gradsAND.push({
						OR: selectedEmphases
					});
				}*/
				if (typeof(selectedDegrees) !== 'undefined' && selectedDegrees != "") {
					gradsAND.push(selectedDegrees);
				}
				
				gradsAND = addToArrayAsOR(gradsAND, selectedgsiGsr);
				/*if (selectedgsiGsr.length > 0) {
					gradsAND.push({
						OR: selectedgsiGsr
					});
				}*/
			}
			
			var result = {};
			
			if (boolIncludeUndergrads && boolIncludeGrads) {
				
								
				if (undergradsAND.length > 0 && gradsAND.length > 0) {
					
					result.OR = [];
										
					result.OR = addToArrayAsAND(result.OR, undergradsAND);										
					result.OR = addToArrayAsAND(result.OR, gradsAND);
										
				} else if(undergradsAND.length > 0) {
					result.AND = undergradsAND;	
				} else if(gradsAND.length > 0) {
					result.AND = gradsAND;	
				}
				
								
			} else if (boolIncludeUndergrads) {
				
				if(undergradsAND.length > 0) {
					result.AND = undergradsAND;	
				}				
				
			} else if (boolIncludeGrads) {
				
				if(gradsAND.length > 0) {
					result.AND = gradsAND;	
				}
								
			}
			
			
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
			$outputJson.val(formatJSON(result));
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
			if (boolTemplateHasUndergradsData && boolTemplateHasGradsData) {
				
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

