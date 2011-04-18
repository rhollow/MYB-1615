
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
		var $designateTermYear = $("#designate_term_year", $sectionC);
		var $cohortStatus = $(".cohort_status", $sectionC);
		
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
			
		
		var buildSelectedOptionsArray = function($allItemsOption, $rootGroup) {

			var selectedOptionsOR = [];
						
			if ($allItemsOption != null && $allItemsOption.is(':checked')) {
				
				var allItemsOptionValue = $allItemsOption.val();
				if (allItemsOptionValue !== "") {
					selectedOptionsOR.push(allItemsOptionValue);
				}
				
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
		
		var buildCohortStatusObject = function() {
									
			var selectedValue = $("input[name=cohort_statuses]:checked", $cohortStatus).val();									
        	if (selectedValue === 'cohort_status_specified_students') {
        		var semester = $("#designate_term_semester", $cohortStatus).val();
        		var year = $("#designate_term_year", $cohortStatus).val();
        		var cohort = $("input[name=cohort_status_terms]:checked", $cohortStatus).val();
        		
        		var result = {
					AND: [semester, "designate_term_year_" + year, cohort]
				};
				
				return result;
        		
        	} else {
        		return null;
        	}        	
		};
		
		
		var buildUndergradsANDArray = function() {

			var undergradsAND = [];
			
			var selectedUndergradMajorsOR = buildSelectedOptionsArray($("#undergrad_majors_all"), $(".majors"));
			var selectedLevelsOR = buildSelectedOptionsArray($("#undergrad_level_all"), $(".levels"));
			var selectedAdmittedAs = $('input[name=undergrad_admitted_as]:checked').val(); // can be 'undefined'
			var selectedDeclared = $('input[name=undergrad_declared]:checked').val(); // can be 'undefined'


			undergradsAND = addToArrayAsOR(undergradsAND, selectedUndergradMajorsOR);								
			
			undergradsAND = addToArrayAsOR(undergradsAND, selectedLevelsOR);
							
			if (typeof(selectedAdmittedAs) !== 'undefined' && selectedAdmittedAs != "") {
				undergradsAND.push(selectedAdmittedAs);
			}
			
			if (typeof(selectedDeclared) !== 'undefined' && selectedDeclared != "") {
				undergradsAND.push(selectedDeclared);
			}
			
			return undergradsAND;	
		};
		
		var buildGradsANDArray = function() {

			var gradsAND = [];
			
			var selectedGradProgramsOR = buildSelectedOptionsArray($("#grad_programs_all"), $(".programs"));
			var selectedCertificates = buildSelectedOptionsArray(null, $(".certificates"));
			var selectedEmphases = buildSelectedOptionsArray(null, $(".emphases"));
			var selectedDegrees = $('input[name=grad_degrees]:checked').val(); // can be 'undefined'
			var selectedgsiGsr = buildSelectedOptionsArray(null, $(".gsiGsr"));
			
			
			gradsAND = addToArrayAsOR(gradsAND, selectedGradProgramsOR);

			gradsAND = addToArrayAsOR(gradsAND, selectedCertificates);

			gradsAND = addToArrayAsOR(gradsAND, selectedEmphases);

			if (typeof(selectedDegrees) !== 'undefined' && selectedDegrees != "") {
				gradsAND.push(selectedDegrees);
			}
			
			gradsAND = addToArrayAsOR(gradsAND, selectedgsiGsr);
						
			return gradsAND;	
		};
		
		/* Boolean stuff start */
		var canConvertORtoAND = function(obj) {
			if(!obj.hasOwnProperty("OR")) return false;
			return obj.OR.length === 1;
		}
		
		var canConvertANDtoOR = function(obj) {
			if(!obj.hasOwnProperty("AND")) return false;
			return obj.AND.length === 1;
		}
		
		var convertORtoAND = function(obj) {
			obj.AND = obj.OR;
			delete obj.OR;
		}
		
		var convertANDtoOR = function(obj) {
			obj.OR = obj.AND;
			delete obj.AND;
		}
		
		var joinTwoConditionsByAND = function(a, b) {
			
			if(canConvertORtoAND(a))convertORtoAND(a);
			if(canConvertORtoAND(b))convertORtoAND(b);
			
			if(a.hasOwnProperty("AND") && b.hasOwnProperty("AND")) {
				// simple array merge will do
				a.AND = a.AND.concat(b.AND);
				return a;
			}
			
			if(a.hasOwnProperty("AND") && b.hasOwnProperty("OR")) {
				// add b as array element to a.AND array
				a.AND.push(b);
				return a;
			}
			
			if(a.hasOwnProperty("OR") && b.hasOwnProperty("AND")) {
				// add a as array element to b.AND array
				b.AND.unshift(a);
				return b;
			}
			
			if(a.hasOwnProperty("OR") && b.hasOwnProperty("OR")) {
				// Need to wrap everything into a new object here				
				var result = {AND: [a, b]};
				return result;
			}
			
			return a; //default
		};
		
		var joinTwoConditionsByOR = function(a, b) {
			
			if(canConvertANDtoOR(a))convertANDtoOR(a);
			if(canConvertANDtoOR(b))convertANDtoOR(b);
			
			if(a.hasOwnProperty("OR") && b.hasOwnProperty("OR")) {
				// simple array merge will do
				a.OR = a.OR.concat(b.OR);
				return a;
			}
			
			if(a.hasOwnProperty("OR") && b.hasOwnProperty("AND")) {
				// add b as array element to a.OR array
				a.OR.push(b);
				return a;
			}
			
			if(a.hasOwnProperty("AND") && b.hasOwnProperty("OR")) {
				// add a as array element to b.OR array
				b.OR.unshift(a);
				return b;
			}
			
			if(a.hasOwnProperty("AND") && b.hasOwnProperty("AND")) {
				// Need to wrap everything into a new object here				
				var result = {OR: [a, b]};
				return result;
			}
			
			return a; //default
			
		};
		/* Boolean stuff end */
		
		var saveList = function() {
			
						
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
			
			//console.log("Include undergrads: " + boolIncludeUndergrads + " Include grads: " + boolIncludeGrads);
			
			// For undergrads
			var undergradsAND = [];			
			
			if (boolIncludeUndergrads) {
				undergradsAND = buildUndergradsANDArray();
			}
			
			
			// For grads
			var gradsAND = [];
			
			if (boolIncludeGrads) {
				gradsAND = buildGradsANDArray();
			}
			
			var result = {};
			
			if (boolIncludeUndergrads && boolIncludeGrads) {
				
								
				if (undergradsAND.length > 0 && gradsAND.length > 0) {
					
					result.OR = [];
										
					result.OR = addToArrayAsAND(result.OR, undergradsAND);										
					result.OR = addToArrayAsAND(result.OR, gradsAND);
										
				} else if(undergradsAND.length > 0) {
					
					result.OR = [$includeGrads.val()];
					result.OR = addToArrayAsAND(result.OR, undergradsAND);					
					//result.AND = undergradsAND;
						
				} else if(gradsAND.length > 0) {
					result.OR = [$includeUndergrads.val()];
					result.OR = addToArrayAsAND(result.OR, gradsAND);
					//result.AND = gradsAND;	
				}
				
				// Special case: if nothing besides two include graduates/undergraduates checkboxes is selected
				if (!result.hasOwnProperty("OR") && !result.hasOwnProperty("AND")) {					
					result.OR = [$includeUndergrads.val(), $includeGrads.val()];
				}
								
			} else if (boolIncludeUndergrads) {
				
				if(undergradsAND.length > 0) {
					result.AND = undergradsAND;	
				} else {
					// Special case: if nothing is selected
					result.OR = [$includeUndergrads.val()];
				}				
				
			} else if (boolIncludeGrads) {
				
				if(gradsAND.length > 0) {
					result.AND = gradsAND;	
				} else {
					// Special case: if nothing is selected
					result.OR = [$includeGrads.val()];
				}
								
			}
			
			
			// Section C
			var cohortStatus = buildCohortStatusObject();			
			if(cohortStatus != null) {
				result = joinTwoConditionsByAND(result, cohortStatus);
			} 
			
			
			$outputJson.val(formatJSON(result));
		}
		
		
		var disableCohortStatusForSpecifiedStudents = function() {
						
			$(".cohort_status_col_left .sub_group", $cohortStatus).addClass("disabled");
			$(".cohort_status_col_right", $cohortStatus).addClass("disabled");
			$(".cohort_status_col_left .sub_group select, .cohort_status_col_right input", $cohortStatus).attr("disabled", "disabled");						
		};
		
		var enableCohortStatusForSpecifiedStudents = function() {
						
			$(".cohort_status_col_left .sub_group", $cohortStatus).removeClass("disabled");
			$(".cohort_status_col_right", $cohortStatus).removeClass("disabled");
			$(".cohort_status_col_left .sub_group select, .cohort_status_col_right input", $cohortStatus).removeAttr("disabled");			
			
		};
		
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
								
				$("input", $gradsGroup).attr("disabled", "disabled");
				$("input", $undergradsGroup).attr("disabled", "disabled");
					
						
				$includeGrads.click(function(){
						if($includeGrads.is(':checked')){
							$("input", $gradsGroup).removeAttr("disabled");						
							$gradsGroup.removeClass("disabled");
						} else {
							$("input", $gradsGroup).attr("disabled", "disabled");
							$gradsGroup.addClass("disabled");
						}
											
				});
								
				$includeUndergrads.click(function(){
						if($includeUndergrads.is(':checked')){
							$("input", $undergradsGroup ).removeAttr("disabled");							
							$undergradsGroup.removeClass("disabled");						
						} else {
							$("input", $undergradsGroup ).attr("disabled", "disabled");							
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
			
			// section C : cohort status
			disableCohortStatusForSpecifiedStudents();
			
			$("input[name=cohort_statuses]", $cohortStatus).change(function(){
   				var selectedValue = $("input[name=cohort_statuses]:checked", $cohortStatus).val();
   				if (selectedValue === 'cohort_status_all_students') {
        			disableCohortStatusForSpecifiedStudents();
        		} else if (selectedValue === 'cohort_status_specified_students') {
        			enableCohortStatusForSpecifiedStudents();
        		}
        		
			});
			
			$saveList.click(saveList);
			
						
		};

        // run the initialization function when the widget object loads
        doInit();
	
    };

	sakai.api.Widgets.Container.registerForLoad("template_test");	
});

