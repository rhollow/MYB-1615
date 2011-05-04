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

/* global $, Config, opensocial */

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core",
    "/dev/lib/myb/jquery/jquery-ui-tabs-min.js", "/dev/javascript/myb/myb.securepage.js"], function($, sakai, myb) {
	sakai_global.listpage = function(){
	    
	    
	    /*-------------------------------- Roman ------------------------------------------*/
	    /////////////////////////////
        // Configuration variables //
        /////////////////////////////
        
        /**
         * Trimpath template
         */
        var $template = $("#template");
		
		/**
		 * View to render the template
		 */
		var $view = $("#view");
		
		/**
		 * Section C wrapper DIV
		 */		
		var $sectionC = $("#section_c");
		
		/**
		 * Section C cohort staus wrapper DIV
		 */
		var $cohortStatus = $(".cohort_status", $sectionC);
		
		/**
		 * Designate term year from section C cohort status
		 */
		var $designateTermYear = $("#designate_term_year", $cohortStatus);
		
		/**
		 * Undergraduate students wrapper DIV (template must be loaded before using this variable)
		 */
		var $undergradsGroup;
		
		/**
		 * Graduate students wrapper DIV (template must be loaded before using this variable)
		 */
		var $gradsGroup;
		
		/**
		 * Save dynamic list button
		 */	
		var $saveList = $("#save_list");
		
		/**
		 * Text area for JSON output (debug only)
		 */
		var $outputJson = $("#output_json");
		
		/**
		 * 'Include undergraduate students' checkbox (or hidden input when checkbox is not displayed)
		 */
		var $includeUndergradsCheckbox;

		/**
		 * 'Include graduate students' checkbox (or hidden input when checkbox is not displayed)
		 */
		var $includeGradsCheckbox;

		/**
		 * Whether the loaded trimpath template includes undergraduate students data
		 */
		var boolTemplateHasUndergradsData = false;
		
		/**
		 * Whether the loaded trimpath template includes graduate students data
		 */
		var boolTemplateHasGradsData = false;
	    
	    //////////////////////////////////////////////////////////
        // Functions for manipulating boolean condition objects //
        //////////////////////////////////////////////////////////		
		
		/**
		 * Checks if the condition object can be converted from OR form to AND form.
		 * 
		 * @return {boolean}true if the condition object can be safely converted from OR form to AND form; otherwise returns {boolean}false. 
		 */
		var canConvertORtoAND = function(obj) {
			if(!obj.hasOwnProperty("OR")) return false;
			var len = obj.OR.length;
			return len === 1 || len === 0;
		}
		
		/**
		 * Checks if the condition object can be converted from AND form to OR form.
		 * 
		 * @return {boolean}true if the condition object can be safely converted from AND form to OR form; otherwise returns {boolean}false. 
		 */
		var canConvertANDtoOR = function(obj) {
			if(!obj.hasOwnProperty("AND")) return false;
			var len = obj.AND.length
			return len === 1 || len === 0;
		}
		
		/**
		 * Converts the condition object from OR form to AND form.
		 * The function doesn't do any checks before conversion.
		 * 
		 * @param {Object}	obj	object to convert
		 */
		var convertORtoAND = function(obj) {
			obj.AND = obj.OR;
			delete obj.OR;
		}
		
		/**
		 * Converts the condition object from AND form to OR form.
		 * The function doesn't do any checks before conversion.
		 * 
		 * @param {Object}	obj	object to convert
		 */
		var convertANDtoOR = function(obj) {
			obj.OR = obj.AND;
			delete obj.AND;
		}
		
		/**
		 * Checks if the condition object is empty, i.e. doesn't have AND or OR properties, or one of these properties contains an empty array
		 * 
		 * @return {boolean}true if the condition object is empty; otherwise returns {boolean}false. 
		 */
		var isConditionObjectEmpty = function(obj) {
			var objHasOwnPropertyAND = obj.hasOwnProperty("AND");
			var objHasOwnPropertyOR = obj.hasOwnProperty("OR");
			return (objHasOwnPropertyAND && obj.AND.length === 0) || (objHasOwnPropertyOR && obj.OR.length === 0) || (!objHasOwnPropertyAND && !objHasOwnPropertyOR);
		};
		
		/**
		 * Joins two condition objects by AND condition.
		 * This function tries to optimeze the output object to avoid excessive object wrapping.
		 * To avoid object clonning the function operates on its arguments, there is no guarantee that the arguments will remain unchanged.
		 * 
		 * @param {Object}	a	first condition object to join (must contain either AND or OR field)
		 * @param {Object}	b	second condition object to join (must contain either AND or OR field)
		 * 
		 * @return {Object} an object containing the first condition object joined by AND with the second condition object. 
		 */
		var joinTwoConditionsByAND = function(a, b) {
			
			var isAEmpty = isConditionObjectEmpty(a);
			var isBEmpty = isConditionObjectEmpty(b); 									
			
			if(isAEmpty && isBEmpty) {
				var emptyObj = {};
				return emptyObj;
			}
			
			if(isAEmpty) {
				// trying to join empty object 'a' with 'b', just return 'b' in this case
				return b;
			}
			
			if(isBEmpty) {
				// trying to join empty object 'b' with 'a', just return 'a' in this case
				return a;
			}
						
			
			if(canConvertORtoAND(a)) {
				convertORtoAND(a);
			} 
			
			if(canConvertORtoAND(b)){
				convertORtoAND(b);
			} 
			
			var aHasOwnPropertyAND = a.hasOwnProperty("AND");
			var aHasOwnPropertyOR = a.hasOwnProperty("OR");
			
			var bHasOwnPropertyAND = b.hasOwnProperty("AND");
			var bHasOwnPropertyOR = b.hasOwnProperty("OR");
			
			if(aHasOwnPropertyAND && bHasOwnPropertyAND) {
				// simple array merge will do
				a.AND = a.AND.concat(b.AND);
				return a;
			}
			
			if(aHasOwnPropertyAND && bHasOwnPropertyOR) {
				// add b as array element to a.AND array
				a.AND.push(b);
				return a;
			}
			
			if(aHasOwnPropertyOR && bHasOwnPropertyAND) {
				// add a as array element to b.AND array
				b.AND.unshift(a);
				return b;
			}
			
			if(aHasOwnPropertyOR && bHasOwnPropertyOR) {
				// Need to wrap everything into a new object here				
				var result = {AND: [a, b]};
				return result;
			}
			
			return a; //default, we shouldn't be here
		};
		
		/**
		 * Joins two condition objects by OR condition.
		 * This function tries to optimeze the output object to avoid excessive object wrapping.
		 * To avoid object clonning the function operates on its arguments, there is no guarantee that the arguments will remain unchanged.
		 * 
		 * @param {Object}	a	first condition object to join (must contain either AND or OR field)
		 * @param {Object}	b	second condition object to join (must contain either AND or OR field)
		 * 
		 * @return {Object} an object containing the first condition object joined by OR with the second condition object. 
		 */
		var joinTwoConditionsByOR = function(a, b) {
			
			var isAEmpty = isConditionObjectEmpty(a);
			var isBEmpty = isConditionObjectEmpty(b); 						
						
			if(isAEmpty && isBEmpty) {
				var emptyObj = {};
				return emptyObj;
			}
			
			if(isAEmpty) {
				// trying to join empty object 'a' with 'b', just return 'b' in this case
				return b;
			}
			
			if(isBEmpty) {
				// trying to join empty object 'b' with 'a', just return 'a' in this case
				return a;
			}
			
			
			if(canConvertANDtoOR(a)) {
				convertANDtoOR(a);
			}
			
			if(canConvertANDtoOR(b)){
				convertANDtoOR(b);
			}
			
			var aHasOwnPropertyAND = a.hasOwnProperty("AND");
			var aHasOwnPropertyOR = a.hasOwnProperty("OR");
			
			var bHasOwnPropertyAND = b.hasOwnProperty("AND");
			var bHasOwnPropertyOR = b.hasOwnProperty("OR");
			
			if(aHasOwnPropertyOR && bHasOwnPropertyOR) {
				// simple array merge will do
				a.OR = a.OR.concat(b.OR);
				return a;
			}
			
			if(aHasOwnPropertyOR && bHasOwnPropertyAND) {
				// add b as array element to a.OR array
				a.OR.push(b);
				return a;
			}
			
			if(aHasOwnPropertyAND && bHasOwnPropertyOR) {
				// add a as array element to b.OR array
				b.OR.unshift(a);
				return b;
			}
			
			if(aHasOwnPropertyAND && bHasOwnPropertyAND) {
				// Need to wrap everything into a new object here				
				var result = {OR: [a, b]};
				return result;
			}
			
			return a; //default, we shouldn't be here
			
		};
		
		
		
		
		////////////////////////////////////////////////////////////////////////
        // Functions for gathering the information about the selected options //
        ////////////////////////////////////////////////////////////////////////		
		
		/**
		 * Gathers all selected options in the selected element group and returns them as an OR condition object.
		 * 
		 * @param {JQuery}	$allItemsOption	an option that represents all items, can be null if there is no such option
		 * @param {JQuery}	$rootGroup	element group in which to search for selected options
		 * 
		 * @return {Object} an OR condition object containing all selected options in the selected element group.
		 */
		var buildSelectedOptionsObjectAsOR = function($allItemsOption, $rootGroup) {

			var selectedOptions = {OR: []};
						
			if ($allItemsOption != null && $allItemsOption.is(':checked')) {
				
				var allItemsOptionValue = $allItemsOption.val();
				if (allItemsOptionValue !== "") {
					selectedOptions.OR.push(allItemsOptionValue);
				}
				
			} else {
			
				var $selectedOptions = $("input:checkbox:checked", $rootGroup);

				$selectedOptions.each(function(i, curOption) {
						selectedOptions.OR.push(curOption.value);
				});
			}
			
			return selectedOptions;
		};
			
		/**
		 * Gathers infomation about the cohort status and returns it as an object.
		 * Returned information includes: semester, year and cohort.
		 * 
		 * @return {Object} A condition object containing the information about the cohort status in the AND field. Return value can be null if nothing is selected. 
		 */
		var buildCohortStatusObject = function() {
			
			var result = {};
			
        	if ($("#cohort_status_specified_students", $sectionC).is(':checked')) {
        		var semester = $("#designate_term_semester", $cohortStatus).val();
        		var year = $("#designate_term_year", $cohortStatus).val();
        		var cohort = $("input[name=cohort_status_terms]:checked", $cohortStatus).val();
        		
        		result = {
					AND: [semester, "designate_term_year_" + year, cohort]
				};				
        	}
        	
        	return result;        	
		};
		
		/**
		 * Gathers infomation about the registration status and returns it as an object.
		 *  
		 * @return {Object} A condition object containing the information about the registration status.  
		 */
		var buildRegistrationStatusObject = function() {
			
			var registrationStatus = {};
			
			if($("#reg_status_only_designated_statuses", $sectionC).is(':checked')) {						
				
				var selectedRegisteredOR = buildSelectedOptionsObjectAsOR(null, $(".reg_status .sub_group", $sectionC));
				var selectedCurrencyOR = buildSelectedOptionsObjectAsOR(null, $(".current_or_not .sub_group", $sectionC));
				var selectedWithdrawnOR = buildSelectedOptionsObjectAsOR(null, $(".student_reg_status .sub_group", $sectionC));
				
				registrationStatus = joinTwoConditionsByAND(registrationStatus, selectedRegisteredOR);
				registrationStatus = joinTwoConditionsByAND(registrationStatus, selectedCurrencyOR);
				registrationStatus = joinTwoConditionsByAND(registrationStatus, selectedWithdrawnOR);
			}	
			
			return registrationStatus;			
		};
		
		/**
		 * Gathers all undergraduates related information and returns it as an object.
		 * Returned information includes: undergraduate majors, levels, 'admitted as' status and 'declared' status. 
		 * 
		 * @return {Object} A condition object containing all undergraduates related information in the AND field.
		 */
		var buildUndergradsObjectAsAND = function() {

			var undergrads = {};
			
			var selectedUndergradMajorsOR = buildSelectedOptionsObjectAsOR(null, $(".majors"));
			var selectedLevelsOR = buildSelectedOptionsObjectAsOR(null, $(".levels"));
			var selectedAdmittedAsOR = buildSelectedOptionsObjectAsOR(null, $(".admittedAs"));
			var selectedDeclaredOR = buildSelectedOptionsObjectAsOR(null, $(".declared"));

			undergrads = joinTwoConditionsByAND(undergrads, selectedUndergradMajorsOR);											
			undergrads = joinTwoConditionsByAND(undergrads, selectedLevelsOR);
			undergrads = joinTwoConditionsByAND(undergrads, selectedAdmittedAsOR);
			undergrads = joinTwoConditionsByAND(undergrads, selectedDeclaredOR);						
			
			return undergrads;	
		};
		
		/**
		 * Gathers all graduate students related information and returns it as an object.
		 * Returned information includes: graduate programs, certificates, emphases, degrees. 
		 * 
		 * @return {Object} A condition object containing all graduate students related information in the AND field.
		 */
		var buildGradsObjectAsAND = function() {

			var grads = {};
			
			var selectedGradProgramsOR = buildSelectedOptionsObjectAsOR(null, $(".programs"));
			var selectedCertificatesOR = buildSelectedOptionsObjectAsOR(null, $(".certificates"));
			var selectedEmphasesOR = buildSelectedOptionsObjectAsOR(null, $(".emphases"));
			var selectedDegreesOR = buildSelectedOptionsObjectAsOR(null, $(".degrees"));
						
			grads = joinTwoConditionsByAND(grads, selectedGradProgramsOR);
			grads = joinTwoConditionsByAND(grads, selectedCertificatesOR);			
			grads = joinTwoConditionsByAND(grads, selectedEmphasesOR);
			grads = joinTwoConditionsByAND(grads, selectedDegreesOR);
						
			return grads;	
		};
		
		
		/**
		 * Gathers all selected options for both undergraduate and graduate students (if available).
		 * Returns these options as an appropriate condition object.
		 * 
		 * @return {Object} a condition object containing all selected options for undergraduate and graduate students.
		 */		
		var buildUndergraduatesAndGraduatesResultingObject = function (){
			
			var boolIncludeUndergrads = false;
			var boolIncludeGrads = false;  
			
			if(boolTemplateHasUndergradsData && boolTemplateHasGradsData) {
				// If the both checkboxes are available, need to check their statuses
				boolIncludeUndergrads = $includeUndergradsCheckbox.length > 0 && $includeUndergradsCheckbox.is(":checked");
				boolIncludeGrads = $includeGradsCheckbox.length > 0 && $includeGradsCheckbox.is(":checked");
								
			} else {
				if(boolTemplateHasUndergradsData) {
					// Only undergrads are available, no checkbox
					boolIncludeUndergrads = true;
				} else if(boolTemplateHasGradsData) {
					// Only grads are available, no checkbox
					boolIncludeGrads = true;
				}
			}
			
			
			// For undergrads
			var undergrads = {};			
			
			if (boolIncludeUndergrads) {
				undergrads = buildUndergradsObjectAsAND();
				if(isConditionObjectEmpty(undergrads)) {
					undergrads ={OR: [$includeUndergradsCheckbox.val()]};
				}				
			}
			
			
			// For grads
			var grads = {};
			
			if (boolIncludeGrads) {
				grads = buildGradsObjectAsAND();
				if(isConditionObjectEmpty(grads)) {
					grads ={OR: [$includeGradsCheckbox.val()]};
				}
								
			}
			
			var result = {};
									
			if (boolIncludeUndergrads && boolIncludeGrads) {
				
				result = joinTwoConditionsByOR(undergrads, grads);
																								
			} else if (boolIncludeUndergrads) {
				
				result = undergrads;								
				
			} else if (boolIncludeGrads) {
				
				result = grads;												
			}
			
			return result;
		};
		
		//////////////////////////
		// UI related functions //
		//////////////////////////
		
		/**
		 * Enables all graduate students section of the form programmatically.
		 * Must be called AFTER loading template.
		 */		
		var enableGradsSection = function() {
			$includeGradsCheckbox.attr("checked", "checked");
			$("input", $gradsGroup).removeAttr("disabled");						
			$gradsGroup.removeClass("disabled");
		};
		
		/**
		 * Enables all undergraduate students section of the form programmatically.
		 * Must be called AFTER loading template.
		 */		
		var enableUndergradsSection = function() {
			$includeUndergradsCheckbox.attr("checked", "checked");
			$("input", $undergradsGroup).removeAttr("disabled");						
			$undergradsGroup.removeClass("disabled");
		};
		
		/**
		 * Populates designate term Year listbox in the section C with the last 5 year numbers.
		 * Current year becomes the default choice. 
		 */
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
		};
	    
	    /**
	     * Resets the editing form UI (checkboxes and CSS styles)
	     * Must be called AFTER loading template.
	     */
	    var resetListEditingFormCheckboxesAndStyles = function() {
	    	
	    	// reset all checkboxes
	    	$("input:checkbox:checked", $("#create_new_list")).removeAttr("checked");
	    	
	    	$("#list_name").val("");
	    	$("#description").text("");
	    	$undergradsGroup.addClass("disabled");
			$gradsGroup.addClass("disabled");
								
			$("input", $gradsGroup).attr("disabled", "disabled");				
			$("input", $undergradsGroup).attr("disabled", "disabled");
			
			// section c
			$("#reg_status_include_all", $sectionC).attr("checked", "checked");
			$("#cohort_status_all_students", $sectionC).attr("checked", "checked");
			$("#cohort_status_term_before", $sectionC).attr("checked", "checked");

			$("#designate_term_semester option:first", $sectionC).attr("selected", "selected");
			$("#designate_term_year option:last", $sectionC).attr("selected", "selected");
			


			$("#special_program_all_students", $sectionC).attr("checked", "checked");
			$("#student_status_all_students", $sectionC).attr("checked", "checked");
			$("#residency_status_all_students", $sectionC).attr("checked", "checked");
			
	    };
		
		///////////////////////////////////////////////////////////////
        // Functions for loading the information from a dynamic list //
        ///////////////////////////////////////////////////////////////		

		var dumpOptionIDs = function(filterObj, idArray) {
			
			if(filterObj == null || idArray == null) {
				return;
			}
			
			var conditionArray;
			if (filterObj.hasOwnProperty("AND")) {
				conditionArray = filterObj.AND;
			} else if (filterObj.hasOwnProperty("OR")) {
				conditionArray = filterObj.OR;
			} else {
				// empty object
				return;
			}
			
			
			for (var i=0; i < conditionArray.length; i++) {
				var arrayElement = conditionArray[i];
				if(jQuery.type(arrayElement) === "string") {
					idArray.push(conditionArray[i]);
				} else {
					// assuming the object is a container
					dumpOptionIDs(arrayElement, idArray);
				}
			};
						
		};
				
		var getNumberOfOptionsInGroup = function($groupRoot) {
			 return $("input:checkbox", $groupRoot).length;
		};
		
		var getNumberOfSelectedOptionsInGroup = function($groupRoot) {
			 return $("input:checkbox:checked", $groupRoot).length;
		};
		
		var isSomethingSelectedInUndergradsSection = function () {			
			return getNumberOfSelectedOptionsInGroup($undergradsGroup) > 0;
		};
		
		var isSomethingSelectedInGradsSection = function () {
			return getNumberOfSelectedOptionsInGroup($gradsGroup) > 0;			
		};
				
		var isSomethingSelectedInRegistrationStatusSection = function () {
			var totalOptionsSelected = getNumberOfSelectedOptionsInGroup($(".reg_status .sub_group", $sectionC)) +
			getNumberOfSelectedOptionsInGroup($(".current_or_not .sub_group", $sectionC)) +
			getNumberOfSelectedOptionsInGroup($(".student_reg_status .sub_group", $sectionC));

			return totalOptionsSelected	> 0;			
		};
		
		var isSomethingSelectedInSpecialProgramsSection = function () {
			return getNumberOfSelectedOptionsInGroup($(".special_programs .sub_group", $sectionC)) > 0; 
		};
		
		var isSomethingSelectedInStudentStatusSection = function () {
			return getNumberOfSelectedOptionsInGroup($(".student_and_residency_status_col_left .sub_group", $sectionC)) > 0; 
		};
		
		var isSomethingSelectedInResidencyStatusSection = function () {
			return getNumberOfSelectedOptionsInGroup($(".student_and_residency_status_col_right .sub_group", $sectionC)) > 0; 
		};
				
		var areAllOptionsInGroupSelected = function ($groupRoot) {
			 var numberOfOptions = getNumberOfOptionsInGroup($groupRoot);
			 var numeberOfSelectedOptions = getNumberOfSelectedOptionsInGroup($groupRoot);
			 
			 return numeberOfSelectedOptions > 0 && numberOfOptions === numeberOfSelectedOptions;    
		};
		
		
		////////////////////////////////
        // Dynamic list save function //
        ////////////////////////////////		
		
		/**
		 * Saves the selected options a new dynamic list
		 */
		/*var saveDynamicList = function() {
						
			var result = buildUndergraduatesAndGraduatesResultingObject();
			
			// Section C

			var registrationStatus = buildRegistrationStatusObject();
			result = joinTwoConditionsByAND(result, registrationStatus);
			

			var cohortStatus = buildCohortStatusObject();						
			result = joinTwoConditionsByAND(result, cohortStatus);
			
			
			var specialPrograms = buildSelectedOptionsObjectAsOR($("#special_program_all_students", $sectionC), $(".special_programs", $sectionC));			
			result = joinTwoConditionsByAND(result, specialPrograms);
			
			var studentStatus = buildSelectedOptionsObjectAsOR($("#student_status_all_students", $sectionC), $(".student_and_residency_status_col_left .sub_group", $sectionC));			
			result = joinTwoConditionsByAND(result, studentStatus);
			
			var residencyStatus = buildSelectedOptionsObjectAsOR($("#residency_status_all_students", $sectionC), $(".student_and_residency_status_col_right .sub_group", $sectionC));			
			result = joinTwoConditionsByAND(result, residencyStatus);
			 
			

			
			return $outputJson.val(formatJSON(result));
		}*/
	    
	    /*-------------------------------- Roman ------------------------------------------*/
	    
	    
	    
	    /**
	     *
	     * Configuration
	     *
	     */
	    var submitData = {}; // Data to be passed to saveJSON
	    var allLists = []; // Array of all the lists
	    var userUrl;
	    var generalMessageFadeOutTime = 3000; // The amount of time it takes till the general message box fades out
	    var sortBy = "name";
	    var sortOrder = "descending";
	    var editExisting = false;
	    var currList;

	
	
	    /**
	     *
	     * CSS IDs
	     *
	     */
	    var inboxID = "#inbox";
	    var inboxClass = ".inbox";
	    var inbox = "inbox";
	    var inboxArrow = inboxClass + "_arrow";
	    var inboxTable = inboxID + "_table";
	    var inboxTablePreloader = inboxTable + "_preloader";
	    var inboxGeneralMessage = inboxID + "_general_message";
	    var inboxMessageError = inbox + "_error_message";
	
	    /**
	     * This will show the preloader.
	     */
	    var showLoader = function(){
	        $(inboxTable).append(sakai.api.Util.TemplateRenderer(inboxTablePreloader.substring(1), {}));
	    };
	    
	    /**
	     * Shows a general message on the top screen
	     * @param {String} msg    the message you want to display
	     * @param {Boolean} isError    true for error (red block)/false for normal message(green block)
	     */
	    var showGeneralMessage = function(msg, isError){
	    
	        // Check whether to show an error type message or an information one
	        var type = isError ? sakai.api.Util.notification.type.ERROR : sakai.api.Util.notification.type.INFORMATION;
	        
	        // Show the message to the user
	        sakai.api.Util.notification.show("", msg, type);
	        
	    };
	    
	    /**
	     * Check or uncheck all messages depending on the top checkbox.
	     */
	    var tickMessages = function(){
	        $(".inbox_inbox_check_list").attr("checked", ($("#inbox_inbox_checkAll").is(":checked") ? "checked" : ''));
	    };
	    
	    var getDataFromInput = function() {
	        var result = {};
			
			result.context = "g-ced-students";
	        result.listName = $.trim($("#list_name").val());
			if(result.listName == null || result.listName == "") {
				$("#invalid_name").show();
				return -1;
			}
	        result.desc = $.trim($("#description").val());
	        
	        // NOT SUPPORTED FOR POC
	        // result.size = $("#list_size").val();
	        
			// Gathering the data on standing
			//TODO: check for errors
			/*if($("#undergrad:checked").val() == null && $("#grad:checked").val() == null) {
				$("#invalid_major").show();
					return -1;
			}*/
			
			var dynamicListFilter = buildUndergraduatesAndGraduatesResultingObject();
			
			// Section C

			var registrationStatus = buildRegistrationStatusObject();
			dynamicListFilter = joinTwoConditionsByAND(dynamicListFilter, registrationStatus);
			

			var cohortStatus = buildCohortStatusObject();						
			dynamicListFilter = joinTwoConditionsByAND(dynamicListFilter, cohortStatus);
			
			
			var specialPrograms = buildSelectedOptionsObjectAsOR($("#special_program_all_students", $sectionC), $(".special_programs", $sectionC));			
			dynamicListFilter = joinTwoConditionsByAND(dynamicListFilter, specialPrograms);
			
			var studentStatus = buildSelectedOptionsObjectAsOR($("#student_status_all_students", $sectionC), $(".student_and_residency_status_col_left .sub_group", $sectionC));			
			dynamicListFilter = joinTwoConditionsByAND(dynamicListFilter, studentStatus);
			
			var residencyStatus = buildSelectedOptionsObjectAsOR($("#residency_status_all_students", $sectionC), $(".student_and_residency_status_col_right .sub_group", $sectionC));			
			dynamicListFilter = joinTwoConditionsByAND(dynamicListFilter, residencyStatus);
			
	        result.filter = $.toJSON(dynamicListFilter);
	        
	        return result;
	    }
	    
	    // NOT SUPPORTED FOR POC
	    /**
	     * Updates the input field that displays the current size of the list being created
	     */
	    sakai_global.listpage.updateListSize = function(){
	        var data = getDataFromInput();
			if(data < 0) {
				alert("Error");
			}
	        
	        // STILL NEEDS TO BE IMPLEMENTED
	        // var size = query(selectedContext, selectedMajor, selectedStanding);
	        // document.createListForm.list_size.value = size;
	    }
	    	    
	    /**
	     * Removes all the messages out of the DOM.
	     * It will also remove the preloader in the table.
	     */
	    var removeAllListsOutDOM = function(){
	        $(".inbox_list").remove();
	    };
	    
	    var removeSparseSpecialProperties = function(obj) {
	    	for (var key in obj) {
				if(obj.hasOwnProperty(key) && key.match(/^_/)) {
					delete obj[key];
				}
			}
	    };
	    
	    var renderLists = function(response){
	        allLists = response.lists || [];
	        
	        // removing special _properties
	        removeSparseSpecialProperties(allLists);
	        
	        var data = {
	            "links": allLists,
	            sakai: sakai
	        }
	        
	        // remove previous lists
	        removeAllListsOutDOM();
	        
	        // Add them to the DOM
	        $(inboxTable).children("tbody").append(sakai.api.Util.TemplateRenderer("#inbox_inbox_lists_template", data));
	        
	        // do checkboxes
	        tickMessages();
	    }
	    
	    /**
	     * Get the dynamic list out of the list with the specific id.
	     * @param {String} id    The id of a dynamic list
	     */
	    var getListWithId = function(id){
	        if(allLists.hasOwnProperty(id)) {
	        	return allLists[id];
	        }
	        /*for (var i = 0, j = allLists.length; i < j; i++) {
	            if (allLists[i]["sakai:id"] === id) {
	                return allLists[i];
	            }
	        }*/
	        return null;
	    };
	    
	    /**
	     * Returns the index of the list with the specific id.
	     * @param {Object} id
	     */
	    var getIndexFromId = function(id) {
	        for (var i = 0, j = allLists.length; i < j; i++) {
	            if (allLists[i]["sakai:id"] === id) {
	                return i;
	            }
	        }
	        return -1;
	    }
	    
	    /**
	     * Displays only the list with that id.
	     * @param {String} id    The id of a list
	     */
	    var displayList = function(id){
	        // Display edit list tab
	        $.bbq.pushState({"tab": "new"},2);
			//clearInputFields();
			
			resetListEditingFormCheckboxesAndStyles();
				
	        
	        // Fill in input fields with list data
	        var list = getListWithId(id);
	        $("#list_name").val(list["sakai:name"]);
	        $("#description").text(list["sakai:description"]);
	        	        
	        var idArray = [];
	        dumpOptionIDs(list.query.filter, idArray);
	        var includeAllGradsId = $includeGradsCheckbox.val();
	        var includeAllUndergradsId = $includeUndergradsCheckbox.val();
	        
	        	        
	        for (var i=0; i < idArray.length; i++) {
				var currentId = idArray[i];
				
				// cohort status - designate term - year
				if(currentId.match(/^designate_term_year_/)){
					
					var year = currentId.substring(20); // 20 is the length of 'designate_term_year_'
					$("option[value='" + year +"']", $cohortStatus).attr("selected","selected");
					
					// we need to select 'Only include students in designated cohort(s)' option if we got here,
					// because we can have the year option only if this option is selected 
					$("#cohort_status_specified_students", $cohortStatus).attr("checked","checked");
					
					continue;
				}
				
				
				switch(currentId) {
					case includeAllGradsId:
						enableGradsSection();						
						break;
					case includeAllUndergradsId:
						enableUndergradsSection();
						break;
					case "designate_term_semester_spring":
					case "designate_term_semester_fall":
						// fall-through is intentional here
						$("option[value='" + currentId +"']", $sectionC).attr("selected","selected");
						break;
					default:
						$("input[value='" + currentId +"']").attr("checked","checked");	
						break;
				}
					
			}
			
			if(isSomethingSelectedInUndergradsSection()) {
				enableUndergradsSection();
			}
			
			if(isSomethingSelectedInGradsSection()) {
				enableGradsSection();
			}
			
			// registration status radio buttons (section c)
			if(isSomethingSelectedInRegistrationStatusSection()) {
				$("#reg_status_only_designated_statuses", $sectionC).attr("checked", "checked");
			}
			
			
			
			
			
			// processing 'select all' checkboxes (section c)
			if(areAllOptionsInGroupSelected($(".reg_status .sub_group", $sectionC))) {
				$('#reg_status_select_all_in_group', $sectionC).attr("checked", "checked");
			}
			
			if(areAllOptionsInGroupSelected($(".current_or_not .sub_group", $sectionC))) {
				$('#currency_status_select_all_in_group', $sectionC).attr("checked", "checked");
			}
			
			if(areAllOptionsInGroupSelected($(".student_reg_status .sub_group", $sectionC))) {
				$('#student_reg_status_select_all_in_group', $sectionC).attr("checked", "checked");
			}
	        
	        
	        
	        
			// special programs radio buttons (section c)
			if(isSomethingSelectedInSpecialProgramsSection()) {
				$("#special_program_specified_students", $sectionC).attr("checked", "checked");
			}
			
			// Student status radio buttons (section c)
			if(isSomethingSelectedInStudentStatusSection()) {
				$("#student_status_specified_students", $sectionC).attr("checked", "checked");
			}
			
			// Residency status radio buttons (section c)
			if(isSomethingSelectedInResidencyStatusSection()) {
				$("#residency_status_specified_students", $sectionC).attr("checked", "checked");
			}
	        
	        
	        // NOT SUPPORTED FOR POC
	        // document.createListForm.list_size.value = list["sakai:size"];
	        
	    }
	
	    // Check all messages
	    $("#inbox_inbox_checkAll").change(function(){
	        tickMessages();
	    });
	    
	    // Sorters for the inbox.
	    /* Commented out for the myBerkeley POC since sorting is broken - eli
	    $(".inbox_inbox_table_header_sort").bind("mouseenter", function() {
	        if (sortOrder === 'descending') {
	            $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortUp).html()));
	        }
	        else {
	            $(this).append(sakai.api.Security.saneHTML($(inboxInboxSortDown).html()));
	        }
	    });
	    
	    $(".inbox_inbox_table_header_sort").bind("mouseout", function() {
	        $(inboxTable + " " + inboxArrow).remove();
	    });
	    
	    $(".inbox_inbox_table_header_sort").bind("click", function() {
	        sortBy = $(this).attr("id").replace(/inbox_table_header_/gi, "");
	        sortOrder = (sortOrder === "descending") ? "ascending" : "descending";
	
	        getAllMessages(); // no such function defined, but the function should sort the data and re-populate the inbox
	    });
	    */
	    
	    var deleteLists = function(listsArray) {
	        var listId = listsArray;
	        
	        for (var i = 0, j = listId.length; i < j; i++) {
	            var index = getIndexFromId(listId[i]);
	            if(index >= 0) {
	                $("#inbox_table_list_" + listId[i]).empty();
	                $("#inbox_table_list_" + listId[i]).remove();
	                
	                if (allLists.length == 1) {
	                    sakai.api.Server.removeJSON(userUrl, loadData);
	                    return;
	                } else {
	                    allLists.splice(index, 1);
	                }
	            } else {
	                alert("Error: list not found");
	            }
	        }
	        
	        submitData.lists = allLists;
	        sakai.api.Server.saveJSON(userUrl, submitData, loadData);
	    };
	    
		/**
	     * Returns a value indicating whether the specified list object already exists.
	     *
	     * @param {Object} listToCheck The list to seek 
	     * @return {Boolean} true if the value parameter occurs within the existing lists; otherwise, false
	     */
		var listAlreadyExists = function(listToCheck) {
			
			for (var i = 0, j = allLists.length; i < j; i++) {
				var exsistingList = allLists[i];
				var exsistingListObj = {
					"context" : exsistingList.query.context[0], 
					"listName": exsistingList["sakai:name"],
					"desc": exsistingList["sakai:description"],
					"filter": exsistingList.query.filter				
				} 
				
				if(listEquals(exsistingListObj, listToCheck)) return true;
			}
			
			return false;				
		};
		
		/**
	     * Compares two specified list objects.
	     *
	     * @param {Object} list1 The first list
	     * @param {Object} list2 The The second list
	     * @return {Boolean} true if the lists are equal; otherwise false
	     */
		var listEquals = function(list1, list2) {
			
			return false;
			//TODO: debug
					
			if(list1.context !== list2.context) return false;
			if(list1.listName !== list2.listName) return false;
			if(list1.desc !== list2.desc) return false;
	
			var list1Standing = list1.standing;
			var list2Standing = list2.standing;
					
			var list1StandingLength = list1Standing.length;
			var list2StandingLength = list2Standing.length;
			
			if(list1StandingLength !== list2StandingLength) return false;
			
			// Comparing lists of majors
			
			var list1UndergradMajors = [];
			var list2UndergradMajors = [];
			var list1GradMajors = [];
			var list2GradMajors = [];
			
			// Assigning udergraduate and graduate majors to appropriate variables
			// Assuming that list1StandingLength == list2StandingLength
			for (var i = 0; i < list1StandingLength; i++) {
				
				if(list1Standing[i].hasOwnProperty("undergrad")) {
					list1UndergradMajors = list1Standing[i].undergrad.major;
				} else if(list1Standing[i].hasOwnProperty("grad")) {
					list1GradMajors = list1Standing[i].grad.major;
				}
				
				if(list2Standing[i].hasOwnProperty("undergrad")) {
					list2UndergradMajors = list2Standing[i].undergrad.major;
				} else if(list2Standing[i].hasOwnProperty("grad")) {
					list2GradMajors = list2Standing[i].grad.major;
				}
			}
			
			if(list1UndergradMajors.length !== list2UndergradMajors.length) return false;
			if(list1GradMajors.length !== list2GradMajors.length) return false;
			
			// Comparing undergraduate majors lists (assuming that both arrays have the same length)
			for(var i = 0, j = list1UndergradMajors.length; i < j; i++) {
				if(list2UndergradMajors.indexOf(list1UndergradMajors[i]) === -1) return false;
			}
			
			// Comparing graduate majors lists (assuming that both arrays have the same length)
			for(var i = 0, j = list1GradMajors.length; i < j; i++) {
				if(list2GradMajors.indexOf(list1GradMajors[i]) === -1) return false;
			}
			
			return true;
		}
	    
	    var finishSaveAndLoad = function() {
	        //clearInputFields();
	        $.bbq.pushState({"tab": "existing"},2);
	        loadData();
	    };
	    
	    var generateId = function() {
	        var id = "dl-" + sakai.data.me.user.userid + "-" + new Date().getTime();
	        return id;
	    }
	    
	    var saveList = function(data, index) {        
	        if (listAlreadyExists(data)) {
	            showGeneralMessage($("#inbox_generalmessages_already_exists").text());
	            return;
	        }
	        
	        if(index != null && index >= 0) { // we are editing an existing list
	            allLists[index]["sakai:name"] = data.listName;
	            allLists[index]["sakai:description"] = data.desc;
	            //allLists[index]["sakai:dateModified"] = new Date();
	            //allLists[index]["sakai:dateModified@TypeHint"] = "Date";
	            //allLists[index]["sakai:modifiedBy"] = sakai.data.me.user.userid;
	            allLists[index].query.context = [data.context];
	            allLists[index].query.filter = data.filter;
	        } else { // we are creating a new list
	            var id = generateId();
	
	            var list = {
	                "sakai:id": id,
	                "sakai:name": data.listName,
	                "sakai:description": data.desc,
	                //"sakai:dateModified": new Date(),
	                //"sakai:dateModified@TypeHint": "Date",
	                //"sakai:modifiedBy": sakai.data.me.user.userid,
	                "query": {
	                    "context": [data.context],
	                    "filter": data.filter
	                }
	            }
	            //allLists.push(list);
	        }
	       // submitData.lists = allLists;
	        
	        
	        var list = {
	                "sakai:id": id,
	                "sakai:name": data.listName,
	                "sakai:description": data.desc,
	                //"sakai:dateModified": new Date(),
	                //"sakai:dateModified@TypeHint": "Date",
	                //"sakai:modifiedBy": sakai.data.me.user.userid,
	                "query": {
	                    "context": [data.context],
	                    "filter": data.filter
	                }
	              };
	        
	        sakai.api.Server.saveJSON(userUrl+"/lists/"+id, list, finishSaveAndLoad);
	    };
	    
	    var saveListOld = function(data, index) {        
	        if (listAlreadyExists(data)) {
	            showGeneralMessage($("#inbox_generalmessages_already_exists").text());
	            return;
	        }
	        
	        if(index != null && index >= 0) { // we are editing an existing list
	            allLists[index]["sakai:name"] = data.listName;
	            allLists[index]["sakai:description"] = data.desc;
	            allLists[index]["sakai:dateModified"] = new Date();
	            //allLists[index]["sakai:dateModified@TypeHint"] = "Date";
	            allLists[index]["sakai:modifiedBy"] = sakai.data.me.user.userid;
	            allLists[index].query.context = [data.context];
	            allLists[index].query.filter = data.filter;
	        } else { // we are creating a new list
	            var id = generateId();
	
	            var list = {
	                "sakai:id": id,
	                "sakai:name": data.listName,
	                "sakai:description": data.desc,
	                "sakai:dateModified": new Date(),
	                "sakai:dateModified@TypeHint": "Date",
	                "sakai:modifiedBy": sakai.data.me.user.userid,
	                "query": {
	                    "context": [data.context],
	                    "filter": data.filter
	                }
	            }
	            allLists.push(list);
	        }
	        submitData.lists = allLists;
	        sakai.api.Server.saveJSON(userUrl, submitData, finishSaveAndLoad);
	    };
	    
		// List creation events
	   
	  
	    // Button click events
	    $("#dyn_lists_delete_button").live("click", function(){
	        var listId = [];
	        $(".inbox_inbox_check_list:checked").each(function(){
	            var id = $(this).val();
	            listId.push(id);
	        });
	        
	        $("#inbox_inbox_checkAll").attr("checked", "");
	        tickMessages();
	
	        if (listId.length < 1) {
	            showGeneralMessage($("#inbox_generalmessages_none_selected").text());
	        } else {
	            deleteLists(listId);
	        }
	    });
	    
	    $("#inbox_inbox_duplicate_button").live("click", function(){
	        var listId = [];
	        $(".inbox_inbox_check_list:checked").each(function(){
	            var id = $(this).val();
	            listId.push(id);
	        });
	        
	        $("#inbox_inbox_checkAll").attr("checked", "");
	        tickMessages();
	
	        if (listId.length < 1) {
	            showGeneralMessage($("#inbox_generalmessages_none_selected").text());
	        } else if (listId.length > 1) {
	            showGeneralMessage($("#inbox_generalmessages_duplicate_multiple").text());
	        } else {
	            displayList(listId[0]);
	        }
	    });
	    
	    $(".editLink").live("click", function(evt){   
	        editExisting = true;
	        var id = evt.target.id;
	        currList = id;
	        displayList(id);
	    });
	    
	    $("#inbox_inbox_back_button").live("click", function(){
	        window.location = "/dev/inboxnotifier.html";
	    });
	    
	    $("#inbox_inbox_cancel_button").live("click", function(){
	        editExisting = false;
	        //clearInputFields();
	        $.bbq.pushState({"tab": "existing"},2);
	    });
	    
	    $("#dynlist_save_button").live("click", function(){
	        $("#invalid_name").hide();
	        $("#invalid_major").hide();
	        // NOT SUPPORTED FOR POC
	        //$("#invalid_size").hide();
			
	        var data = getDataFromInput();
			if(data < 0) {
				return;
			}
	        
	        // NOT SUPPORTED FOR POC
	        // var size = $("#list_size").val();
	        
			// In IE browser jQuery.trim() function doesn't work this way $('#selector').text().trim()
			// It should be called like this $.trim($('#selector').text())
			// See http://bit.ly/d8mDx2
	        if (editExisting) {
	            saveList(data, getIndexFromId(currList));
				editExisting = false;
	        } else {
	            saveList(data, null);
	        }
	    });
	    
	    // NOT SUPPORTED FOR POC
	    // On selecting checkbox events
	    /*
	    $(".major_checkbox").click(function() {
	        sakai_global.listpage.updateListSize();
	    });
	    */
	 
	    
	    
	    
	    var switchToEditMode = function() {
	    	
	    	// Set headers and tab styling
	    	$("h1.title").text("Dynamic List Editor");
	    	
	    	$("#existing_lists").hide();	    	
	    	$("#create_new_list").show();
	      
	        
	        // Show/hide appropriate buttons
	        
	        $("#dynamic_lists_create_new").hide();
	        
	        $("#dyn_lists_delete_button").hide();
	        $("#dyn_lists_copy_button").hide();
	        $("#dyn_lists_edit_button").hide();
	        
	        $("#inbox_inbox_cancel_button").show();
	        $("#dynlist_save_button").show();
	    };
	    
	    var switchToListMode = function() {
	    	
	    	// Set headers and tab styling
	    	
	    	$("h1.title").text("Dynamic List Manager");
	        $("#create_new_list").hide();
	        $("#existing_lists").show();
	        
	        
	        // Show/hide appropriate buttons
	        
	        
	        $("#dynamic_lists_create_new").show();
	        
	        $("#dyn_lists_delete_button").show();
	        $("#dyn_lists_copy_button").show();
	        $("#dyn_lists_edit_button").show();
	        
	        $("#inbox_inbox_cancel_button").hide();
	        $("#dynlist_save_button").hide();
	        
	        /*$("#inbox_inbox_cancel_button").hide();
	        $("#dynlist_save_button").hide();
	        $("#inbox_inbox_delete_button").show();
	        $("#inbox_inbox_duplicate_button").show();
	        $("#inbox_inbox_back_button").show();*/
	    	
	    };
	    
	    
	    /*-------------------------------- Roman ------------------------------------------*/
	    $("#dynamic_lists_create_new").click(function() {
	        $.bbq.pushState({"tab": "new"},2);
	        
	        // NOT SUPPORTED FOR POC
	        // sakai_global.listpage.updateListSize();
	        
			resetListEditingFormCheckboxesAndStyles();
	        switchToEditMode();
	    });
	    /*-------------------------------- Roman ------------------------------------------*/
	    
	    
	    
	    var setTabState = function(){
	        var tab = $.bbq.getState("tab");
	        if (tab) {
	            switch (tab) {
	                case "existing":
	                    switchToListMode();
	                    break;
	                case "new":
	                    switchToEditMode();
	                    break;
	            }
	        }
	    };
	    
	    $(window).bind('hashchange', function(e) {
	        setTabState();
	    });
	    
	    var createDefaultList = function() {
	        allLists = [];
	        var emptyData = {
	            "links": []
	        }
	        
	        $(inboxTable).children("tbody").append(sakai.api.Util.TemplateRenderer("#inbox_inbox_lists_template", emptyData));
	    }
	    
	    /**
	     * Takes the data returned by the server and formats it to get it ready for posting to the server
	     * @param {Object} data
	     */
	    var formatData = function(data) {
	       
	       // filter data is stored as a string, it needs to be parsed 
	        for (var i=0; i < data.lists.length; i++) {
				var list = data.lists[i];
				list.query.filter = $.parseJSON(list.query.filter);
			}
			 
	        var result =  {
	            "lists": data.lists
	        };
	        return result;
	    }
	    
	    var loadData = function() {
	        sakai.api.Server.loadJSON(userUrl, function(success, data){
	            //$("#tabs").tabs();
	            setTabState();
	            
	            if (success) {
	                submitData = formatData(data);
	                renderLists(submitData);
	            } else {
	                createDefaultList();
	            }
	        });
	    }

		
	    /////////////////////////////
        // Initialization function //
        /////////////////////////////

        /**
         * Initialization function that is run when the widget is loaded. Determines
         * which mode the widget is in (settings or main), loads the necessary data
         * and shows the correct view.
         */
	    var doInit = function() {
			var security = sakai.api.Security;		       
	        
	        // if the user is not a member of the advisors group then bail
	        if (!myb.api.security.isUserAnAdvisor()) {
	            security.send403();
	            return;
	        }
	        
	        
	        
	        
	        
	        
	        
	        /*-------------------------------- Roman ------------------------------------------*/
	        
	        
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
			
			$includeUndergradsCheckbox = $("#include_undergrads");
			$includeGradsCheckbox = $("#include_grads");
			
			// Define undergrad and grad groups AFTER template has been rendered
			$undergradsGroup = $(".undergrads_group");
			$gradsGroup = $(".grads_group");
			
			//Disabling all graduate and undergraduate controls
			if (boolTemplateHasUndergradsData && boolTemplateHasGradsData) {
														
				$includeGradsCheckbox.click(function(event){
						if($includeGradsCheckbox.is(':checked')){
							$("input", $gradsGroup).removeAttr("disabled");						
							$gradsGroup.removeClass("disabled");
						} else {
							$("input", $gradsGroup).attr("disabled", "disabled");
							$gradsGroup.addClass("disabled");
						}
											
				});
								
				$includeUndergradsCheckbox.click(function(){
						if($includeUndergradsCheckbox.is(':checked')){
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
            
            var $regStatusSelectAllInGroup = $('#reg_status_select_all_in_group', $sectionC);
			$regStatusSelectAllInGroup.click(function(){
				if($regStatusSelectAllInGroup.is(':checked')) {
					$(".reg_status .sub_group input", $sectionC).attr("checked", "checked");
				} else {
					$(".reg_status .sub_group input", $sectionC).removeAttr("checked");
				}
			});
			
			
			var $currencyStatusSelectAllInGroup = $('#currency_status_select_all_in_group', $sectionC);
			$currencyStatusSelectAllInGroup.click(function(){
				if($currencyStatusSelectAllInGroup.is(':checked')) {
					$(".current_or_not .sub_group input", $sectionC).attr("checked", "checked");
				} else {
					$(".current_or_not .sub_group input", $sectionC).removeAttr("checked");
				}
			});
			
			var $studentRegStatusSelectAllInGroup = $('#student_reg_status_select_all_in_group', $sectionC);
			$studentRegStatusSelectAllInGroup.click(function(){
				if($studentRegStatusSelectAllInGroup.is(':checked')) {
					$(".student_reg_status .sub_group input", $sectionC).attr("checked", "checked");
				} else {
					$(".student_reg_status .sub_group input", $sectionC).removeAttr("checked");
				}
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
			
	        
	        /*-------------------------------- Roman ------------------------------------------*/
	        
	        
	        
	        
	        userUrl = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists";

			loadData();
	    };
	
	    doInit();
    };

    sakai.api.Widgets.Container.registerForLoad("listpage");
});
