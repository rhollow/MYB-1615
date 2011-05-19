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

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core", "/dev/javascript/myb/myb.securepage.js"],
    function($, sakai, myb) {
	sakai_global.listpage = function(){

	    /////////////////////////////
        // Configuration variables //
        /////////////////////////////
        
        /**
		 * Hashtable of all the lists
		 */
		var allLists = {};
		
        /**
         * Dynamic list prefix
         */
        var DYNAMIC_LIST_PREFIX = "dl-";

		/**
		 * Whether the loaded trimpath template includes undergraduate students data
		 */
		var boolTemplateHasUndergradsData = false;
		
		/**
		 * Whether the loaded trimpath template includes graduate students data
		 */
		var boolTemplateHasGradsData = false;			
		
		/**
		 * Dynamic lists base URL (parent node in Sparse), delete this node to remove all dynamic lists for current user
		 */		
		var dynamicListsBaseUrl;
		
		/**
		 * Url for counting number of people targeted by a filter
		 */
		var dynamicListsPeopleCountingUrl = "/var/myberkeley/dynamiclists/g-ced-students.json";
		
		/**
		 * Needed to prevent unnecessary people counting requests when editing a new list
		 */
		var lastUsedFilterString = "";
				
		
		//////////////////////
        // jQuery selectors //
        //////////////////////
                		
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
		 * 'Include undergraduate students' checkbox (or hidden input when checkbox is not displayed)
		 */
		var $includeUndergradsCheckbox;

		/**
		 * 'Include graduate students' checkbox (or hidden input when checkbox is not displayed)
		 */
		var $includeGradsCheckbox;
        
        /**
         * Dynamic list 'Edit' button
         */
		var $dynListsEditButton = $("#dyn_lists_edit_button");
	  	
	  	/**
         * Dynamic list 'Copy' button
         */
	  	var $dynListsCopyButton = $("#dyn_lists_copy_button");
	  	
	  	/**
         * Dynamic list 'delete' button
         */
	  	var $dynListsDeleteButton = $("#dyn_lists_delete_button");
	  		  	
	  	/**
         * Dynamic list 'Back to Group Notification Manager' button
         */
	  	var $dynListsBackToNotifManagerButton = $("#dyn_lists_back_to_group_notification_manager_button");	 
	  	
	  	/**
         * Dynamic list 'Save' button
         */
	  	var $dynListsSaveButton = $("#dyn_lists_save_button");
	  		  	
	  	/**
         * Dynamic list 'Cancel' button (cancels editing and switches withe my to list mode)
         */
	  	var $dynListsCancelEditingButton = $("#dyn_lists_cancel_button");
	  	
	  	/**
	  	 * Dynamic list 'Create' button
	  	 */
	  	var $dynListsCreateButton = $("#dynamic_lists_create_new");
	  	
	  	/**
	  	 * Show more/less button in section C (template must be loaded before using this variable)
	  	 */
	  	var $showMoreOrLess;
	  	
	  	/**
	  	 * HTML div to diplay the number of users targeted by the current list
	  	 */
	  	var $studentsTargetedByCurrentList;
	  	
	  	
	  	var $tableOfLists = $("#inbox_table");
	  	
	    
	    //////////////////////////////////////////////////////////
        // Functions for manipulating boolean condition objects //
        //////////////////////////////////////////////////////////		
		
		/**
         * Checks if the condition object can be converted from OR form to AND form.
         *
         * @return {boolean}true if the condition object can be safely converted from OR form to AND form; otherwise returns {boolean}false.
         */
        var canConvertORtoAND = function(obj) {
            if (!obj.hasOwnProperty("OR")) return false;
            var len = obj.OR.length;
            return len === 1 || len === 0;
        };
		
		/**
         * Checks if the condition object can be converted from AND form to OR form.
         *
         * @return {boolean}true if the condition object can be safely converted from AND form to OR form; otherwise returns {boolean}false.
         */
        var canConvertANDtoOR = function(obj) {
            if (!obj.hasOwnProperty("AND")) return false;
            var len = obj.AND.length;
            return len === 1 || len === 0;
        };
		
		/**
         * Converts the condition object from OR form to AND form.
         * The function doesn't do any checks before conversion.
         *
         * @param {Object}    obj    object to convert
         */
        var convertORtoAND = function(obj) {
            obj.AND = obj.OR;
            delete obj.OR;
        };
		
		/**
         * Converts the condition object from AND form to OR form.
         * The function doesn't do any checks before conversion.
         *
         * @param {Object}    obj    object to convert
         */
        var convertANDtoOR = function(obj) {
            obj.OR = obj.AND;
            delete obj.AND;
        };
		
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
						
			if ($allItemsOption !== null && $allItemsOption.is(':checked')) {
				
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
	     * Hides section C (common filter settings).
	     */
	    var hideSectionC = function() {
	    	$showMoreOrLess.text("Show Less");
	    	$sectionC.hide();
	    };
	    
	    /**
	     * Toggles section C visibility.
	     */
	    var toggleSectionC = function () {
				
			if($sectionC.is(":visible")) {
				hideSectionC();					
			} else {
				$showMoreOrLess.text("Show Less");
				$sectionC.show();					
			}						
		};
	    
	    		
	    /**
	     * Resets the editing form UI (checkboxes and CSS styles)
	     * Must be called AFTER loading template.
	     */
	    var resetListEditingForm = function() {
	    	
	    	
	    	lastUsedFilterString = "";
	    	$studentsTargetedByCurrentList.addClass("noStudentsTargeted").text("0");
	    		    	
	    	hideSectionC();
	    	
	    	// reset all checkboxes
	    	$("input:checkbox:checked", $("#create_new_list")).removeAttr("checked");
	    	
	    	$("#list_name").val("");
	    	$("#description").val("");
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


        //TODO: showLoader is not used, maybe we should remove it
		/**
	     * This will show the preloader.
	     */
	    var showLoader = function(){
	        $tableOfLists.append(sakai.api.Util.TemplateRenderer("inbox_table_preloader", {}));
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
	        updateEditCopyDeleteButtonsStatus();
	    };

		
		/**
		 * Updates the number of people selected by filter string.
		 * 
		 * @param {String} filterString	a condition object represented as a string
		 *
		 */
		var updateNumberOfPeopleSelectedByFilter = function(filterString) {
			
			if(lastUsedFilterString === filterString) {
				 return;
			}
			
			lastUsedFilterString = filterString;

	      	$.ajax({
		        url: dynamicListsPeopleCountingUrl,
		        traditional: true,
		        type: "POST",
		        data: {
		          criteria: filterString
		        },
		        success: function(data) {
		          if(data.count > 0) {
		          	$studentsTargetedByCurrentList.removeClass("noStudentsTargeted");
		          } else {
		          	$studentsTargetedByCurrentList.addClass("noStudentsTargeted");
		          }
		          $studentsTargetedByCurrentList.text(data.count);          
		        },
		        error: function(xhr, textStatus, thrownError) {
		          $studentsTargetedByCurrentList.addClass("noStudentsTargeted");
		          $studentsTargetedByCurrentList.text("N/A");
		        }
	      });

		};

        var switchToEditMode = function() {

	    	// Set headers and tab styling
	    	$("h1.title").text("Dynamic List Editor");

	    	$("#existing_lists").hide();
	    	$("#create_new_list").show();


	        // Show/hide appropriate buttons

	        $dynListsCreateButton.hide();

	        $dynListsDeleteButton.hide();
	        $dynListsCopyButton.hide();
	        $dynListsEditButton.hide();
	        $dynListsBackToNotifManagerButton.hide();

	        $dynListsCancelEditingButton.show();
	        $dynListsSaveButton.show();
	    };

	    var switchToListMode = function() {

	    	// Set headers and tab styling

	    	$("h1.title").text("Dynamic List Manager");
	        $("#create_new_list").hide();
	        $("#existing_lists").show();


	        // Show/hide appropriate buttons


	        $dynListsCreateButton.show();

	        $dynListsDeleteButton.show();
	        $dynListsCopyButton.show();
	        $dynListsEditButton.show();
	        $dynListsBackToNotifManagerButton.show();

	        $dynListsCancelEditingButton.hide();
	        $dynListsSaveButton.hide();
	    };
		
		/**
	     * Removes all the messages out of the DOM.
	     * It will also remove the preloader in the table.
	     */
	    var removeAllListsOutDOM = function(){
	        $(".inbox_list").remove();
	    };
		
		var displayLoadedLists = function() {

            var data = {
                links: allLists,
                sakai: sakai
            };

            // remove previous lists
            removeAllListsOutDOM();

            // Add them to the DOM
            $tableOfLists.children("tbody").append(sakai.api.Util.TemplateRenderer("#inbox_inbox_lists_template", data));

            // do checkboxes
            tickMessages();
        };

        /**
         * Return the number of options (checkboxes) in a group.
         *
         * @param {jQuery} $groupRoot jQuery selector
         *
         * @return {Integer} the number of options (checkboxes) in the specified group.
         */
        var getNumberOfOptionsInGroup = function($groupRoot) {
             return $("input:checkbox", $groupRoot).length;
        };

        /**
         * Return the number of checked options (checkboxes) in a group.
         *
         * @param {jQuery} $groupRoot jQuery selector
         *
         * @return {Integer} the number of checked options (checkboxes) in the specified group.
         */
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

        var getNumberOfSelectedLists = function() {
            return $(".inbox_inbox_check_list:checked").length;
           };

          var updateEditCopyDeleteButtonsStatus = function() {
              var num = getNumberOfSelectedLists();
              if(num === 0) {
                  $dynListsEditButton.attr('disabled', 'disabled');
                  $dynListsCopyButton.attr('disabled', 'disabled');
                  $dynListsDeleteButton.attr('disabled', 'disabled');
              } else if(num === 1){
                  $dynListsEditButton.removeAttr('disabled');
                  $dynListsCopyButton.removeAttr('disabled');
                  $dynListsDeleteButton.removeAttr('disabled');
              } else if(num > 1){
                  $dynListsEditButton.attr('disabled', 'disabled');
                  $dynListsCopyButton.attr('disabled', 'disabled');
                  $dynListsDeleteButton.removeAttr('disabled');
              }
          };

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		//////////////////////////////////////////////////////////////////////
        // Functions for loading dynamic lists data from a condition object //
        //////////////////////////////////////////////////////////////////////		

		/**
		 * Recursively traverses the condition object and builds an array of all used option IDs.
		 * 
		 * @param {Object} filterObj	A condition object
		 * @param {Array} idArray	An array to which option IDs will be appended
		 */
		var dumpOptionIDs = function(filterObj, idArray) {
			
			if(filterObj === null || idArray === null) {
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
				if($.type(arrayElement) === "string") {
					idArray.push(conditionArray[i]);
				} else {
					// assuming the object is a container
					dumpOptionIDs(arrayElement, idArray);
				}
			}
						
		};

	    /**
         * Resets the editing form and loads the list with specified id into it.
         * @param {String} id    The id of a list
         * @param {Boolean} copyMode    When set to true string "Copy of " is prepended to the name of the displayed list and description is cleared
         */
        var loadListIntoEditingForm = function(id, copyMode) {

            resetListEditingForm();

            if (!allLists.hasOwnProperty(id) || allLists[id] == null) {
                return;
            }
            var list = allLists[id];

            // Fill in input fields with list data
            if (copyMode) {
                $("#list_name").val("Copy of " + list["sakai:name"]);
            } else {
                $("#list_name").val(list["sakai:name"]);
                $("#description").val(list["sakai:description"]);
            }

            var idArray = [];
            var filterAsObject = $.parseJSON(list.filter);
            dumpOptionIDs(filterAsObject, idArray); // dumped IDs are stored in idArray
            var includeAllGradsId = $includeGradsCheckbox.val();
            var includeAllUndergradsId = $includeUndergradsCheckbox.val();


            for (var i = 0; i < idArray.length; i++) {
                var currentId = idArray[i];

                // cohort status - designate term - year
                if (currentId.match(/^designate_term_year_/)) {

                    var year = currentId.substring(20); // 20 is the length of 'designate_term_year_'
                    $("option[value='" + year + "']", $cohortStatus).attr("selected", "selected");

                    // we need to select 'Only include students in designated cohort(s)' option if we got here,
                    // because we can have the year option only if this option is selected
                    $("#cohort_status_specified_students", $cohortStatus).attr("checked", "checked");

                    continue;
                }


                switch (currentId) {
                    case includeAllGradsId:
                        enableGradsSection();
                        break;
                    case includeAllUndergradsId:
                        enableUndergradsSection();
                        break;
                    case "designate_term_semester_spring":
                    case "designate_term_semester_fall":
                        // fall-through is intentional here
                        $("option[value='" + currentId + "']", $sectionC).attr("selected", "selected");
                        break;
                    default:
                        $("input[value='" + currentId + "']").attr("checked", "checked");
                        break;
                }

            }

            if (isSomethingSelectedInUndergradsSection()) {
                enableUndergradsSection();
            }

            if (isSomethingSelectedInGradsSection()) {
                enableGradsSection();
            }

            // registration status radio buttons (section c)
            if (isSomethingSelectedInRegistrationStatusSection()) {
                $("#reg_status_only_designated_statuses", $sectionC).attr("checked", "checked");
            }


            // processing 'select all' checkboxes (section c)
            if (areAllOptionsInGroupSelected($(".reg_status .sub_group", $sectionC))) {
                $('#reg_status_select_all_in_group', $sectionC).attr("checked", "checked");
            }

            if (areAllOptionsInGroupSelected($(".current_or_not .sub_group", $sectionC))) {
                $('#currency_status_select_all_in_group', $sectionC).attr("checked", "checked");
            }

            if (areAllOptionsInGroupSelected($(".student_reg_status .sub_group", $sectionC))) {
                $('#student_reg_status_select_all_in_group', $sectionC).attr("checked", "checked");
            }


            // special programs radio buttons (section c)
            if (isSomethingSelectedInSpecialProgramsSection()) {
                $("#special_program_specified_students", $sectionC).attr("checked", "checked");
            }

            // Student status radio buttons (section c)
            if (isSomethingSelectedInStudentStatusSection()) {
                $("#student_status_specified_students", $sectionC).attr("checked", "checked");
            }

            // Residency status radio buttons (section c)
            if (isSomethingSelectedInResidencyStatusSection()) {
                $("#residency_status_specified_students", $sectionC).attr("checked", "checked");
            }

            updateNumberOfPeopleSelectedByFilter(list.filter);

        };

        var createEmptyRootNodeForDynamicLists = function() {

            // create dynamic lists node
            var props = {
                "sling:resourceType": "myberkeley/dynamicliststore"
            };

            $.ajax({
                        type: "POST",
                        url: dynamicListsBaseUrl,
                        async: false,
                        cache: false,
                        dataType:"json",
                        data: props,
                        error: function(xhr, textStatus, thrownError) {
                            sakai.api.Util.notification.show(errorText, "", sakai.api.Util.notification.type.ERROR);
                        }
                    });
        };


	    var loadDynamicListsFromServer = function() {
            sakai.api.Server.loadJSON(dynamicListsBaseUrl, function(success, data) {
                if (success) {
                    allLists = data;
                    displayLoadedLists();
                } else {
                    allLists = [];
                    createEmptyRootNodeForDynamicLists();
                    displayLoadedLists();
                }
                setTabState();
            });
        };




        //////////////////////////
        // Dynamic lists saving //
        //////////////////////////

         var buildFilterStringFromListEditingForm = function() {

			 // Sections A and B
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

	        return $.toJSON(dynamicListFilter);
	    };

        var getDataFromInput = function() {
            var result = {};

            result.context = "g-ced-students";
            result.listName = $.trim($("#list_name").val());
            if (result.listName === null || result.listName === "") {
                $("#invalid_name").show();
                return -1;
            }
            result.desc = $.trim($("#description").val());

            // Gathering the data on standing
            //TODO: check for errors
            /*if($("#undergrad:checked").val() === null && $("#grad:checked").val() === null) {
             $("#invalid_major").show();
             return -1;
             }*/

            result.filter = buildFilterStringFromListEditingForm();

            return result;
        };

        var generateId = function() {
            return DYNAMIC_LIST_PREFIX + sakai.data.me.user.userid + "-" + new Date().getTime();
        };

	    var saveList = function(data, listId) {
	        if (listAlreadyExists(data)) {
	            showGeneralMessage($("#inbox_generalmessages_already_exists").text());
	            return;
	        }

	        var id = listId; // list id used for saving
	        if(id === null){
	        	// creating a new list
	        	id = generateId();
	        }

	        var list = {
                  	"sling:resourceType": "myberkeley/dynamiclist",
	                "sakai:name": data.listName,
	                "sakai:description": data.desc,
                    context: data.context,
                    filter: data.filter
	              };

	        sakai.api.Server.saveJSON(dynamicListsBaseUrl + "/" + id, list, function() {
	        	$.bbq.pushState({},2);
	        	loadDynamicListsFromServer();
	        });
	    };


        ////////////////////////////
        // Dynamic lists deletion //
        ////////////////////////////

         /**
         * This will do a DELETE request to the specified paths and delete each list.
         * @param {String[]} paths The array of dynamic lists that you want to delete.
         */
	    var batchDeleteLists = function(paths) {
	    	var requests = [];
            $(paths).each(function(i,val) {
                var req = {
                    "url": val,
                    "method": "POST",
                    "parameters": {
                        ":operation": "delete"
                    }
                };
                requests.push(req);
            });
            $.ajax({
                url: sakai.config.URL.BATCH,
                traditional: true,
                type: "POST",
                data: {
                    requests: $.toJSON(requests)
                },
                success: function(data) {
                     //showGeneralMessage("Lists successfully deleted.");
                },
                error: function(xhr, textStatus, thrownError) {
                   showGeneralMessage("Error deleting lists.");
                }
            });
	    };


	    var deleteLists = function(listIds) {

	        var paths = []; // paths to nodes to delete
	        for (var i = 0, j = listIds.length; i < j; i++) {

	            var currentId = listIds[i];

	            if(allLists.hasOwnProperty(currentId)) {

	                $("#inbox_table_list_" + currentId).empty();
	                $("#inbox_table_list_" + currentId).remove();

	                // store path to delete
	                paths.push(dynamicListsBaseUrl + "/" + currentId);

	                // delete list from memory
	                delete allLists[currentId];
	            } else {
	                alert("Error: list \"" + currentId + "\" not found");
	            }
	        }


	        // batch delete nodes selected for deletion
	        batchDeleteLists(paths);
	    };


        /////////////////////////////
        // Dynamic lists comparing //
        /////////////////////////////

        /**
	     * Returns a value indicating whether the specified list object already exists.
	     *
	     * @param {Object} listToCheck The list to seek
	     * @return {Boolean} true if the value parameter occurs within the existing lists; otherwise, false
	     */
		var listAlreadyExists = function(listToCheck) {

			//TODO: rewrite!!!!!

			for (var key in allLists) {
				if(!allLists.hasOwnProperty(key)) {
					continue;
				}
				var existingList = allLists[key];
				var existingListObj = {
                    "context" : existingList.context,
                    "listName": existingList["sakai:name"],
                    "desc": existingList["sakai:description"],
                    "filter": existingList.filter
                };

				if(listEquals(existingListObj, listToCheck)) return true;
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
            //TODO: rewrite this method
        };

        ////////////////////
        // Click handlers //
        ////////////////////

        $dynListsCreateButton.click(function() {
	        $.bbq.pushState("new",2);
	    });
	  
	  
	  	$(".inbox_inbox_check_list").live("click", function(){
	  		updateEditCopyDeleteButtonsStatus();
	  	});
	  	  
	    // Button click events
	    $dynListsDeleteButton.live("click", function(){
	        var listId = [];
	        $(".inbox_inbox_check_list:checked").each(function(){	            
	            var id = $(this).val();
	            listId.push(id);
	        });
	        
	        $("#inbox_inbox_checkAll").removeAttr("checked");
	        tickMessages();
	
	        if (listId.length < 1) {
	            showGeneralMessage($("#inbox_generalmessages_none_selected").text());
	        } else {
	            deleteLists(listId);
	            loadDynamicListsFromServer();
	        }
	    });
	    
	    $dynListsCopyButton.live("click", function(){
	        var listIds = [];
	        $(".inbox_inbox_check_list:checked").each(function(){
	            var id = $(this).val();
	            listIds.push(id);
	        });
	        
	        if (listIds.length == 0) {
	        	return;
	        }
	        
	        $("#inbox_inbox_checkAll").removeAttr("checked");
	        tickMessages();
	
	        // Display new list
	        $.bbq.pushState({"copy": listIds[0]},2);	        
	        
	    });
	    
	    $dynListsEditButton.live("click", function(){
	        	        
	        var listIds = [];
	        $(".inbox_inbox_check_list:checked").each(function(){
	            var id = $(this).val();
	            listIds.push(id);
	        });
	        
	        if (listIds.length == 0) {
	        	return;
	        }
	        	        
	        // Display edit list
	        $.bbq.pushState({"edit": listIds[0]},2);
	    });

	    $(".editLink").live("click", function(evt){   

	        var id = evt.target.id;
	        	        
	        // Display edit list
	        $.bbq.pushState({"edit": id},2);
	    });
	    
	    $dynListsBackToNotifManagerButton.live("click", function(){
	        window.location = "/dev/inboxnotifier.html";
	    });
	    
	    $dynListsCancelEditingButton.live("click", function(){
	        //editExisting = false;
	        $.bbq.pushState({},2);
	    });
	    
	    $dynListsSaveButton.live("click", function(){
	        $("#invalid_name").hide();
	        $("#invalid_major").hide();

	        var data = getDataFromInput();
			if(data < 0) {
				return;
			}

			// In IE browser jQuery.trim() function doesn't work this way $('#selector').text().trim()
			// It should be called like this $.trim($('#selector').text())
			// See http://bit.ly/d8mDx2
	        
	        var state = $.bbq.getState();
	        
	        if(state.hasOwnProperty("edit") ) {
	       		saveList(data, state.edit);
	       	} else if(state.hasOwnProperty("new")) {
	        	saveList(data, null);
	        } else if(state.hasOwnProperty("copy")) {
	        	saveList(data, null);
	        }	        
	    });

        // Check all messages
	    $("#inbox_inbox_checkAll").change(function(){
	        tickMessages();
	    });











	    
	    

	    



	    
	    	    



	    ////////////////////////////////
        // Hashchange events handling //
        ////////////////////////////////

		 var setTabState = function(){
	        
	        var state = $.bbq.getState();
	        
	        if (state.hasOwnProperty("new")) {	
	        	resetListEditingForm();        	
	        	switchToEditMode();
	        } else if(state.hasOwnProperty("edit")) {	        	
				loadListIntoEditingForm(state.edit, false);
	        	switchToEditMode();
	        }  else if (state.hasOwnProperty("copy")) {	        	
	        	loadListIntoEditingForm(state.copy, true);
	        	switchToEditMode();
	        } else {
	        	switchToListMode();
	        }	        
	    };
		
		 $(window).bind('hashchange', function() {
	        setTabState();
	    });







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
	       
        	// Trimpath template for sections A and B of the list editing form (section C is static and doesn't require a templete)
        	var $listEditFormTemplate = $("#list_edit_form_template");
                	
        	// View to render the template for sections A and B
			var $view = $("#view");
	        
	        // Loading template
            var template;
			$.ajax({
                    url: "nauth_ced.json", // TODO: put this into sakai config
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
			
			boolTemplateHasUndergradsData = typeof(template.undergraduates) !== 'undefined' && template.undergraduates !== null;
			boolTemplateHasGradsData = typeof(template.graduates) !== 'undefined' && template.graduates !== null; 
			
			$view.html(sakai.api.Util.TemplateRenderer($listEditFormTemplate, template));
			
			$includeUndergradsCheckbox = $("#include_undergrads");
			$includeGradsCheckbox = $("#include_grads");
			
			// Define undergrad and grad groups AFTER template has been rendered
			$undergradsGroup = $(".undergrads_group");
			$gradsGroup = $(".grads_group");
			
			//Disabling all graduate and undergraduate controls
			if (boolTemplateHasUndergradsData && boolTemplateHasGradsData) {
														
				$includeGradsCheckbox.click(function(){
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
			
			$studentsTargetedByCurrentList = $(".students_targeted_by_list_container .readonly_textbox");
			
			
			// interactive number of users
			var $listEditingDiv = $("#create_new_list");
			$("input:checkbox, input:radio", $listEditingDiv).click(function() {
				var filterString = buildFilterStringFromListEditingForm();
				updateNumberOfPeopleSelectedByFilter(filterString);
			});
			$("select", $listEditingDiv).change(function() {
				var filterString = buildFilterStringFromListEditingForm();
				updateNumberOfPeopleSelectedByFilter(filterString);
			});
			
	  	  	//Show more/less button in section C
	  		$showMoreOrLess = $("#show_more_or_less");
			// section C toggle button
			$showMoreOrLess.click(toggleSectionC);

			
	        
	        // this is needed for the situation when we reload this page with #new
	        resetListEditingForm();
	        
	        dynamicListsBaseUrl = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists";

			loadDynamicListsFromServer();
	    };
	
	    doInit();
    };

    sakai.api.Widgets.Container.registerForLoad("listpage");
});
