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
/* global $, Config, jQuery, sakai, sdata */

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core", "/dev/lib/myb/myb.dynlist.logic.js", "/dev/javascript/myb/myb.securepage.js"], function($, sakai, myb, Condition) {
    /**
     * @name sakai_global.dynamiclisteditor
     *
     * @class dynamiclisteditor
     *
     * @description
     * Dynamic list editor widget
     *
     * @version 0.0.1
     * @param {String} tuid Unique id of the widget
     * @param {Boolean} showSettings Show the settings of the widget or not
     */
    sakai_global.dynamiclisteditor = function(tuid, showSettings) {

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////

        /**
         * Hashtable of all the lists
         */
        //var allLists = {};

        /**
         * A list being edited (or null for new lists)
         */
        var currentList = null;


        /**
         * Current list's Id (or null for new lists)
         */
        var currentListId = null;

        /**
         * Whether the loaded trimpath template includes undergraduate students data
         */
        var hasUndergradsData = false;

        /**
         * Whether the loaded trimpath template includes graduate students data
         */
        var hasGradsData = false;

        /**
         * Dynamic lists base URL (parent node in Sparse), delete this node to remove all dynamic lists for current user
         */
        var dynamicListsBaseUrl;
        
        /**
         * The dynamic list context which is in effect for this editing session.
         */
        var dynamicListContext;
        var dynamicListContextUrl;
        var dynamicListContextAllowed;

        /**
         * Needed to prevent unnecessary people counting requests when editing a new list
         */
        var lastUsedCriteriaString = "";

        /**
         * Number of students targeted by the last criteria string
          */
        var lastNumberOfTargetedStudents = 0;

        var validatorObj = null;
        var validatorObjTemplate = null;

        /**
         * This parameter is passed to jQuery validate plugin
         */
        var debugValidation = true;

        /**
         * The overlay transparency as a percentage. If 0 the overlay is disabled, and the page will remain interactive.
         * If 100 the overlay will be 100% opaque.
         */
        var dialogOverlayTransparency = 20;


        //////////////////////
        // jQuery selectors //
        //////////////////////

        /**
         * Widget's root element
         */
        var $rootElement = $("#" + tuid);

        var $formElement = $("#dyn-list-form", $rootElement); // Used for validation

        // View to render the template for sections A and B
        var $view = $("#view", $rootElement);

        /**
         * Section C wrapper DIV
         */
        var $sectionC = $("#section_c", $rootElement);

        /**
         * Section C cohort status wrapper DIV
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
         * Dynamic list 'Save' button
         */
        var $dynListsSaveButton = $("#dyn_lists_save_button", $rootElement);

        /**
         * Dynamic list 'Cancel' button (cancels editing and switches withe my to list mode)
         */
        var $dynListsCancelEditingButton = $("#dyn_lists_cancel_button", $rootElement);

        /**
         * Show more/less button in section C (template must be loaded before using this variable)
         */
        var $showMoreOrLess;

        /**
         * HTML div to diplay the number of users targeted by the current list
         */
        var $studentsTargetedByCurrentList = $("#dynamiclisteditor_number_of_students", $rootElement);

        /**
         * College name to display in the template
         */
        var $college = $("#dynamiclisteditor_college", $rootElement);


        var $cohortStatusSpecifiedStudents = $("#cohort_status_specified_students", $cohortStatus);
        var $designateTermSemester = $("#designate_term_semester", $cohortStatus);
        var $regStatusOnlyDesignatedStatuses = $("#reg_status_only_designated_statuses", $sectionC);
        var $regStatusIncludeAll = $("#reg_status_include_all", $sectionC);
        var $cohortStatusAllStudents = $("#cohort_status_all_students", $sectionC);
        var $cohortStatusTermBefore = $("#cohort_status_term_before", $sectionC);
        var $specialProgramAllStudents = $("#special_program_all_students", $sectionC);
        var $studentStatusAllStudents = $("#student_status_all_students", $sectionC);
        var $residencyStatusAllStudents = $("#residency_status_all_students", $sectionC);
        var $listName = $("#list_name", $rootElement);
        var $description = $("#description", $rootElement);
        var $regStatusSelectAllInGroup = $('#reg_status_select_all_in_group', $sectionC);
        var $currencyStatusSelectAllInGroup = $('#currency_status_select_all_in_group', $sectionC);
        var $studentRegStatusSelectAllInGroup = $('#student_reg_status_select_all_in_group', $sectionC);
        var $regStatusSubGroup = $(".reg_status .sub_group", $sectionC);
        var $currentOrNotSubGroup  = $(".current_or_not .sub_group", $sectionC);
        var $studentRegStatusSubGroup = $(".student_reg_status .sub_group", $sectionC);
        var $specialProgramSpecifiedStudents = $("#special_program_specified_students", $sectionC);
        var $specialPrograms = $(".special_programs", $sectionC);

        var $noStudentsWarningDialogSaveButton = $("#dialog-save-button", $rootElement);
        var $noStudentsEverWarningDialogSaveButton = $("#dialog-save-button-for-no-students-ever", $rootElement);

        /**
          * For dialog-overlay to remind user to save their draft.
          * (When user clicks on 'Create New DyNamic List' button.)
          */
         var $noStudentsWarningDialog = $("#no_students_warning_dialog", $rootElement).jqm({
             modal: true,
             overlay: dialogOverlayTransparency,
             toTop: true,
             onShow: null
         });

        /**
          * For dialog-overlay to remind user to save their draft.
          * (When user clicks on 'Create New DyNamic List' button.)
          */
         var $noStudentsEverWarningDialog = $("#no_students_ever_warning_dialog", $rootElement).jqm({
             modal: true,
             overlay: dialogOverlayTransparency,
             toTop: true,
             onShow: null
         });


        ////////////////////////////////////////////////////////////////////////
        // Functions for gathering the information about the selected options //
        ////////////////////////////////////////////////////////////////////////

        /**
         * Gathers all selected options in the selected element group and returns them as an OR condition object.
         *
         * @param $allItemsOption	an option that represents all items, can be null if there is no such option
         * @param $rootGroup	element group in which to search for selected options
         *
         * @return {Object} an OR condition object containing all selected options in the selected element group.
         */
        var buildSelectedOptionsObjectAsOR = function($allItemsOption, $rootGroup) {

            var selectedOptions = new Condition();
            selectedOptions.OR = [];

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
         * Gathers information about the cohort status and returns it as an object.
         * Returned information includes: semester, year and cohort.
         *
         * @return {Object} a condition object containing the information about the cohort status in the AND field. Return value can be null if nothing is selected.
         */
        var buildCohortStatusObject = function() {

            var result = new Condition();

            if ($cohortStatusSpecifiedStudents.is(':checked')) {
                var semester = $designateTermSemester.val();
                var year = $designateTermYear.val();
                var cohort = $("input[name=cohort_status_terms]:checked", $cohortStatus).val();

                result.AND = [semester, "designate_term_year_" + year, cohort];
            }

            return result;
        };

        /**
         * Gathers information about the registration status and returns it as an object.
         *
         * @return {Object} a condition object containing the information about the registration status.
         */
        var buildRegistrationStatusObject = function() {

            var registrationStatus = new Condition();

            if($regStatusOnlyDesignatedStatuses.is(':checked')) {

                var selectedRegisteredOR = buildSelectedOptionsObjectAsOR(null, $regStatusSubGroup);
                var selectedCurrencyOR = buildSelectedOptionsObjectAsOR(null, $currentOrNotSubGroup);
                var selectedWithdrawnOR = buildSelectedOptionsObjectAsOR(null, $studentRegStatusSubGroup);

                registrationStatus = registrationStatus.joinTwoConditionsByAND(selectedRegisteredOR);
                registrationStatus = registrationStatus.joinTwoConditionsByAND(selectedCurrencyOR);
                registrationStatus = registrationStatus.joinTwoConditionsByAND(selectedWithdrawnOR);
            }

            return registrationStatus;
        };

        /**
         * Gathers all undergraduates related information and returns it as an object.
         * Returned information includes: undergraduate majors, levels and 'admitted as' status.
         *
         * @return {Object} a condition object containing all undergraduates related information in the AND field.
         */
        var buildUndergradsObjectAsAND = function() {

            var undergrads = new Condition();
            var undergradsFilter = new Condition();

            var selectedUndergradMajorsOR = buildSelectedOptionsObjectAsOR(null, $(".majors"));
            var selectedLevelsOR = buildSelectedOptionsObjectAsOR(null, $(".levels"));
            var selectedAdmittedAsOR = buildSelectedOptionsObjectAsOR(null, $(".admittedAs"));

            undergrads = selectedUndergradMajorsOR;
            if(undergrads.isConditionObjectEmpty()) {
                undergrads.OR =[$includeUndergradsCheckbox.val()];
            }
            
            undergradsFilter = selectedLevelsOR;
            undergradsFilter = undergradsFilter.joinTwoConditionsByAND(selectedAdmittedAsOR);
            
            undergrads.joinFilterToCondition(undergradsFilter);

            return undergrads;
        };

        /**
         * Gathers all graduate students related information and returns it as an object.
         * Returned information includes: graduate programs, degrees.
         *
         * @return {Object} a condition object containing all graduate students related information in the AND field.
         */
        var buildGradsObjectAsAND = function() {

            var grads = new Condition();
            var gradsFilter = new Condition();

            var selectedGradProgramsOR = buildSelectedOptionsObjectAsOR(null, $(".programs", $rootElement));
            var selectedDegreesOR = buildSelectedOptionsObjectAsOR(null, $(".degrees", $rootElement));

            grads = grads.joinTwoConditionsByAND(selectedGradProgramsOR);
            if(grads.isConditionObjectEmpty()) {
                grads.OR = [$includeGradsCheckbox.val()];
            }
            gradsFilter = gradsFilter.joinTwoConditionsByAND(selectedDegreesOR);
            grads.joinFilterToCondition(gradsFilter);

            return grads;
        };


        /**
         * Gathers all selected options for both undergraduate and graduate students (if available).
         * Returns these options as an appropriate condition object.
         *
         * @return {Object} a condition object containing all selected options for undergraduate and graduate students.
         */
        var buildUndergraduatesAndGraduatesResultingObject = function (){

            var includeUndergrads = false;
            var includeGrads = false;

            if(hasUndergradsData && hasGradsData) {
                // If the both checkboxes are available, need to check their statuses
                includeUndergrads = $includeUndergradsCheckbox.length > 0 && $includeUndergradsCheckbox.is(":checked");
                includeGrads = $includeGradsCheckbox.length > 0 && $includeGradsCheckbox.is(":checked");

            } else {
                if(hasUndergradsData) {
                    // Only undergrads are available, no checkbox
                    includeUndergrads = true;
                } else if(hasGradsData) {
                    // Only grads are available, no checkbox
                    includeGrads = true;
                }
            }


            // For undergrads
            var undergrads = new Condition();

            if (includeUndergrads) {
                undergrads = buildUndergradsObjectAsAND();
            }


            // For grads
            var grads = new Condition();

            if (includeGrads) {
                grads = buildGradsObjectAsAND();
            }

            var result = new Condition();

            if (includeUndergrads && includeGrads) {

                result = undergrads.joinTwoConditionsByOR(grads);

            } else if (includeUndergrads) {

                result = undergrads;

            } else if (includeGrads) {

                result = grads;
            }

            return result;
        };

        //////////////////////////
        // UI related functions //
        //////////////////////////

        /**
         * Get the internationalised value for a specific key.
         * @param {String} key The key which you want to be translated
         */
        var translate = function(key) {
            return sakai.api.i18n.Widgets.getValueForKey("dynamiclisteditor", "default", key);
        };

        /**
         * used to toggle or set checkbox sets
         * @param {jQuery Object} $masterCheck the master checkbox
         * @param {jQuery Object} $subGroup the group of subcheckbox boxes
         * @param {bool} setActive enable or disable the set, if undefined set to the checked state of $masterCheck
         * @param {bool} setMaster set the checkedState of $masterCheck to setActive state
         */
        
        var toggleControlSet = function ($masterCheck, $subGroup, setActive, setMaster) {
            setActive = setActive || $masterCheck.is(':checked');
            if (setMaster) {
                if (setActive) {
                    $masterCheck.attr("checked", "checked");
                } else {
                    $masterCheck.removeAttr("checked");
                }
            }
            if (setActive) {
                $("input", $subGroup).removeAttr("disabled");
                $subGroup.removeClass("disabled");
            } else {
                $("input", $subGroup).attr("disabled", "disabled");
                $subGroup.addClass("disabled");
            }
        };

        /**
         * Enables all graduate students section of the form programmatically.
         * Must be called AFTER loading template.
         */
        var enableGradsSection = function() {
            toggleControlSet($includeGradsCheckbox, $gradsGroup, true, true);
        };

        /**
         * Enables all undergraduate students section of the form programmatically.
         * Must be called AFTER loading template.
         */
        var enableUndergradsSection = function() {
            toggleControlSet($includeUndergradsCheckbox, $undergradsGroup, true, true);
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
                // escaping < and > with \x3C and \x3E
                if (i === curr_year) {
                    yearsArr.push("\x3Coption value='" + i + "' selected='selected'\x3E" + i + "\x3C/option\x3E");
                } else {
                    yearsArr.push("\x3Coption value='" + i + "'\x3E" + i + "\x3C/option\x3E");
                }
            }
            $designateTermYear.append(yearsArr.join(''));
        };

        /**
         * Hides section C (common criteria settings).
         */
        var hideSectionC = function() {
            $showMoreOrLess.text("Show More");
            $sectionC.hide();
        };

        /**
         * Shows section C (common criteria settings).
         */
        var showSectionC = function() {
            $showMoreOrLess.text("Show Less");
            $sectionC.show();
        };

        /**
         * Toggles section C visibility.
         */
        var toggleSectionC = function () {

            if($sectionC.is(":visible")) {
                hideSectionC();
            } else {
                showSectionC();
            }
        };


        /**
         * Resets the editing form UI (checkboxes and CSS styles)
         * Must be called AFTER loading template.
         */
        var initListEditingForm = function() {


            lastUsedCriteriaString = "";
            lastNumberOfTargetedStudents = 0;


            hideSectionC();

            // reset all checkboxes
            $("input:checkbox:checked", $("#create_new_list")).removeAttr("checked");

            if(hasUndergradsData && hasGradsData) {
                $undergradsGroup.addClass("disabled");
                $gradsGroup.addClass("disabled");

                $("input", $gradsGroup).attr("disabled", "disabled");
                $("input", $undergradsGroup).attr("disabled", "disabled");

            } else if(hasUndergradsData) {
                $("input", $undergradsGroup ).removeAttr("disabled");
                $undergradsGroup.removeClass("disabled");
            } else if(hasGradsData) {
                $("input", $gradsGroup).removeAttr("disabled");
                $gradsGroup.removeClass("disabled");
            }


            // section c
            $regStatusIncludeAll.attr("checked", "checked");
            $cohortStatusAllStudents.attr("checked", "checked");
            $cohortStatusTermBefore.attr("checked", "checked");

            $("#designate_term_semester option:first", $sectionC).attr("selected", "selected");
            $("#designate_term_year option:last", $sectionC).attr("selected", "selected");


            $specialProgramAllStudents.attr("checked", "checked");
            $studentStatusAllStudents.attr("checked", "checked");
            $residencyStatusAllStudents.attr("checked", "checked");

        };

        /**
         * Shows a general message on the top screen.
         * @param {String} msg The message you want to display.
         * @param {Boolean} isError True for error (red block) or false for normal message(green block).
         * @param {String} title Message title to display.
         */
        var showGeneralMessage = function(msg, isError, title) {
            // Check whether to show an error type message or an information one.
            var type = isError ? sakai.api.Util.notification.type.ERROR : sakai.api.Util.notification.type.INFORMATION;

            // Show the message to the user.
            sakai.api.Util.notification.show(title? title : "", msg, type);
        };

        /**
         * Updates the number of people selected by criteria string.
         *
         * @param {String} criteriaString	a condition object represented as a string
         *
         */
        var updateNumberOfPeopleSelectedByCriteria = function(criteriaString) {

            // Prevent unnecessary AJAX requests
            if(lastUsedCriteriaString === criteriaString) {
                 return;
            }

            lastUsedCriteriaString = criteriaString;
            lastNumberOfTargetedStudents = 0;

            $.ajax({
                url: dynamicListContextUrl + ".json",
                traditional: true,
                type: "POST",
                data: {
                  criteria: criteriaString
                },
                success: function(data) {
                  if(data.count > 0) {
                    $studentsTargetedByCurrentList.removeClass("noStudentsTargeted");
                  } else {
                    $studentsTargetedByCurrentList.addClass("noStudentsTargeted");
                  }
                  lastNumberOfTargetedStudents = data.count;

                  $studentsTargetedByCurrentList.text(lastNumberOfTargetedStudents);
                },
                error: function() {
                  $studentsTargetedByCurrentList.addClass("noStudentsTargeted").text("N/A");
                }
          });

        };

        /**
         * Switches to edit dynamic list mode
         */
        var switchToEditMode = function() {

            $("#create_new_list").show();


            // Show/hide appropriate buttons
            $dynListsCancelEditingButton.show();
            $dynListsSaveButton.removeClass("disabled");
            $dynListsSaveButton.show();
        };



        /**
         * Return the number of options (checkboxes) in a group.
         *
         * @param {jQuery} $groupRoot jQuery selector
         *
         * @return {Number} the number of options (checkboxes) in the specified group.
         */
        var getNumberOfOptionsInGroup = function($groupRoot) {
             return $("input:checkbox", $groupRoot).length;
        };

        /**
         * Return the number of checked options (checkboxes) in a group.
         *
         * @param {jQuery} $groupRoot jQuery selector
         *
         * @return {Number} the number of checked options (checkboxes) in the specified group.
         */
        var getNumberOfSelectedOptionsInGroup = function($groupRoot) {
             return $("input:checkbox:checked", $groupRoot).length;
        };

        /**
         * Checks if there are selected options in the undergraduates section in section B.
         *
         * @return {Boolean} true if there are selected options in the undergraduates section in section B; otherwise false.
         */
        var isSomethingSelectedInUndergradsSection = function () {
            return getNumberOfSelectedOptionsInGroup($undergradsGroup) > 0;
        };

        /**
         * Checks if there are selected options in the graduates section in section B.
         *
         * @return {Boolean} true if there are selected options in the graduates section in section B; otherwise false.
         */
        var isSomethingSelectedInGradsSection = function () {
            return getNumberOfSelectedOptionsInGroup($gradsGroup) > 0;
        };

        /**
         * Checks if there are selected options in the registration status section in section C.
         *
         * @return {Boolean} true if there are selected options in the registration status section in section C; otherwise false.
         */
        var isSomethingSelectedInRegistrationStatusSection = function () {
            var totalOptionsSelected = getNumberOfSelectedOptionsInGroup($regStatusSubGroup) +
            getNumberOfSelectedOptionsInGroup($currentOrNotSubGroup) +
            getNumberOfSelectedOptionsInGroup($studentRegStatusSubGroup);

            return totalOptionsSelected	> 0;
        };

        /**
         * Checks if there are selected options in the special programs section in section C.
         *
         * @return {Boolean} true if there are selected options in the special programs section in section C; otherwise false.
         */
        var isSomethingSelectedInSpecialProgramsSection = function () {
            return getNumberOfSelectedOptionsInGroup($(".special_programs .sub_group", $sectionC)) > 0;
        };

        /**
         * Checks if there are selected options in the student status section in section C.
         *
         * @return {Boolean} true if there are selected options in the student status section in section C; otherwise false.
         */
        var isSomethingSelectedInStudentStatusSection = function () {
            return getNumberOfSelectedOptionsInGroup($(".student_and_residency_status_col_left .sub_group", $sectionC)) > 0;
        };

        /**
         * Checks if there are selected options in the residency status section in section C.
         *
         * @return {Boolean} true if there are selected options in the residency status section in section C; otherwise false.
         */
        var isSomethingSelectedInResidencyStatusSection = function () {
            return getNumberOfSelectedOptionsInGroup($(".student_and_residency_status_col_right .sub_group", $sectionC)) > 0;
        };

        /**
         * Checks if all options in the given group are selected
         *
         * @param $groupRoot {jQuery} group (jQuery selector) to check for selected options
         *
         * @return  {Boolean} true if all options in the given group are selected; otherwise false.
         */
        var areAllOptionsInGroupSelected = function ($groupRoot) {
             var numberOfOptions = getNumberOfOptionsInGroup($groupRoot);
             var numberOfSelectedOptions = getNumberOfSelectedOptionsInGroup($groupRoot);

             return numberOfSelectedOptions > 0 && numberOfOptions === numberOfSelectedOptions;
        };

        //////////////////////////////////////////////////////////////////////
        // Functions for loading dynamic lists data from a condition object //
        //////////////////////////////////////////////////////////////////////

        /**
         * Recursively traverses the condition object and builds an array of all used option IDs.
         *
         * @param {Object} criteriaObj	A condition object
         * @param {Array} idArray	An array to which option IDs will be appended
         */
        var dumpOptionIDs = function(criteriaObj, idArray) {

            if(criteriaObj === null || idArray === null) {
                return;
            }

            var conditionArray;
            // An object must have either AND or OR property, but not both at the same time,
            // FILTER property is optional
            if (criteriaObj.hasOwnProperty("AND")) {
                conditionArray = criteriaObj.AND;
            } else if (criteriaObj.hasOwnProperty("OR")) {
                conditionArray = criteriaObj.OR;
            } else {
                // empty object, assume that FILTER cannot exist without corresponding AND or OR properties
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

            // Check if we have a FILTER node
            if (criteriaObj.hasOwnProperty("FILTER")) {
                dumpOptionIDs(criteriaObj.FILTER, idArray);
            }

        };

        /**
         * Returns true is something selected in section C.
         *
         * @param idArray an array of criteria IDs.
         *
         * @return {Boolean} Returns true if something is selected in section C; otherwise returns false.
         */
        var isSomethingSelectedInSectionC = function(idArray) {

            if(idArray.length === 0) return false;

            // list of regular expressions to match condition IDs from section C
            var reg_status = /^reg_status_/;
            var currency_status = /^currency_status_/;
            var student_reg_status = /^student_reg_status_/;
            var designate_term_semester = /^designate_term_semester_/;
            var designate_term_year = /^designate_term_year_/;
            var cohort_status_term = /^cohort_status_term_/;
            var special_program = /^special_program_/;
            var student_status = /^student_status_/;
            var residency_status = /^residency_status_/;

            for (var i = 0; i < idArray.length; i++) {
                var currentId = idArray[i];
                if (    currentId.match(reg_status) ||
                        currentId.match(currency_status) ||
                        currentId.match(student_reg_status) ||
                        currentId.match(designate_term_semester) ||
                        currentId.match(designate_term_year) ||
                        currentId.match(cohort_status_term) ||
                        currentId.match(special_program) ||
                        currentId.match(student_status) ||
                        currentId.match(residency_status)
                        ) {
                    return true;

                }
            }

            return false;
        };

        /**
         * Created empty root node for dynamic lists (needed to set the correct resource type for this node,
         * otherwise things like counting the number of users targeted by dynamic list will not work).
         *
         * @param {Object} successCallback (optional) Function to call if successful post; usually redirect function.
         */
        var createEmptyRootNodeForDynamicLists = function(successCallback) {

            // create dynamic lists node
            var props = {
                "sling:resourceType": "myberkeley/dynamicliststore"
            };

            $.ajax({
                        type: "POST",
                        url: dynamicListsBaseUrl,
                        async: true,
                        cache: false,
                        dataType:"json",
                        data: props,
                        success: function(){
                            // If a callback function is specified in argument, call it.
                            if ($.isFunction(successCallback)) {
                                successCallback();
                            }
                        },
                        error: function(xhr, textStatus, thrownError) {
                            showGeneralMessage(thrownError, true);
                        }
                    });
        };


        var onLoadList = function(copyMode) {

            // Fill in input fields with list data
            if (copyMode) {
                $listName.val("Copy of " + currentList["sakai:name"]);
            } else {
                $listName.val(currentList["sakai:name"]);
                $description.val(currentList["sakai:description"]);
            }

            loadTemplate(currentList.context);

        };

        var resetForm = function() {

            currentList = null;
            currentListId = null;

            clearInvalids();

            $listName.val("");
            $description.val("");

            $college.text("N/A");
            $studentsTargetedByCurrentList.addClass("noStudentsTargeted").text("0");

            //Removing old template
            $view.empty();
        };

        /**
         * Resets the editing form and loads the list with specified id into it.
         * @param {String} listId    The id of a list
         * @param {Boolean} copyMode    When set to true string "Copy of " is prepended to the name of the displayed list and description is cleared
         */
        var loadList = function(listId, copyMode) {

           resetForm();

            var url = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists/" + listId + ".1.json";

            $.ajax({
                url: url,
                cache: false,
                async: true,
                success: function(data){
                    currentListId = listId;
                    currentList = data;
                    currentList.criteria = $.parseJSON(data.criteria);
                    onLoadList(copyMode);
                },
                error: function(){
                    showGeneralMessage(translate("THE_LIST_COULD_NOT_BE_LOADED"), true);
                }
            });
        };


        //////////////////////////
        // Dynamic lists saving //
        //////////////////////////

        /**
         * Gathers all information from sections A, B and C.
         * Returns a condition object created from this data as string.
         *
         *  @return {Object} a condition object created from sections A, B, C data.
         */
        var buildCriteriaFromListEditingForm = function() {

            // Sections A and B
            var dynamicListCriteria = buildUndergraduatesAndGraduatesResultingObject();

            // Section C
            // TODO THE BELOW IS NOT YET ENABLED AND WILL NOT WORK AS CODED.

            var registrationStatus = buildRegistrationStatusObject();
            dynamicListCriteria = dynamicListCriteria.joinTwoConditionsByAND(registrationStatus);


            var cohortStatus = buildCohortStatusObject();
            dynamicListCriteria = dynamicListCriteria.joinTwoConditionsByAND(cohortStatus);


            var specialPrograms = buildSelectedOptionsObjectAsOR($specialProgramAllStudents, $specialPrograms);
            dynamicListCriteria = dynamicListCriteria.joinTwoConditionsByAND(specialPrograms);

            var studentStatus = buildSelectedOptionsObjectAsOR($studentStatusAllStudents, $(".student_and_residency_status_col_left .sub_group", $sectionC));
            dynamicListCriteria = dynamicListCriteria.joinTwoConditionsByAND(studentStatus);

            var residencyStatus = buildSelectedOptionsObjectAsOR($residencyStatusAllStudents, $(".student_and_residency_status_col_right .sub_group", $sectionC));
            dynamicListCriteria = dynamicListCriteria.joinTwoConditionsByAND(residencyStatus);

            return dynamicListCriteria;
        };

        /**
         * Gathers all information from sections A, B and C.
         * Returns a condition object created from this data as string.
         *
         *  @return {String} a condition object created from sections A, B, C data as string.
         */
         var buildCriteriaStringFromListEditingForm = function() {
            return $.toJSON(buildCriteriaFromListEditingForm());
        };

        var validateUserInput = function() {
            // single ampersand is used for full-circuit evaluation
            return validatorObj.form() & validatorObjTemplate.form();
        };

        var getDataFromInput = function() {
            var result = {};

            result.context = dynamicListContext.name;
            result.listName = $.trim($listName.val());
            result.desc = $.trim($description.val());

            result.criteria = buildCriteriaStringFromListEditingForm();

            return result;
        };

        /**
         * Generates a unique ID for a new dynamic list
         *
         * @returns {String} generated list ID. For ex. "dl-892685-1305835896429"
         */
        var generateId = function() {
            return "dl-" + sakai.data.me.user.userid + "-" + new Date().getTime();
        };

        /**
         * Saves the given list.
         *
         * @param data  List object
         * @param listId    Existing list ID if you want to overwrite it or null for a new list.
         */
        var saveList = function(data, listId) {
            if (listAlreadyExists(data)) {
                showGeneralMessage($("#list_generalmessages_already_exists").text(), true);
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
                    criteria: data.criteria
                  };


            var url = dynamicListsBaseUrl + "/" + id;

            // First try to create the root node with the correct resource type,
            // if the node was created successfully try to save the list
            createEmptyRootNodeForDynamicLists(function() {
                    sakai.api.Server.saveJSON(url, list, function() {
                    //$.bbq.removeState(["new","cp","edit","context"]);
                    $.bbq.pushState({l: "dynlists", saved: id}, 2);
                });
            });
        };

        /**
         * Gathers all data from current form and calls the save list function with the appropriate arguments.
         */
        var getDataAndSaveList = function() {

            $dynListsSaveButton.addClass("disabled");

            var data = getDataFromInput();

            var state = $.bbq.getState();

            if(state.hasOwnProperty("edit") ) {
                saveList(data, state.edit);
            } else if(state.hasOwnProperty("new")) {
                saveList(data, null);
            } else if(state.hasOwnProperty("cp")) {
                saveList(data, null);
            }

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

            //TODO: rewrite this feature
            // I think duplicate checks should be done on the server-side without loading all available lists to the client

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

        $dynListsCancelEditingButton.click(function(){
            $.bbq.removeState(["new","cp","edit","context"]);
        });


        // Event handler for when user clicks on warning dialog's "Save" button.
        $noStudentsWarningDialogSaveButton.click(function() {
            $noStudentsWarningDialog.jqmHide();
            getDataAndSaveList();
        });

        // Event handler for when user clicks on 'no students ever' warning dialog's "Save" button.
        $noStudentsEverWarningDialogSaveButton.click(function() {
            $noStudentsEverWarningDialog.jqmHide();
            getDataAndSaveList();
        });

        $dynListsSaveButton.click(function(){

            if(!validateUserInput()) {
                showGeneralMessage(translate("PLEASE_CORRECT_THE_FIELDS_AS_SPECIFIED_BY_THE_RED_ERROR_TEXT"), true, translate("ERROR_LIST_CANNOT_BE_CREATED"));
                return;
            }

            if ($dynListsSaveButton.is(".disabled")) {
                // prevent double-clicking of save button
                return;
            }

            if (lastNumberOfTargetedStudents > 0) {
                getDataAndSaveList();
            } else if(lastUsedCriteriaString.length === 0 || lastUsedCriteriaString === "{}"){
                $noStudentsEverWarningDialog.jqmShow();
            } else {
                $noStudentsWarningDialog.jqmShow();
            }
        });

        /**
         * Defines variables and sets up event handlers for template-dependent page elements
         * (i.e. elements that do not exist before template loading).
         */
        var setupTemplateDependentVarsAndEventHandlers = function() {

            $includeUndergradsCheckbox = $("#include_undergrads", $rootElement);
            $includeGradsCheckbox = $("#include_grads", $rootElement);

            // Define undergrad and grad groups AFTER template has been rendered
            $undergradsGroup = $(".undergrads_group", $rootElement);
            $gradsGroup = $(".grads_group", $rootElement);

            //Disabling all graduate and undergraduate controls
            if (hasUndergradsData && hasGradsData) {

                $includeGradsCheckbox.click(function(){
                    toggleControlSet($includeGradsCheckbox, $gradsGroup);
                    clearInvalidsInSection($gradsGroup);
                });

                $includeUndergradsCheckbox.click(function(){
                    toggleControlSet($includeUndergradsCheckbox, $undergradsGroup);
                    clearInvalidsInSection($undergradsGroup);
                });
            }

            $regStatusSelectAllInGroup.click(function(){
                if($regStatusSelectAllInGroup.is(':checked')) {
                    $(".reg_status .sub_group input", $sectionC).attr("checked", "checked");
                } else {
                    $(".reg_status .sub_group input", $sectionC).removeAttr("checked");
                }
            });

            $currencyStatusSelectAllInGroup.click(function(){
                if($currencyStatusSelectAllInGroup.is(':checked')) {
                    $(".current_or_not .sub_group input", $sectionC).attr("checked", "checked");
                } else {
                    $(".current_or_not .sub_group input", $sectionC).removeAttr("checked");
                }
            });

            $studentRegStatusSelectAllInGroup.click(function(){
                if($studentRegStatusSelectAllInGroup.is(':checked')) {
                    $(".student_reg_status .sub_group input", $sectionC).attr("checked", "checked");
                } else {
                    $(".student_reg_status .sub_group input", $sectionC).removeAttr("checked");
                }
            });

            // interactive number of users
            var $listEditingDiv = $("#create_new_list");
            $("input:checkbox, input:radio", $listEditingDiv).click(function() {
                var criteriaString = buildCriteriaStringFromListEditingForm();
                updateNumberOfPeopleSelectedByCriteria(criteriaString);

                // Removing validation errors if nothing was selected in undergrads and grads sections
                // This is just a UI tweak
                clearValidationErrorsInDisabledTemplateSections();

            });
            $("select", $listEditingDiv).change(function() {
                var criteriaString = buildCriteriaStringFromListEditingForm();
                updateNumberOfPeopleSelectedByCriteria(criteriaString);
            });

            // Apply validation rules to section B
            var $formElementTemplate = $("#frm_main_dynamic", $rootElement); // Used for validation
            validatorObjTemplate = setupValidationForTemplate($formElementTemplate);


            //Show more/less button in section C
            $showMoreOrLess = $("#show_more_or_less", $rootElement);
            // section C toggle button
            $showMoreOrLess.click(toggleSectionC);

        };
        
        var filterTemplateDataByContext = function(data) {
            if (propExists(data.undergraduates)) {
                if (propExists(data.undergraduates.id) && !dynamicListContextAllowed.test(data.undergraduates.id)) {
                    delete data.undergraduates.id;
                }
                if (propExists(data.undergraduates.majors)) {
                    filterItemsArrayByContext(data.undergraduates, "majors");
                }
                if (!propExists(data.undergraduates.id) && !propExists(data.undergraduates.majors)) {
                    delete data.undergraduates;
                }
            }
            if (propExists(data.graduates)) {
                if (propExists(data.graduates.id) && !dynamicListContextAllowed.test(data.graduates.id)) {
                    delete data.graduates.id;
                }
                if (propExists(data.graduates.programs)) {
                    filterItemsArrayByContext(data.graduates, "programs");
                }
                if (!propExists(data.graduates.id) && !propExists(data.graduates.programs)) {
                    delete data.graduates;
                }
            }
        };
        
        var filterItemsArrayByContext = function(data, arrayHolder) {
          var newItemArray = [];
          for (var i = 0; i < data[arrayHolder].items_array.length; i++) {
              var item = data[arrayHolder].items_array[i];
              if (propExists(item.id) && dynamicListContextAllowed.test(item.id)) {
                  newItemArray.push(item);
              }
          }
          if (newItemArray.length > 0) {
              data[arrayHolder].items_array = newItemArray;
          } else {
              delete data[arrayHolder];
          }
        };

        ////////////////////
        // Page templates //
        ////////////////////

        /**
         * This script is used for dynamic generation of sections A and B
         * @param prop a property to check
         *
         * @return true if property exists and is not null; otherwise false.
         */
        var propExists = function(prop) {
     			return typeof(prop) !== 'undefined' && prop !== null;
    	};


        var loadDataIntoTemplate = function () {
          var idArray = [];
            dumpOptionIDs(currentList.criteria, idArray); // dumped IDs are stored in idArray

            var includeAllGradsId = $includeGradsCheckbox.val();
            var includeAllUndergradsId = $includeUndergradsCheckbox.val();


            for (var i = 0; i < idArray.length; i++) {
                var currentArrayId = idArray[i];

                // cohort status - designate term - year
                if (currentArrayId.match(/^designate_term_year_/)) {

                    var year = currentArrayId.substring(20); // 20 is the length of 'designate_term_year_'
                    $("option[value='" + year + "']", $cohortStatus).attr("selected", "selected");

                    // we need to select 'Only include students in designated cohort(s)' option if we got here,
                    // because we can have the year option only if this option is selected
                    $cohortStatusSpecifiedStudents.attr("checked", "checked");

                    continue;
                }


                switch (currentArrayId) {
                    case includeAllGradsId:
                        enableGradsSection();
                        break;
                    case includeAllUndergradsId:
                        enableUndergradsSection();
                        break;
                    case "designate_term_semester_spring":
                    case "designate_term_semester_fall":
                        // fall-through is intentional here
                        $("option[value='" + currentArrayId + "']", $sectionC).attr("selected", "selected");
                        break;
                    default:
                        $("input[value='" + currentArrayId + "']").attr("checked", "checked");
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
                $regStatusOnlyDesignatedStatuses.attr("checked", "checked");
            }


            // processing 'select all' checkboxes (section c)
            if (areAllOptionsInGroupSelected($regStatusSubGroup)) {
                $regStatusSelectAllInGroup.attr("checked", "checked");
            }

            if (areAllOptionsInGroupSelected($currentOrNotSubGroup)) {
                $currencyStatusSelectAllInGroup.attr("checked", "checked");
            }

            if (areAllOptionsInGroupSelected($studentRegStatusSubGroup)) {
                $studentRegStatusSelectAllInGroup.attr("checked", "checked");
            }


            // special programs radio buttons (section c)
            if (isSomethingSelectedInSpecialProgramsSection()) {
                $specialProgramSpecifiedStudents.attr("checked", "checked");
            }

            // Student status radio buttons (section c)
            if (isSomethingSelectedInStudentStatusSection()) {
                $("#student_status_specified_students", $sectionC).attr("checked", "checked");
            }

            // Residency status radio buttons (section c)
            if (isSomethingSelectedInResidencyStatusSection()) {
                $("#residency_status_specified_students", $sectionC).attr("checked", "checked");
            }

            updateNumberOfPeopleSelectedByCriteria($.toJSON(currentList.criteria));

             if(isSomethingSelectedInSectionC(idArray)) {
                 showSectionC();
             }
        };

        var onLoadTemplate = function(data) {

            if (!data) return;

           $college.text(data.college);

           filterTemplateDataByContext(data);

           // what information do we have?
           hasUndergradsData = propExists(data.undergraduates);
           hasGradsData = propExists(data.graduates);

            // Trimpath template for sections A and B of the list editing form (section C is static and doesn't require a templete)
            var $listEditFormTemplate = $("#list_edit_form_template", $rootElement);

           // rendering the loaded template
           $view.html(sakai.api.Util.TemplateRenderer($listEditFormTemplate, {data: data, propExists: propExists}));
           setupTemplateDependentVarsAndEventHandlers();
           initListEditingForm();

           if(currentList)loadDataIntoTemplate();
        };

        /**
         * Loads page template form the current dynamic list context and renders it.
         */
        var loadTemplate = function(contextName) {

            if(arguments.length === 1) {
                setDynamicListContext(contextName);
            } else {
                setDynamicListContext();
            }

            // Derive the college template from the context.
            var firstCriterion = dynamicListContext["myb-clauses"][0];
            var matches = firstCriterion.match(/\/colleges\/([^/]+)/);
            var collegeName = matches[1];
            var templateUrl = "/devwidgets/dynamiclisteditor/colleges/" + collegeName + ".json";

            $.ajax({
                    url: templateUrl,
                    type: "GET",
                    async: true,
                    cache: true,
                    dataType: "json",
                    success: function(data){
                         onLoadTemplate(data);
                    },
                     error: function(xhr, textStatus, thrownError) {
                         showGeneralMessage(thrownError, true);
                    }
            });
        };


        ////////////////////////////////
        // Hashchange events handling //
        ////////////////////////////////

        /**
         * Sets current state of this component (list mode or edit mode).
         * This function is called when hashchange event fires.
         */
         var setState = function(){

            var state = $.bbq.getState();

            if (!(state.hasOwnProperty("l") && state.l === "dynlists")) {
                return;
            }

            if (state.hasOwnProperty("new")) {

                resetForm();

                loadTemplate();

                // When only undergraduates or graduates data exists in the template, we need to update the users count.
                // This is necessary because include undergrads/grads checkbox is checked by default in this case, but hidden.
                if((hasGradsData && !hasUndergradsData) || (!hasGradsData && hasUndergradsData)) {
                    var criteriaString = buildCriteriaStringFromListEditingForm();
                    updateNumberOfPeopleSelectedByCriteria(criteriaString);
                }
                switchToEditMode();
                $rootElement.show();
            } else if(state.hasOwnProperty("edit")) {

                loadList(state.edit, false);
                switchToEditMode();
                $rootElement.show();
            }  else if (state.hasOwnProperty("cp")) {


                loadList(state.cp, true);
                switchToEditMode();
                $rootElement.show();
            } else {
                $rootElement.hide();
            }

            // HACK: To prevent flickering this widget was made invisible in HTML code, need to undo this
            $("div.dynamiclisteditor_widget", $rootElement).show();
        };

         $(window).bind('hashchange', function() {
            setState();
        });
         
        var findDynamicListContext = function(contextName) {           
            for (var i=0; i < sakai.data.me.dynamiclistcontexts.length; i++) {
                if (sakai.data.me.dynamiclistcontexts[i].name === contextName) {
                    return sakai.data.me.dynamiclistcontexts[i];
                }
            }
            return null;
        };
        
        /**
         * Virtually all advisers will have access to only one Dynamic List context,
         * and so the UX design does not yet include a way to switch contexts.
         * To support testing and "super-advisers" (e.g., Tony), I've added this
         * URL-based hack.
         */
        var setDynamicListContext = function(contextName) {

            if(contextName) {
                dynamicListContext = findDynamicListContext(contextName);
            } else {
                var state = $.bbq.getState();
                if (state.hasOwnProperty("context")) {
                    dynamicListContext = findDynamicListContext(state.context);
                }  else {
                    dynamicListContext = sakai.data.me.dynamiclistcontexts[0];
                }
            }
            dynamicListContextUrl = "/var/myberkeley/dynamiclists/" + dynamicListContext.name;
            
            // Initialize filtering pattern from the context clauses.
            var pattern = "";
            var alloweds = dynamicListContext["myb-clauses"];
            for (var i = 0; i < alloweds.length; i++) {
                if (i > 0) {
                    pattern += "|";
                }
                pattern += alloweds[i].replace("*", ".*");
            }
            //console.log("pattern = " + pattern);
            dynamicListContextAllowed = new RegExp(pattern);
        };

        //////////////////////////
        // Validation functions //
        //////////////////////////

        var clearInvalids = function() {
            $("label.error", $rootElement).hide();
            $(".error", $rootElement).removeClass("error");
        };

        var clearInvalidsInSection = function($section) {
            $("label.error", $section).hide();
            $(".error", $section).removeClass("error");
        };


        // List name validation

        var errorMessagesForListNameValidation = {
            list_name: translate("PLEASE_ENTER_A_NAME_FOR_THIS_LIST")
        };

        var setupValidationForListName = function($frm) {
            return $frm.validate({
                debug: debugValidation,
                messages: errorMessagesForListNameValidation
            });
        };


        // Dynamic list template validation

        // Validation rules for dynamic list template

        var errorMessagesForTemplateValidation = {
        };

        var errorPlacementForTemplateValidation = function(error, element) {
            var elName = element.attr("name");
            switch(elName) {
                default:
                    error.insertAfter(element);
                break;
            }
        };

        // These two rules below make validation happen only when something is checked in undergrads or grads sections
        var undergradDependencyRuleForTemplateValidation = {
            depends: function(element) {
                return isSomethingSelectedInUndergradsSection();
            }
        };

        var gradDependencyRuleForTemplateValidation = {
            depends: function(element) {
                return isSomethingSelectedInGradsSection();
            }
        };

        var setupValidationForTemplate = function($frm) {
            return $frm.validate({
                debug: debugValidation,
                rules: {
                },
                messages: errorMessagesForTemplateValidation,
                errorPlacement: errorPlacementForTemplateValidation
            });
        };


        var clearValidationErrorsInDisabledTemplateSections = function() {
            if(!isSomethingSelectedInUndergradsSection()){
                clearInvalidsInSection($undergradsGroup);
            }

            if(!isSomethingSelectedInGradsSection()){
                clearInvalidsInSection($gradsGroup);
            }
        };
        
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

            // if the user is not a member of the advisers group then bail
            if (!myb.api.security.isUserAnAdviser()) {
                security.send403();
                return;
            }

            populateDesignateTermYear();

            dynamicListsBaseUrl = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists";

            // This is only for static form validation, validation for a template is applied when it has been loaded.
            validatorObj = setupValidationForListName($formElement);

            setState();

        };

        doInit();

    };
    
    sakai.api.Widgets.widgetLoader.informOnLoad("dynamiclisteditor");

});