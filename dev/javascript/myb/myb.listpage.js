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

require(["jquery","sakai/sakai.api.core", "myb/myb.api.core", "/dev/lib/myb/myb.dynlist.logic.js", "/dev/javascript/myb/myb.securepage.js"],
    function($, sakai, myb, Condition) {
    sakai_global.listpage = function(){

        /////////////////////////////
        // Configuration variables //
        /////////////////////////////

        /**
         * Hashtable of all the lists
         */
        var allLists = {};

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

            if ($("#cohort_status_specified_students", $sectionC).is(':checked')) {
                var semester = $("#designate_term_semester", $cohortStatus).val();
                var year = $("#designate_term_year", $cohortStatus).val();
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

            if($("#reg_status_only_designated_statuses", $sectionC).is(':checked')) {

                var selectedRegisteredOR = buildSelectedOptionsObjectAsOR(null, $(".reg_status .sub_group", $sectionC));
                var selectedCurrencyOR = buildSelectedOptionsObjectAsOR(null, $(".current_or_not .sub_group", $sectionC));
                var selectedWithdrawnOR = buildSelectedOptionsObjectAsOR(null, $(".student_reg_status .sub_group", $sectionC));

                registrationStatus = registrationStatus.joinTwoConditionsByAND(selectedRegisteredOR);
                registrationStatus = registrationStatus.joinTwoConditionsByAND(selectedCurrencyOR);
                registrationStatus = registrationStatus.joinTwoConditionsByAND(selectedWithdrawnOR);
            }

            return registrationStatus;
        };

        /**
         * Gathers all undergraduates related information and returns it as an object.
         * Returned information includes: undergraduate majors, levels, 'admitted as' status and 'declared' status.
         *
         * @return {Object} a condition object containing all undergraduates related information in the AND field.
         */
        var buildUndergradsObjectAsAND = function() {

            var undergrads = new Condition();

            var selectedUndergradMajorsOR = buildSelectedOptionsObjectAsOR(null, $(".majors"));
            var selectedLevelsOR = buildSelectedOptionsObjectAsOR(null, $(".levels"));
            var selectedAdmittedAsOR = buildSelectedOptionsObjectAsOR(null, $(".admittedAs"));
            var selectedDeclaredOR = buildSelectedOptionsObjectAsOR(null, $(".declared"));

            undergrads = undergrads.joinTwoConditionsByAND(selectedUndergradMajorsOR);
            undergrads = undergrads.joinTwoConditionsByAND(selectedLevelsOR);
            undergrads = undergrads.joinTwoConditionsByAND(selectedAdmittedAsOR);
            undergrads = undergrads.joinTwoConditionsByAND(selectedDeclaredOR);

            return undergrads;
        };

        /**
         * Gathers all graduate students related information and returns it as an object.
         * Returned information includes: graduate programs, certificates, emphases, degrees.
         *
         * @return {Object} a condition object containing all graduate students related information in the AND field.
         */
        var buildGradsObjectAsAND = function() {

            var grads = new Condition();

            var selectedGradProgramsOR = buildSelectedOptionsObjectAsOR(null, $(".programs"));
            var selectedCertificatesOR = buildSelectedOptionsObjectAsOR(null, $(".certificates"));
            var selectedEmphasesOR = buildSelectedOptionsObjectAsOR(null, $(".emphases"));
            var selectedDegreesOR = buildSelectedOptionsObjectAsOR(null, $(".degrees"));

            grads = grads.joinTwoConditionsByAND(selectedGradProgramsOR);
            grads = grads.joinTwoConditionsByAND(selectedCertificatesOR);
            grads = grads.joinTwoConditionsByAND(selectedEmphasesOR);
            grads = grads.joinTwoConditionsByAND(selectedDegreesOR);

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
            var undergrads = new Condition();

            if (boolIncludeUndergrads) {
                undergrads = buildUndergradsObjectAsAND();
                if(undergrads.isConditionObjectEmpty()) {
                    undergrads.OR =[$includeUndergradsCheckbox.val()];
                }
            }


            // For grads
            var grads = new Condition();

            if (boolIncludeGrads) {
                grads = buildGradsObjectAsAND();
                if(grads.isConditionObjectEmpty()) {
                    grads.OR = [$includeGradsCheckbox.val()];
                }

            }

            var result = new Condition();

            if (boolIncludeUndergrads && boolIncludeGrads) {

                result = undergrads.joinTwoConditionsByOR(grads);

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
            $showMoreOrLess.text("Show More");
            $sectionC.hide();
        };

        /**
         * Shows section C (common filter settings).
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

            // Prevent unnecessary AJAX requests
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
                error: function() {
                  $studentsTargetedByCurrentList.addClass("noStudentsTargeted");
                  $studentsTargetedByCurrentList.text("N/A");
                }
          });

        };

        /**
         * Switches to edit dynamic list mode
         */
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

            $dynListsCancelEditingButton.show();
            $dynListsSaveButton.show();
        };

        /**
         * Switches to dynamic lists table mode
         */
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


        /**
         * sorts an array of dynamic lists on the date of last modification, this can be used with the JavaScript sort function.
         *
         * @param {Object} a  a dynamic list item
         * @param {Object} b  another dynamic list item
         *
         * @return {Number} Less than 0: Sort "a" to be a lower index than "b";
         *                   Zero: "a" and "b" should be considered equal, and no sorting performed;
         *                   Greater than 0: Sort "b" to be a lower index than "a".
         */
        var sortByDateFunction = function(a, b) {

            return a['_lastModified'] - b['_lastModified'];
        };

        /**
         * Renders loaded lists to the lists table
         */
        var displayLoadedLists = function() {

            //Sort by date
            var listsArray = [];
            for(var key in allLists){
                if(allLists.hasOwnProperty(key)) {
                    listsArray.push(allLists[key]);
                }
            }
            listsArray.sort(sortByDateFunction);



            var data = {
                links: listsArray, //allLists,
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
            var totalOptionsSelected = getNumberOfSelectedOptionsInGroup($(".reg_status .sub_group", $sectionC)) +
            getNumberOfSelectedOptionsInGroup($(".current_or_not .sub_group", $sectionC)) +
            getNumberOfSelectedOptionsInGroup($(".student_reg_status .sub_group", $sectionC));

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
         * Returns true is something selected in section C.
         *
         * @param idArray an array of filter IDs.
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
         * Resets the editing form and loads the list with specified id into it.
         * @param {String} id    The id of a list
         * @param {Boolean} copyMode    When set to true string "Copy of " is prepended to the name of the displayed list and description is cleared
         */
        var loadListIntoEditingForm = function(id, copyMode) {

            resetListEditingForm();

            if (!allLists.hasOwnProperty(id) || allLists[id] === null) {
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

             if(isSomethingSelectedInSectionC(idArray)) {
                 showSectionC();
             }

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
                            sakai.api.Util.notification.show(thrownError, "", sakai.api.Util.notification.type.ERROR);
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

        /**
         * Gathers all information from sections A, B and C.
         * Returns a condition object created from this data as string.
         *
         *  @return {Object} a condition object created from sections A, B, C data.
         */
         var buildFilterFromListEditingForm = function() {

             // Sections A and B
             var dynamicListFilter = buildUndergraduatesAndGraduatesResultingObject();

            // Section C

            var registrationStatus = buildRegistrationStatusObject();
            dynamicListFilter = dynamicListFilter.joinTwoConditionsByAND(registrationStatus);


            var cohortStatus = buildCohortStatusObject();
            dynamicListFilter = dynamicListFilter.joinTwoConditionsByAND(cohortStatus);


            var specialPrograms = buildSelectedOptionsObjectAsOR($("#special_program_all_students", $sectionC), $(".special_programs", $sectionC));
            dynamicListFilter = dynamicListFilter.joinTwoConditionsByAND(specialPrograms);

            var studentStatus = buildSelectedOptionsObjectAsOR($("#student_status_all_students", $sectionC), $(".student_and_residency_status_col_left .sub_group", $sectionC));
            dynamicListFilter = dynamicListFilter.joinTwoConditionsByAND(studentStatus);

            var residencyStatus = buildSelectedOptionsObjectAsOR($("#residency_status_all_students", $sectionC), $(".student_and_residency_status_col_right .sub_group", $sectionC));
            dynamicListFilter = dynamicListFilter.joinTwoConditionsByAND(residencyStatus);

            return dynamicListFilter;
        };

        /**
         * Gathers all information from sections A, B and C.
         * Returns a condition object created from this data as string.
         *
         *  @return {String} a condition object created from sections A, B, C data as string.
         */
         var buildFilterStringFromListEditingForm = function() {

            return $.toJSON(buildFilterFromListEditingForm());
        };

        var validateUserInput = function() {
            var listName = $.trim($("#list_name").val());
            if (listName === null || listName === "") {
                $("#invalid_name").show();
                return false;
            }

            return true;
        };

        var getDataFromInput = function() {
            var result = {};

            result.context = "g-ced-students";
            result.listName = $.trim($("#list_name").val());
            result.desc = $.trim($("#description").val());

            // Gathering the data on standing
            //TODO: check for errors

            result.filter = buildFilterStringFromListEditingForm();

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
                showGeneralMessage($("#inbox_generalmessages_already_exists").text(), true);
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
                    url: val,
                    method: "POST",
                    parameters: {
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
                error: function() {
                   showGeneralMessage("Error deleting lists.", true);
                }
            });
        };

        /**
         * Removes lists with provided IDs from DOM and sends batch delete AJAX request.
         *
         *  @param listIds {Array} IDs of lists to delete
         */
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
                showGeneralMessage($("#inbox_generalmessages_none_selected").text(), true);
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

            if (listIds.length === 0) {
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

            if (listIds.length === 0) {
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

        $dynListsCancelEditingButton.live("click", function(){
            //editExisting = false;
            $.bbq.pushState({},2);
        });

        $dynListsSaveButton.live("click", function(){
            $("#invalid_name").hide();
            $("#invalid_major").hide();

            if(!validateUserInput()) {
                return;
            }
            var data = getDataFromInput();

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

        /**
         * Defines variables and sets up event handlers for template-dependent page elements
         * (i.e. elements that do not exist before template loading).
         */
        var setupTemplateDependentVarsAndEventHandlers = function() {

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

            // TODO: ask Rachel if we need these two handlers (design has changed)
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
        };

        ////////////////////
        // Page templates //
        ////////////////////

        /**
         * Loads page template form the given url and renders it.
         *
         * @param templateUrl Template URL, for ex. "nauth_ced.json"
         */
        var loadTemplate = function(templateUrl) {

            // Trimpath template for sections A and B of the list editing form (section C is static and doesn't require a templete)
            var $listEditFormTemplate = $("#list_edit_form_template");

            // View to render the template for sections A and B
            var $view = $("#view");

            $.ajax({
                    url: templateUrl,
                    type: "GET",
                    async: false,
                    cache: false,
                    dataType: "json",
                    success: function(data){
                        if (data) {
                           // what information do we have?
                           boolTemplateHasUndergradsData = typeof(data.undergraduates) !== 'undefined' && data.undergraduates !== null;
                           boolTemplateHasGradsData = typeof(data.graduates) !== 'undefined' && data.graduates !== null;

                           // rendering the loaded template
                           $view.html(sakai.api.Util.TemplateRenderer($listEditFormTemplate, data));
                        }
                    },
                     error: function(xhr, textStatus, thrownError) {
                        sakai.api.Util.notification.show(thrownError,"",sakai.api.Util.notification.type.ERROR);
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

            loadTemplate("nauth_ced.json"); // TODO load from advisor's node? or somewhere else based on college membership?

            populateDesignateTermYear();

            setupTemplateDependentVarsAndEventHandlers();

            // this is needed for the situation when we reload this page with some hash parameter, like #new
            resetListEditingForm();

            dynamicListsBaseUrl = "/~" + sakai.data.me.user.userid + "/private/dynamic_lists";

            loadDynamicListsFromServer();
        };

        doInit();
    };

    sakai.api.Widgets.Container.registerForLoad("listpage");
});
