
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
		
		
        /////////////////////////////
        // Initialization function //
        /////////////////////////////

        /**
         * Initialization function that is run when the widget is loaded. Determines
         * which mode the widget is in (settings or main), loads the necessary data
         * and shows the correct view.
         */
        var doInit = function () {
            
			var json={ 
			b: [
				{
					type: "or_group",
					text: "OR group",
					items: [
						{
							field_name: "chk1",
							text: "Checkbox 1"
						},
						{					
							field_name: "chk2",
							text: "Checkbox 2",
							selected: true
						}
					]
				},
				{
					type: "xor_group",
					text: "XOR group",
					/*layout: "vertical",*/
					items: [
						{
							field_name: "rad",
							field_value: "rad1Value",
							text: "Radio 1",
							selected: true							
						},
						{
							field_name: "rad",
							field_value: "rad2Value",
							text: "Radio 2"
						},
						{
							field_name: "rad",
							field_value: "rad3Value",
							text: "Radio 3",
							items:[
									{
										type: "or_group",
										/*text: "OR group",*/
										items: [
											{
												field_name: "chk1",
												text: "Checkbox 1"
											},
											{
												field_name: "chk2",
												text: "Checkbox 2",
												selected: true
											}
										]
									}
							]
						}
					]
				}
			]			
			
			};
			
			//







			var major4_simple = {
				id: "SUM1", // mandatory
				name: "Sumo"  // mandatory
			};
			
			var major5_simple = {
				id: "KEN1", // mandatory
				name: "Kendo"  // mandatory
			};
	
			
			var majors3 = {
				title: "Sub majors2", // optional												
				items_array : [major4_simple, major5_simple] // mandatory				
			};
	




			var major2_simple = {
				id: "NIN1", // mandatory
				name: "Ninjutsu",  // mandatory
				sub_group: majors3
			};
			
			var major3_simple = {
				id: "KAR", // mandatory
				name: "Karate"  // mandatory
			};
	
			
			var majors2 = {
				/*title: "Sub majors",*/ // optional												
				items_array : [major2_simple, major3_simple] // mandatory				
			};
			
			
			
			
			
			
			
			
			
			
			
			var major1_simple = {
				id: "ARCH1", // mandatory
				name: "Architecture1"  // mandatory
			};
			
		
			var major2_compound = {
				id: "ARCH2",  // mandatory
				name: "Architecture2",  // mandatory
				sub_group : majors2  // sub majors, same structure as majors object below, optional
				
			};
			
			var majors = {
				title: "Undergraduate Majors and tracks", // optional
				
				select_all_option_name: "All engineering undergraduates", // optional (top level majors/programs group only)
				select_one_or_more_option_name: "Engineering undergraduates by major", // optional (top level majors/programs group only)
				
				items_array : [major1_simple, major2_compound] // mandatory				
			};
			

			
			var certificates = [
				{
					id: "CERT1", // mandatory
					name: "MOT" // mandatory
				},

				{
					id: "CERT2", // mandatory
					name: "Logistics" // mandatory
				},	
	
			];
			
			var emphases = [
				{
					id: "EMPH1",
					name: "Nanotechnology"
				},

				{
					id: "EMPH2",
					name: "Computational Engineering Science"
				},	
	
			];

			
			// If both undergrads and grads exist, they will be displayed using checkboxes
			var json_list = {
				
				college: "COE",
								
				undergraduates: {
					majors: majors,
					declared: true // whether to display "declared" block, true or false, optional
				},
				
				graduates: {
					programs: majors, //programs, // if there is only one program,  its name is displayed as title
					certificates: certificates, // optional
					emphases: emphases // optional
				}	
			};
			
			
			
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
			
			
			$view.html(sakai.api.Util.TemplateRenderer($template, template));
			
			
			//Disabling all graduate and undergraduate controls
			
			var $undergradsGroup = $(".undergrads_group");
			var $gradsGroup = $(".grads_group");
			$undergradsGroup.addClass("disabled");
			$gradsGroup.addClass("disabled");
			
			
			$(".grads_group input", $gradsGroup).each(function(i, val) {							
				$(val).attr("disabled", "disabled");
			});
			$(".undergrads_group input", $undergradsGroup ).each(function(i, val) {							
				$(val).attr("disabled", "disabled");
			});
				
			
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
			
			var $includeGrads = $('#include_grads');
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
				
			var $includeUndergrads = $('#include_undergrads');
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
						
		};

        // run the initialization function when the widget object loads
        doInit();
	
    };

	sakai.api.Widgets.Container.registerForLoad("template_test");	
});

