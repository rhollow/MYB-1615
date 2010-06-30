/*global $, setTitles, window */

var sakai = sakai || {};

var doInit = function(){
    $(".CheckBoxClass").change(function(){
        if ($(this).is(":checked")) {
            $(this).next("label").addClass("LabelSelected");
        }
        else {
            $(this).next("label").removeClass("LabelSelected");
        }
        setTitles();
        setTitles();
    });
};

window.onload(doInit());
