// Thanks to Zach Holman (http://zachholman.com/) for this awesomesauce
jQuery(document).ready(function($) {

    $('#twitter').mouseover(function() {
        $('#hover-text').text("Twitter");
    });

    $('#twitter').mouseout(function() {
        $('#hover-text').text("Around the Web:");
    });

    $('#facebook').mouseover(function() {
        $('#hover-text').text("Facebook");
    });

    $('#facebook').mouseout(function() {
        $('#hover-text').text("Around the Web:");
    });

    $('#github').mouseover(function() {
        $('#hover-text').text("GitHub");
    });

    $('#github').mouseout(function() {
        $('#hover-text').text("Around the Web:");
    });

    $('#instagram').mouseover(function() {
        $('#hover-text').text("Instagram");
    });

    $('#instagram').mouseout(function() {
        $('#hover-text').text("Around the Web:");
    });
});