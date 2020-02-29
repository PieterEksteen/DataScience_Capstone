$(document).keyup(function(event) {
        if ($("#string").is(":focus") && (event.key == "Enter")) {
            $("#predict").click();
        }
    });