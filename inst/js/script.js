
/*
This script is used for setting up the initial user input editor and send signal to R to consume when
the user clicks on the button.
*/

var code_input = null;

function setup_code_input_editor() {
  code_input = CodeMirror.fromTextArea($(".code_input")[0], {
        mode: 'r',
        lineNumbers: true
  });
}

function send_toggle(message) {
  console.log("R received message from JS. " + message);
}

// For other JS events we can listen to: https://shiny.rstudio.com/articles/js-events.html
// We're doing this so that we know shiny is fully initialized before trying to make custom
// handler for code input editor text
$(document).on("shiny:sessioninitialized", function(event) {
  console.log("shiny initialized on main script");

  // handler to set the input editor's text to the one sent by R via examples dropdown menu
  Shiny.addCustomMessageHandler('set_code', function(message) {
    console.log("Getting code from R: " + message);
    Shiny.setInputValue("unravel-code_ready", message);
  });

  // handler to send R back the code when clicking the Unravel button
  Shiny.addCustomMessageHandler('need_code', function(message) {
    console.log("From R: " + message);
    Shiny.setInputValue("unravel-code_ready", code_input.getDoc().getValue());
  });

});
