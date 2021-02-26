
/*
This script is used for setting up the editors, prompts, toggles and summary boxes for the datawat explorer.
*/

var lines = {};
var sortable = null;

function setup_editors() {
  console.log('setting up editors');
  $('.verb').each(function(index, element){
    let ID = element.id;
     // $('input[type=checkbox]')[0].getAttribute("toggle-id");
  	console.log(index + " " + ID);
  	if (!(ID in lines)) {
      let line_editor = CodeMirror.fromTextArea(element, {
        mode: 'r',
        readOnly: 'nocursor',
        styleActiveLine: false,
        lineNumbers: false,
        firstLineNumber: 1
      });
      line_editor.setSize(null, 50);
      let line_class = '.' + ID;
      let line_glyph = $(line_class + '-glyph')[0]
      let line_editor_wrapper = line_editor.getWrapperElement();
      let line_summary_box = $('#datawat-' + ID)[0];
      let line_summary_box_col = $(line_class + '-summary-box-col')[0];
      let line_summary_box_row = $(line_class + '-summary-box-row')[0];

      // store all line related info in
      lines[ID] = {
        editor: line_editor,
        wrapper: line_editor_wrapper,
        glyph: line_glyph,
        summary_box: line_summary_box,
        summary_box_col: line_summary_box_col,
        summary_box_row: line_summary_box_row
      };
  	}
  });
}

function setup_sortable() {
  sortable = Sortable.create(simpleList, { });
  //Shiny.setInputValue("datawat-ready", "Gimme the prompts, and I'll setup everything else!");
}

function setup_prompts(summaries) {
  console.log("setting up prompts...");
  summaries.forEach(e => {
    console.log(e);
    let key = e.lineid;
    let line_tippy = tippy(lines[key].summary_box, {
      theme: 'light',
      allowHTML: true,
      placement: 'bottom',
      interactive: true,
      delay: [50, 50],
      trigger: 'click',
      onShow(instance) {
        // when showing tippy, let's callout the code editor's border to draw attention to it
        lines[key].wrapper.style.border = "2px solid black";
      },
      onHide(instance) {
        // when hiding tippy, let's remove the border callout
        lines[key].wrapper.style.border = "1px solid #eee";
      }
    });
    line_tippy.setContent(e.summary);
    lines[key].prompt = line_tippy;
  });
  console.log("JS has set prompts!");
}

function setup_toggles() {
  console.log("setting up toggles...");

  $('input[type=checkbox]').change(function() {
    ID = $(this).attr('toggle-id')
    console.log("Event on ID: " + ID);
    line_editor_wrapper = lines[ID].wrapper;
    line_glyph = lines[ID].glyph;
    line_summary_box = lines[ID].summary_box;
    line_summary_box_col = lines[ID].summary_box_col;
    line_summary_box_row = lines[ID].summary_box_row;
  	// if checked, we enable the line so make divs opaque
  	checked = $(this).prop('checked')
    if (checked) {
      line_editor_wrapper.style.opacity = "1";
    	line_glyph.style.opacity = "1";
    	line_summary_box.style.opacity = "1";
    	line_summary_box_col.style.opacity = "1";
    	line_summary_box_row.style.opacity = "1";
    } else {
      // else hide or dim elements
      line_editor_wrapper.style.opacity = "0.25";
      line_glyph.style.opacity = "0.25";
      line_summary_box.style.opacity = "0";
      line_summary_box_col.style.opacity = "0";
      line_summary_box_row.style.opacity = "0";
    }
    Shiny.setInputValue(
      "datawat-toggle",
      {
        lineid: $(this).attr('line-id'),
        checked: checked
      }
    );
  });
}

function setup_box_listeners() {
  console.log("setting up box listeners...");
  for (const [key, value] of Object.entries(lines)) {
  	$("#datawat-"+key)[0].addEventListener("click", function() {
  	  let square = $(this).attr('lineid');
  	  Shiny.setInputValue("datawat-square", square);
  	});
  	// the R side will update the dataframe output, and send info about box, summary box/row/col, and prompt
    Shiny.addCustomMessageHandler('square', function(message) {
      console.log("sending square message!");
      send_toggle(message);
    });
  }
  Shiny.setInputValue("datawat-ready", "Gimme the prompts, and I'll setup everything else!");
}

function send_toggle(message) {
  console.log("R received message from JS. " + message);
}

$(document).ready(function() {
  console.log("explorer js document ready!");
});

// For other JS events we can listen to: https://shiny.rstudio.com/articles/js-events.html
// We're doing this so that we know shiny is fully initialized before moving on to providing
// more information to UI from R. JS here will initialize prompts, toggles, and the summary box event listeners.
$(document).on("shiny:sessioninitialized", function(event) {
  console.log("shiny initialized on explorer");

  Shiny.addCustomMessageHandler('need_explorer', function(message) {
    console.log("JS is signaling R " + message);
    Shiny.setInputValue("datawat-explorer_ready", "explorer ready!");
  });

  Shiny.addCustomMessageHandler('prompts', function(summaries) {
    console.log("trying to setup the rest in JS")
    // set up the event listener for boxes
    setup_prompts(summaries);
  });

  Shiny.addCustomMessageHandler('toggle', function(message) {
    console.log("sending toggle a message!");
    send_toggle(message);
  });

});
