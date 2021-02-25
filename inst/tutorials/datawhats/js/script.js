
// TODO for the value of each editor maybe include all of its divs (summary box, row/col, glyph etc.)
var lines = {};
var sortable = null;

function setup_editors() {
  $(".verb").each(function(index, element){
    let ID = element.id;
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
      let line_class = "." + ID;
      let line_glyph = $(line_class + "-glyph")[0]
      let line_editor_wrapper = line_editor.getWrapperElement();
      let line_summary_box = $("#datawat-" + ID)[0];
      let line_summary_box_col = $(line_class + "-summary-box-col")[0];
      let line_summary_box_row = $(line_class + "-summary-box-row")[0];

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
}

function setup_toggles() {
  // set up the toggle event css listeners
  $('input:checkbox').change(function() {
    ID = $(this).attr('toggle-id')
    console.log("Event on ID: " + ID);
    line_editor_wrapper = lines[ID].wrapper;
    line_glyph = lines[ID].glyph;
    line_summary_box = lines[ID].summary_box;
    line_summary_box_col = lines[ID].summary_box_col;
    line_summary_box_row = lines[ID].summary_box_row;
    line_prompt = lines[ID].prompt;
  	// if checked, we enable the line so make divs opaque
  	checked = $(this).prop('checked')
    if (checked) {
      line_editor_wrapper.style.opacity = "1";
    	line_glyph.style.opacity = "1";
    	line_summary_box.style.opacity = "1";
    	line_summary_box_col.style.opacity = "1";
    	line_summary_box_row.style.opacity = "1";
    	line_prompt.enable();
    } else {
      // else hide or dim elements
      line_editor_wrapper.style.opacity = "0.25";
      line_glyph.style.opacity = "0.25";
      line_summary_box.style.opacity = "0";
      line_summary_box_col.style.opacity = "0";
      line_summary_box_row.style.opacity = "0";
    	line_prompt.disable();
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

function send_toggle(message) {
  console.log("R received message from JS. " + message);
}

$(document).ready(function() {
  // The last verb will include the script that kicks everything off.
  console.log("ready!");
  // set up editors
  setup_editors();
  // set up the sortablejs
  setup_sortable();
});

// For other JS events we can listen to: https://shiny.rstudio.com/articles/js-events.html
// We're doing this so that we know shiny is fully initialized before moving on to providing
// more information to UI from R. JS here will initialize prompts and the summary box event listeners.
$(document).on("shiny:sessioninitialized", function(event) {
  Shiny.setInputValue("datawat-ready", "R, do yer thang!");
  Shiny.addCustomMessageHandler('ready', function(summaries) {
    // set up the event listener for boxes
    console.log(summaries);
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
    // set up the toggle listeners
    setup_toggles();
  });

  // set up the box event listeners
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

  Shiny.addCustomMessageHandler('toggle', function(message) {
    console.log("sending toggle a message!");
    send_toggle(message);
  });

  // TODO the R side will have to update the following upon change in events:
  // - dataframe output,
  // - type of change summary box,
  // - summary boxrow/col,
  // - prompt

});
