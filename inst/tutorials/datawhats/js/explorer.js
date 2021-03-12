
/*
This script is used for setting up the editors, prompts, toggles and summary boxes for the datawat explorer.
*/

var lines = {};
var snippets = new Map();
var current_snippets = null;
var sortable = null;
// the last editor border to callout when clicking a summary box or the last line by default
var last_line_wrapper = null;
// the last line's callout nodes
var last_callout_nodes = null;

function setup_editors() {
  lines = {};
  console.log('setting up editors');
  $('.verb').each(function(index, element) {
    let ID = element.id;
  	console.log(index + " " + ID);
  	if (!(ID in lines)) {
      let line_editor = CodeMirror.fromTextArea(element, {
        mode: 'r',
        readOnly: 'nocursor',
        styleActiveLine: false,
        lineNumbers: false,
        firstLineNumber: 1,
        lineWrapping: true
      });
      line_editor.setSize(null, 50);
      let line_class = '.' + ID;
      let line_glyph = $(line_class + '-glyph')[0]
      let line_editor_wrapper = line_editor.getWrapperElement();
      let line_summary_box = $('#datawat-' + ID)[0];
      let line_summary_box_col = $(line_class + '-summary-box-col')[0];
      let line_summary_box_row = $(line_class + '-summary-box-row')[0];
      let line_row_content = $(line_class + "-row-content")[0];
      let line_col_content = $(line_class + "-col-content")[0];
      snippets.set((index + 1) + "", line_editor.getDoc().getValue());

      line_row_content.innerHTML = (line_row_content.innerHTML == "") ? "&nbsp;" : line_row_content.innerHTML
      line_col_content.innerHTML = (line_col_content.innerHTML == "") ? "&nbsp;" : line_col_content.innerHTML;

      // store all line related info in
      lines[ID] = {
        editor: line_editor,
        wrapper: line_editor_wrapper,
        glyph: line_glyph,
        summary_box: line_summary_box,
        summary_box_col: line_summary_box_col,
        summary_box_row: line_summary_box_row,
        line_row_content: line_row_content,
        line_col_content: line_col_content
      };
  	}
  });
  current_snippets = new Map(snippets);
}

function setup_sortable() {
  sortable = Sortable.create(simpleList, {});
  /* options */
  sortable.option("onUpdate", function( /**Event*/ evt) {
    // same properties as onEnd
    console.log("reordering");
    line_id = "#" + evt.item.id;
    console.log(line_id);
    order = sortable.toArray();
    // NOTE: for some reason sortable is keeping extra order items, so we slice it
    order = order.slice(0, Object.entries(lines).length);
    order.forEach((value, index) => console.log(index + " " + value));
    // make new snippet order
		new_snippets = order.map(o => [o, snippets.get(o)]);
    current_snippets = new Map(new_snippets);
    // send R the reorder keys
    Shiny.setInputValue("datawat-reorder", Array.from(current_snippets.keys()));
  });
}

function hide_line_wrapper() {
  if (last_line_wrapper != null) {
    console.log("hiding line wrapper");
    last_line_wrapper.style.border = "1px solid #eee";
  }
}

function hide_callout_nodes() {
  if (last_callout_nodes != null) {
    console.log("hiding line callout nodes");
    last_callout_nodes.map(node => node.className = "");
  }
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
        // hide the last line wrapper and callout nodes
        hide_line_wrapper();
        hide_callout_nodes();
        // when showing tippy, let's callout the code editor's border to draw attention to it
        lines[key].wrapper.style.border = "2px solid black";
        // enable the callout words, e.g:
        lines[key].callout_nodes.map(node => node.className = node.id);
        // the last line wrapper and callout nodes will be set here when setting up listeners
        last_line_wrapper = lines[key].wrapper;
        last_callout_nodes = lines[key].callout_nodes;
      },
      onHide(instance) {
        // we won't disable anything for now but keeping this in case
      }
    });
    line_tippy.setContent(e.summary);
    lines[key].prompt = line_tippy;
  });
  console.log("JS has set prompts!");
}

function update_prompts(summaries) {
  console.log("updating prompts...");
  summaries.forEach(e => {
    let key = e.lineid;
    lines[key].prompt.setContent(e.summary);
  });
  console.log("JS has updated prompts!");
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
    line_prompt = lines[ID].prompt;
  	// if checked, we enable the line so make divs opaque
  	checked = $(this).prop('checked')
    hide_line_wrapper();
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

// handler to be used for each summary box click event
function signal_square_clicked(e) {
  let square = $(this).attr('lineid');
  Shiny.setInputValue("datawat-square", square);
}

// this sets up the summary box click event listeners
function setup_box_listeners() {
  console.log("setting up box listeners...");
  for (const [key, value] of Object.entries(lines)) {
    line = lines[key];
    // if there was already an event listener for click, remove the listener
    line.summary_box.removeEventListener("click", signal_square_clicked);
    line.summary_box.addEventListener("click", signal_square_clicked);
  }
  Shiny.addCustomMessageHandler('square', function(message) {
    console.log("sending square message!");
    send_toggle(message);
  });
  // in order for setInputValue to re-trigger upon update of lines, we add the new lines dictionary length
  Shiny.setInputValue("datawat-need_callouts", "Gimme the callouts! " + Object.entries(lines).length);
}

// helper function to callout parts of the code snippet
function callout_code_text(callout, verb_doc) {
  // this marks the specific snippet within a verb document
  // such that we can refer to it later to enable or disable the span for callout highlights
  let snippet = callout.word;
  var lineNumber = 0;
  var charNumber = verb_doc.getValue().indexOf(snippet);
  console.log("callout in callout_code_text " + JSON.stringify(callout));

  var callout_html_node = document.createElement("span");
  callout_html_node.innerHTML = snippet;
  callout_html_node.id = callout.change;
  verb_doc.markText(
    {line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: callout_html_node}
  )
  return callout_html_node;
}

function setup_callouts(callouts) {
  console.log("got the callouts in JS! " + JSON.stringify(callouts));
  // for each lineid, add a the callout words field
  // containing a list like: [{word: "foo", change: "internal-change"}, ...]
  callouts.forEach(e => {
    let line = lines[e.lineid];
    let line_doc = line.editor.getDoc();
    let line_callouts = e.callouts;
    let line_callout_nodes = [];
    if (line_callouts != null) {
      line_callout_nodes = line_callouts.map(callout => callout_code_text(callout, line_doc));
    }
    line.callout_nodes = line_callout_nodes;
  })
  Shiny.setInputValue("datawat-need_prompts", "we need the prompts now " + callouts.length);
}

function update_callouts(callouts) {
  console.log("updating callouts...");
  // NOTE: this is pretty much identical to setup_callouts but naming it different for conveying
  // update operation
  callouts.forEach(e => {
    let line = lines[e.lineid];
    let line_doc = line.editor.getDoc();
    let line_callouts = e.callouts;
    let line_callout_nodes = [];
    if (line_callouts != null) {
      line_callout_nodes = line_callouts.map(callout => callout_code_text(callout, line_doc));
    }
    line.callout_nodes = line_callout_nodes;
  })
  console.log("JS has updated callouts!");
  Shiny.setInputValue("datawat-need_prompts", "we need the prompts now " + callouts.length);
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

  // NOTE: callouts have to be setup before the prompts because we are relying
  // on the tippy prompts to show and hide the callouts, i.e. we need to reference them
  // while we set up the onShow/onHide listener functions
  Shiny.addCustomMessageHandler('callouts', function(callouts) {
    console.log("trying to setup the callouts in JS")
    // set up the callouts
    setup_callouts(callouts);
  });

  Shiny.addCustomMessageHandler('update_callouts', function(callouts) {
    console.log("trying to update the callouts in JS")
    // update the prompts
    update_callouts(callouts);
  });

  Shiny.addCustomMessageHandler('prompts', function(summaries) {
    console.log("trying to setup the prompts in JS")
    // set up the event listener for boxes
    setup_prompts(summaries);
  });

  Shiny.addCustomMessageHandler('update_prompts', function(summaries) {
    console.log("trying to update the prompts in JS")
    // update the prompts
    update_prompts(summaries);
  });

  Shiny.addCustomMessageHandler('toggle', function(message) {
    console.log("sending toggle a message!");
    send_toggle(message);
  });

  // custom handler for updating lines after a toggle update
  // TODO also handle the ordering
  Shiny.addCustomMessageHandler('update_line', function(data) {
    console.log("received update line data!");
    console.log(data);
    // hide the last line wrapper
    hide_line_wrapper();
    // for each line, we need to update the summary box change type, the row, and column
    let j = 1;
    for (let i = 0; i < data.length; i++) {
      e = data[i];
      console.log(e.id)
      line = lines["line" + e.id];
      console.log(line);
      console.log(e.code);
      console.log(e.change);
      if (e.change != "invisible" && e.change != "invalid") {
        line.summary_box.setAttribute("lineid", j);
        console.log(line.summary_box.getAttribute("lineid"));
        j++;
        line.prompt.enable();
        // set the line wrapper for an enabled line
        last_line_wrapper = lines["line" + e.id].wrapper;
      } else {
        line.prompt.disable();
        line.summary_box.setAttribute("lineid", null);
      }
      new_summary_class = `d-flex noSelect justify-content-center ${e.change}-square`;
      line.summary_box.className = new_summary_class;
      line.line_row_content.innerHTML = (e.row == "") ? "&nbsp;" : e.row;
      line.line_col_content.innerHTML = (e.col == "") ? "&nbsp;" : e.col;
      line.editor.getDoc().setValue(e.code);
    }
    // update the last line wrapper to be the "selected" one
    last_line_wrapper.style.border = "2px solid black";
    // setup summary box event listeners again because the lineids have been updated
    // this is important so that R knows to display the right output
    setup_box_listeners();
  });

});
