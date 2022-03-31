
/*
This script is used for setting up the editors, prompts, toggles and summary boxes for the unravel explorer.
*/

var lines = {};
var snippets = null;
var current_snippets = null;
var sortable = null;
// the last editor border to callout when clicking a summary box or the last line by default
var last_line_wrapper = null;
// the last line's callout nodes
var last_callout_nodes = null;
var has_error = false;
var auto_focus = false;

/*
EDITOR
*/

function setup_editors() {
  // reset state first
  lines = {};
  snippets = new Map();
  current_snippets = null;
  last_line_wrapper = null;
  last_callout_nodes = null;
  sortable = null;
  console.log('setting up editors');
  $('.verb').each(function(index, element) {
    let ID = element.id;
  	if (!(ID in lines)) {
      let line_editor = CodeMirror.fromTextArea(element, {
        mode: 'r',
        readOnly: 'nocursor',
        styleActiveLine: false,
        lineNumbers: false,
        firstLineNumber: 1,
        viewportMargin: Infinity
      });
      let line_class = '.' + ID;
      let line_id = index + 1;
      let line_glyph = $(line_class + '-glyph')[0];
      let line_editor_wrapper = line_editor.getWrapperElement();
      // this is for the line wrapper so we can click on line itself to invoke data display
      line_editor_wrapper.setAttribute("lineid", line_id);
      line_editor_wrapper.setAttribute("squareid", line_id);
      // setup summary box and row/col content elements
      let line_summary_box = $('#unravel-' + ID)[0];
      let line_summary_box_col = $(line_class + '-summary-box-col')[0];
      let line_summary_box_row = $(line_class + '-summary-box-row')[0];
      let line_row_content = $(line_class + "-row-content")[0];
      let line_col_content = $(line_class + "-col-content")[0];
      snippets.set((index + 1) + "", line_editor.getDoc().getValue());
      // on initial setup, the row and col content might be empty string so make sure to add an html space
      line_row_content.innerHTML = (line_row_content.innerHTML === "") ? "&nbsp;" : line_row_content.innerHTML;
      line_col_content.innerHTML = (line_col_content.innerHTML === "") ? "&nbsp;" : line_col_content.innerHTML;

      // store all line related info in
      lines[ID] = {
        id: line_id,
        editor: line_editor,
        wrapper: line_editor_wrapper,
        checked: true,
        glyph: line_glyph,
        summary_box: line_summary_box,
        summary_box_col: line_summary_box_col,
        summary_box_row: line_summary_box_row,
        line_row_content: line_row_content,
        line_col_content: line_col_content
      };

      // set the last line to focus
      last_line_wrapper = line_editor_wrapper;
  	}
  });
  current_snippets = new Map(snippets);
}

/*
Sortable
*/

function setup_sortable() {
  sortable = Sortable.create(simpleList, {
    // we set a filter so that we can disable dragging for the first line (dataframe line)
    // the line will have the static class on its div
    filter: '.static'
  });
  /* options */
  sortable.option("onUpdate", function( /**Event*/ evt) {
    // clever trick of cancelling a reorder
    // source: https://github.com/SortableJS/Sortable/issues/264#issuecomment-224127048
    var oldId = evt.oldIndex,
        newId = evt.newIndex,
        reArrange = sortable.toArray(),
        oldSort = sortable.toArray();
    // if we are trying to reorder anything on the first line, undo the reorder
    if (newId === 0) {
      if (oldId < newId) {
          for (var i = oldId; i < newId; i++)
              reArrange[i+1] = oldSort[i];
      } else {
          for (var i = newId + 1; i <= oldId; i++)
              reArrange[i-1] = oldSort[i];
      }
      reArrange[oldId] = oldSort[newId];
      sortable.sort(reArrange);
    } else {
      // otherwise, let the rearrange happen and inform R
      console.log("reordering");
      let line_id = "#" + evt.item.id;
      let order = sortable.toArray();
      // NOTE: for some reason sortable is keeping extra order items, so we slice it
      order = order.slice(0, Object.entries(lines).length);
      // make new snippet order
  		let new_snippets = order.map(o => [o, snippets.get(o)]);
      current_snippets = new Map(new_snippets);
      // send R the reorder keys
      Shiny.setInputValue("unravel-reorder", Array.from(current_snippets.keys()), {priority: "event"});
    }

  });

}

function hide_line_wrapper() {
  if (last_line_wrapper !== null) {
    console.log("hiding line wrapper");
    last_line_wrapper.style.border = "1px solid #eee";
  }
}

function hide_callout_nodes() {
  if (last_callout_nodes !== null) {
    console.log("hiding line callout nodes");
    last_callout_nodes.map(node => node.className = "");
  }
}

/*
Prompts
*/
function setup_prompts(summaries) {
  console.log("setting up prompts...");
  for (let i = 0; i < summaries.length; i++) {
    summary_obj = summaries[i]
    if (summary_obj.summary != "") {
      let key = summary_obj.lineid;
      let line = lines[key];
      let line_tippy = tippy(line.summary_box, {
        theme: 'light',
        allowHTML: true,
        placement: 'bottom',
        interactive: true,
        delay: [50, 50],
        trigger: 'mouseenter click',
        onShow(instance) {
          // hide the last line wrapper and callout nodes
          hide_line_wrapper();
          hide_callout_nodes();
          // when showing tippy, let's callout the code editor's border to draw attention to it
          line.wrapper.style.border = "2px solid black";
          // enable the callout words, e.g:
          line.callout_nodes.map(node => node.className = node.id);
          last_line_wrapper = line.wrapper;
          last_callout_nodes = line.callout_nodes;
        }
      });
      line_tippy.setContent(summary_obj.summary);
      line.prompt = line_tippy;
    }
  }
  auto_focus = true;
  last_line_wrapper.click();
  console.log("JS has set prompts! " + last_line_wrapper);
}

/*
Toggles
*/

function setup_toggles() {
  console.log("setting up toggles...");
  $('input[type=checkbox]').change(function() {
    ID = $(this).attr('toggle-id');
    let line = lines[ID];
    line_editor_wrapper = line.wrapper;
    line_glyph = line.glyph;
    line_summary_box = line.summary_box;
    line_summary_box_col = line.summary_box_col;
    line_summary_box_row = line.summary_box_row;
    line_prompt = line.prompt;
  	// if checked, we enable the line so make divs opaque
  	checked = $(this).prop('checked');
    line.checked = checked;
  	// whenever we toggle, let's hide the last line wrapper
    hide_line_wrapper();
    if (checked) {
      line_editor_wrapper.style.opacity = "1";
    	line_glyph.style.opacity = "1";
    	line_summary_box.style.opacity = "1";
    	line_summary_box_col.style.opacity = "1";
    	line_summary_box_row.style.opacity = "1";
    	if (line.prompt !== undefined) {
    	  line_prompt.enable();
    	}
    	last_line_wrapper = line_editor_wrapper;
    } else {
      // else hide or dim elements
      line_editor_wrapper.style.opacity = "0.25";
      line_glyph.style.opacity = "0.25";
      line_summary_box.style.opacity = "0";
      line_summary_box_col.style.opacity = "0";
      line_summary_box_row.style.opacity = "0";
      if (line.prompt !== undefined) {
        line_prompt.disable();
      }
    }
    Shiny.setInputValue(
      "unravel-toggle",
      {
        lineid: $(this).attr('line-id'),
        checked: checked
      },
      {priority: "event"}
    );
  });
}

/*
Line + Square
*/
// handler to be used for each summary box click event
function signal_square_clicked(e) {
  let key = $(this).attr('lineid');
  // if there were no function summaries (not supported yet) which means no tippy instance
  // go ahead and trigger what would've been triggered with the instance
  let line = lines['line' + key]
  if (line.prompt === undefined) {
    // hide the last line wrapper and callout nodes
    hide_line_wrapper();
    hide_callout_nodes();
    // when showing tippy, let's callout the code editor's border to draw attention to it
    line.wrapper.style.border = "2px solid black";
    // enable the callout words, e.g:
    line.callout_nodes.map(node => node.className = node.id);
    last_line_wrapper = line.wrapper;
    last_callout_nodes = line.callout_nodes;
  }
  Shiny.setInputValue("unravel-square", key, {priority: "event"});
}

// handler to be used for each line click event
function signal_line_clicked(e) {
  let square = $(this).attr('squareid');
  let line_id = $(this).attr('lineid');
  let line = lines["line" + line_id];
  console.log("attempting clicking on line: " + line_id);
  // only if the line is enabled should we try to focus on it and let R know to display data
  if (line.checked) {
    //console.log('updating line and callouts in signal_line_clicked ' + );
    console.log('updating line and callouts in signal_line_clicked: ' + line_id);
    hide_line_wrapper();
    hide_callout_nodes();
    // when showing tippy, let's callout the code editor's border to draw attention to it
    line.wrapper.style.border = "2px solid black";
    // enable the callout words, e.g:
    line.callout_nodes.map(node => node.className = node.id);
    // the last line wrapper and callout nodes will be set here when setting up listeners
    last_line_wrapper = line.wrapper;
    last_callout_nodes = line.callout_nodes;
    if (!auto_focus) {
      Shiny.setInputValue("unravel-line", square, {priority: "event"});
    } else {
      auto_focus = false;
    }
  }
}

// this sets up the summary box click event listeners
function setup_box_listeners() {
  console.log("setting up box listeners...");
  document.getElementById('unravel-fn_help_dummy').style.height = '1px';
  for (const [key, value] of Object.entries(lines)) {
    let line = lines[key];
    // we're adding both a click and a mouseenter so that the user can
    // either chose to click on a line or click on it
    // NOTE: this is redundant currently until we find a better way to trigger
    // by both a mouse event and programmatically.
    // if there was already an event listener for click, remove the listener
    line.summary_box.removeEventListener("mouseenter", signal_square_clicked);
    line.summary_box.addEventListener("mouseenter", signal_square_clicked);
    line.summary_box.removeEventListener("click", signal_square_clicked);
    line.summary_box.addEventListener("click", signal_square_clicked);

    line.wrapper.removeEventListener("mouseenter", signal_line_clicked);
    line.wrapper.addEventListener("mouseenter", signal_line_clicked);
    line.wrapper.removeEventListener("click", signal_line_clicked);
    line.wrapper.addEventListener("click", signal_line_clicked);
  }
  // in order for setInputValue to re-trigger upon update of lines, we add the new lines dictionary length
  Shiny.setInputValue("unravel-need_callouts", "Gimme the callouts! ", {priority: "event"});
}

/*
Callouts
*/

// helper function to zip lists together like in Python
// src: https://stackoverflow.com/a/10284006
function zip(arrays) {
    return arrays[0].map(function(_,i){
        return arrays.map(function(array){return array[i]})
    });
}

// helper function to callout parts of the code snippet
function callout_code_text(callout, verb_doc) {
  // this marks the specific snippet within a verb document
  // such that we can refer to it later to enable or disable the span for callout highlights
  let snippet = callout.word;
  // regex the boundary word by also excluded cases where we may have `word(` since that would
  // be a function call and we can definitely have instances where the variable name is
  // a function name as well
  const re = new RegExp("(\\b" + snippet + "\\b(?!\\())", 'g');
  // for multi-lines, we need to split them into individual lines
  // so that we can get the line number in addition to the match
  let cur_lines = verb_doc.getValue().split("\n")
  let callout_html_nodes = [];
  for (const [index, line] of cur_lines.entries()) {
    const matches = line.matchAll(re);
    for (const m of matches) {
      let callout_html_node = document.createElement("span");
      callout_html_node.innerHTML = snippet;
      callout_html_node.id = callout.change;
      if (m !== undefined) {
        let charNumber = m.index;
        verb_doc.markText(
          {line: index, ch: charNumber},
          {line: index, ch: charNumber + snippet.length},
          {replacedWith: callout_html_node}
        )
      } else {
        // otherwise, set id to "" so that we don't call out any code text
        callout_html_node.id = "";
      }
      callout_html_nodes.push(callout_html_node);
    }
  }
  return callout_html_nodes;
}

function setup_callouts(callouts) {
  console.log("got the callouts in JS! " + JSON.stringify(callouts));
  // for each lineid, add a the callout words field
  // containing a list like: [{word: "foo", change: "internal-change"}, ...]
  // mark variables to highlight in the code text in the Codemirror text editors
  callouts.forEach(e => {
    let line = lines[e.lineid];
    let line_doc = line.editor.getDoc();
    let line_callouts = e.callouts;
    let line_callout_nodes = [];
    if (line_callouts != null) {
      line_callout_nodes = line_callouts.map(callout => callout_code_text(callout, line_doc));
      line_callout_nodes = line_callout_nodes.flat();
    }
    line.callout_nodes = line_callout_nodes;
  })
  Shiny.setInputValue("unravel-need_prompts", "we need the prompts now ", {priority: "event"});
}

/*
Help text linking
*/

// a helper function that will mark all of the function text as hyperlink
// for each line in the code
function setup_fns_help(fns_help) {
  console.log('got the fns_help in JS! ' + JSON.stringify(fns_help));
  // for each element in the JSON list of [{ <function>: <string>, ... }, ...]
  // mark functions in the code text in the Codemirror text editors
  fns_help.forEach((e, index, fullArray) => {
    let line = lines[e.lineid];
    let line_doc = line.editor.getDoc();
    let line_fns_help = e.fns_help;
    let line_fns_help_nodes = [];
    if (line_fns_help != null) {
      // for each line/editor, create html nodes that replace text that should be
      // functions in R
      line_fns_help_nodes = fns_help_code_text(line_fns_help, line_doc, index);
      line_fns_help_nodes = line_fns_help_nodes.flat();
    }
    line.line_fns_help_nodes = line_fns_help_nodes;
  })
}

// helper function to hyperlink parts of the code snippet that has a function call
function fns_help_code_text(fns_help, verb_doc, editor_index) {
  let cur_lines = verb_doc.getValue().split("\n")
  let fns_html_nodes = [];
  for (const [i, fn_help] of fns_help.entries())  {
    let fn_name = fn_help.word;
    // regex the boundary word
    const re = new RegExp("(\\b" + fn_name + "\\b)", 'g');
    // go through every line of the editor to match regex and hyperlink functions
    for (const [index, line] of cur_lines.entries()) {
      const matches = line.matchAll(re);
      for (const m of matches) {
        // construct the HTML markup for CodeMirror
        let fn_html_node = document.createElement("span");
        fn_html_node.innerHTML = fn_help.html;
        fn_html_node.id = fn_name;
        fn_html_node.addEventListener("click", function(event) {
          	Shiny.setInputValue("unravel-fn_help", event.target.id, {priority: "event"});
        });
        // mark up text or leave it be
        if (m !== undefined) {
          let charNumber = m.index;
          verb_doc.markText(
            {line: index, ch: charNumber},
            {line: index, ch: charNumber + fn_name.length},
            {replacedWith: fn_html_node}
          )
        } else {
          // otherwise, set id to "" so that we don't call out any code text
          fn_html_node.id = "";
        }
        fns_html_nodes.push(fn_html_node);
      }
    }
  }

  return fns_html_nodes;
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

  /*
  * Table interaction logging:
  * The following 2 event listeners are for logging if users hover, or click
  * on `reactable`
  */

  // listen to any click and print target
  document.getElementById('unravel-line_table').addEventListener("click", function(e) {
    console.log("Click event on table!")
    Shiny.setInputValue("unravel-table_focus", "clicking on table", {priority: "event"});
  })

  // listen to any focus with mouse on reactable
  document.getElementById('unravel-line_table').addEventListener("mouseenter", function(e) {
    console.log("Mouse hover event on table!")
    Shiny.setInputValue("unravel-table_focus", "focusing on table", {priority: "event"});
  })

  document.getElementById('unravel-data_details').addEventListener("mouseenter", function(e) {
    console.log("Mouse hover event on data details!")
    Shiny.setInputValue("unravel-data_details_focus", "focusing on data details", {priority: "event"});
  })

  Shiny.addCustomMessageHandler('need_explorer', function(message) {
    console.log("JS is signaling R " + message);
    Shiny.setInputValue("unravel-explorer_ready", "explorer ready!", {priority: "event"});
  });

  // NOTE: callouts have to be setup before the prompts because we are relying
  // on the tippy prompts to show and hide the callouts, i.e. we need to reference them
  // while we set up the onShow/onHide listener functions
  Shiny.addCustomMessageHandler('callouts', function(callouts) {
    console.log("trying to setup the callouts in JS")
    // set up the callouts
    setup_callouts(callouts);
  });

  Shiny.addCustomMessageHandler('fns_help', function(fns_help) {
    console.log("trying to setup the fns_help in JS")
    // set up the fns_help
    setup_fns_help(fns_help);
  });

  Shiny.addCustomMessageHandler('prompts', function(summaries) {
    console.log("trying to setup the prompts in JS")
    // set up the event listener for boxes
    setup_prompts(summaries);
  });

  Shiny.addCustomMessageHandler('toggle', function(message) {
    console.log("sending toggle a message!");
    send_toggle(message);
  });

  Shiny.addCustomMessageHandler('square', function(message) {
    console.log("sending square message!");
    send_toggle(message);
  });

  // custom handler for updating lines after a toggle update
  Shiny.addCustomMessageHandler('update_line', function(data) {
    console.log("received update line data!");
    console.log(data);
    // hide the last line wrapper
    hide_line_wrapper();
    hide_callout_nodes();
    // for each line, we need to update the summary box change type, the row, and column
    // this j counter is for setting the correct lineid for summary boxes
    let j = 1;
    for (let i = 0; i < data.length; i++) {
      e = data[i];
      let line = lines["line" + e.id];
      // always destroy the tippy instances
      if (line.prompt !== undefined) {
        line.prompt.destroy();
      }
      let shape = (e.col !== "") ? "-square" : "-rect"
      if (e.change != "invisible" && e.change != "invalid") {
        if (line.prompt !== undefined) {
          line.prompt.enable();
        }
        line.summary_box.setAttribute("lineid", j);
        line.wrapper.setAttribute("squareid", j);
        last_line_wrapper = line.wrapper;
        j++;
      } else {
        line.summary_box.setAttribute("lineid", null);
        line.wrapper.setAttribute("squareid", null);
      }
      new_summary_class = `d-flex noSelect justify-content-center ${e.change}${shape}`;
      line.summary_box.className = new_summary_class;
      // make sure to hide or reveal the row/col based on whether or not that information
      // is available (e.g. error lines would have nothing, lists would only have row, etc.)
      // since R initially would set size to 0em during first Unravel, we have to properly
      // set the font size when we do have the shape information on updates
      line.line_row_content.innerHTML = (e.row == "") ? "&nbsp;" : e.row;
      line.line_row_content.style.fontSize = (e.row !== "") ? "0.8em" : "0em";
      line.line_col_content.innerHTML = (e.col == "") ? "&nbsp;" : e.col;
      line.line_col_content.style.fontSize = (e.col !== "") ? "0.8em" : "0em";
      // finally, set the new code text on the editor
      line.editor.getDoc().setValue(e.code);
    }
    // setup summary box event listeners again because the lineids have been updated
    // this is important so that R knows to display the right output
    setup_box_listeners();
  });

});
