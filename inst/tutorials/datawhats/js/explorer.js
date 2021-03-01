
/*
This script is used for setting up the editors, prompts, toggles and summary boxes for the datawat explorer.
*/

var lines = {};
var snippets = new Map();
var current_snippets = null;
var sortable = null;

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

function setup_box_listeners() {
  console.log("setting up box listeners...");
  for (const [key, value] of Object.entries(lines)) {
  	$("#datawat-"+key)[0].addEventListener("click", function() {
  	  let square = $(this).attr('lineid');
      let toggleId = "#line" + square;
  	  Shiny.setInputValue("datawat-square", square);
  	});
  	// the R side will update the dataframe output, and send info about box, summary box/row/col, and prompt
    Shiny.addCustomMessageHandler('square', function(message) {
      console.log("sending square message!");
      send_toggle(message);
    });
  }
  // in order for setInputValue to re-trigger upon update of lines, we add the new lines dictionary length
  Shiny.setInputValue("datawat-ready", "Gimme the prompts, and I'll setup everything else! " + Object.entries(lines).length);
}

function update_box_listeners(ids) {
  ids.forEach(i => {
    // for each summary box set a new lineid
    $("#datawat-" + i)[0].attr('lineid', i);
    /*
    $("#datawat-" + i)[0].addEventListener("click", function() {
  	  let square = $(this).attr('lineid');
      let toggleId = "#line" + square;
  	  Shiny.setInputValue("datawat-square", square);
  	});
  	*/
  })
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

  Shiny.addCustomMessageHandler('update_prompts', function(summaries) {
    console.log("trying to update the prompts JS")
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
    // for each line, we need to update the summary box change type, the row, and column

    for (let i = 0; i < data.length; i++) {
      e = data[i];
      line = lines["line" + e.id];
      //console.log(line);
      console.log(e.code);
      line.summary_box.setAttribute("lineid", i + 1);
      console.log(line.summary_box.getAttribute("lineid"));
      new_summary_class = `d-flex noSelect justify-content-center ${e.change}-square`;
      line.summary_box.className = new_summary_class;
      line.line_row_content.innerHTML = (e.row == "") ? "&nbsp;" : e.row;
      line.line_col_content.innerHTML = (e.col == "") ? "&nbsp;" : e.col;
      line.editor.getDoc().setValue(e.code);
    }

    /*
    data.forEach(e => {
      ID = "line" + e.id;
      line = lines[ID];
      console.log(e.code);
      line.summary_box.setAttribute("lineid", e.id);
      console.log(line.summary_box.getAttribute("lineid"));
      new_summary_class = `d-flex noSelect justify-content-center ${e.change}-square`;
      line.summary_box.className = new_summary_class;
      line.line_row_content.innerHTML = (e.row == "") ? "&nbsp;" : e.row;
      line.line_col_content.innerHTML = (e.col == "") ? "&nbsp;" : e.col;
      line.editor.getDoc().setValue(e.code);
    });
    */
  });

});
