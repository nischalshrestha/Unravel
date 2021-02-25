
// TODO for the value of each editor maybe include all of its divs (summary box, row/col, glyph etc.)
var lines = {};
var sortable = null;

function setup_editors() {
  $(".verb").each(function(index, element){
    ID = element.id;
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
      line_class = "." + ID;
      line_glyph = $(line_class + "-glyph")[0]
      line_editor_wrapper = line_editor.getWrapperElement();
      line_summary_box = $("#datawat-" + ID)[0];
      line_summary_box_col = $(line_class + "-summary-box-col")[0];
      line_summary_box_row = $(line_class + "-summary-box-row")[0];
      // tippy
      line_tippy = tippy(line_summary_box, {
        theme: 'light',
        allowHTML: true,
        placement: 'bottom',
        interactive: true,
        delay: [50, 50],
        trigger: 'click',
        onShow(instance) {
        	//group_by_verb_callout_nodes.map(node => node.className = node.id);
          // when showing tippy, let's callout the code editor's border to draw attention to it
          line_editor_wrapper.style.border = "2px solid black";
        },
        onHide(instance) {
        	//group_by_verb_callout_nodes.map(node => node.className = '');
          // when hiding tippy, let's remove the border callout
          line_editor_wrapper.style.border = "1px solid #eee";
        }
      });
      line_tippy.setContent("<strong>Summary</strong>: <code class='code'>group_by</code> has no visible change, but we have internally grouped the dataframe by <span class='internal-change'>year</span>, and <span class='internal-change'>sex</span>. Now we can apply operations on these groups using functions like <code class='code'>summarise</code>.");
      // store all line related info in
      lines[ID] = {
        editor: line_editor,
        wrapper: line_editor_wrapper,
        glyph: line_glyph,
        summary_box: line_summary_box,
        summary_box_col: line_summary_box_col,
        summary_box_row: line_summary_box_row,
        prompt: line_tippy
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
    if ($(this).prop('checked')) {
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
  // set up the toggle listeners
  setup_toggles();
  // set up the toggle one way (JS to R) listener
  // the R side will update the dataframe output, and send info about box, summary box/row/col, and prompt
  $('#line1-toggle').change(function() {
    Shiny.setInputValue("datawat-toggle", $(this).prop('checked'));
  })
  Shiny.addCustomMessageHandler('toggle', function(message) {
    console.log("sending toggle a message!");
    send_toggle(message);
  });

  // TODO set up the dialog prompt which requires info from R side (rn it's hardcoded in setup_editors)


});
