
// TODO for the value of each editor maybe include all of its divs (summary box, row/col, glyph etc.)
var editors = {};
var sortable = null;

function create_editors() {
  $(".verb").each(function(index, element){
    ID = element.id;
  	console.log(index + " " + ID);
  	if (!(ID in editors)) {
      let verb_editor = CodeMirror.fromTextArea(element, {
        mode: 'r',
        readOnly: 'nocursor',
        styleActiveLine: false,
        lineNumbers: false,
        firstLineNumber: 1
      });
      verb_editor.setSize(null, 50);
      editors[ID] = verb_editor;
  	}
  });
}

function create_sortable() {
  sortable = Sortable.create(simpleList, { });
}

var line1_summary_box = null;
var line1_editor_wrapper = null;
var line1_summary_box_row = null;

function setup_toggles() {
  // set up the toggle event css listeners
  line1_glyph = $(".line1-glyph")[0]
  line1_editor_wrapper = editors["line1"].getWrapperElement();
  line1_summary_box = $("#datawat-line1")[0];
  line1_summary_box_col = $(".line1-summary-box-col")[0];
  line1_summary_box_row = $(".line1-summary-box-row")[0];

  line1_tippy = tippy(line1_summary_box, {
    theme: 'light',
    allowHTML: true,
    placement: 'bottom',
    interactive: true,
    delay: [50, 50],
    trigger: 'click',
    onShow(instance) {
    	//group_by_verb_callout_nodes.map(node => node.className = node.id);
      // when showing tippy, let's callout the code editor's border to draw attention to it
      line1_editor_wrapper.style.border = "2px solid black";
    },
    onHide(instance) {
    	//group_by_verb_callout_nodes.map(node => node.className = '');
      // when hiding tippy, let's remove the border callout
      line1_editor_wrapper.style.border = "1px solid #eee";
    }
  });

  line1_tippy.setContent("<strong>Summary</strong>: <code class='code'>group_by</code> has no visible change, but we have internally grouped the dataframe by <span class='internal-change'>year</span>, and <span class='internal-change'>sex</span>. Now we can apply operations on these groups using functions like <code class='code'>summarise</code>.");

  $('#line1-toggle').change(function() {
    //console.log('line1_toggle: ' + $(this).prop('checked'));
    console.log(line1_glyph);
  	// TODO programmatically grab the tippy, glyph, summary box, box row and col divs
    if ($(this).prop('checked')) {
      line1_editor_wrapper.style.opacity = "1";
    	line1_glyph.style.opacity = "1";
    	line1_summary_box.style.opacity = "1";
    	line1_summary_box_col.style.opacity = "1";
    	line1_summary_box_row.style.opacity = "1";
    	line1_tippy.enable();
    } else {
      line1_editor_wrapper.style.opacity = "0.25";
      line1_glyph.style.opacity = "0.25";
      line1_summary_box.style.opacity = "0";
      line1_summary_box_col.style.opacity = "0";
      line1_summary_box_row.style.opacity = "0";
    	line1_tippy.disable();
    }
  });

  // example of the toggle event listener
  $('input:checkbox').change(function(){
    toggle_id = $(this).attr('id')
    console.log("Event on ID: " + toggle_id);
  });

}

function send_toggle(message) {
  console.log("R received message from JS. " + message);
}

$(document).ready(function() {
  // The last verb will include the script that kicks everything off.
  console.log("ready!");
  // set up editors
  create_editors();
  // set up the sortablejs
  create_sortable();
  // set up the toggle listeners
  setup_toggles();
  // set up the toggle one way (JS to R) listener
  // the R side will update the dataframe output, and send info about box, summary box/row/col, and prompt
  $('#line1-toggle').change(function() {
    Shiny.setInputValue("datawat-toggle", $(this).prop('checked'));
  })

  // TODO set up the dialog prompt which requires info from R side

  Shiny.addCustomMessageHandler('toggle', function(message) {
    console.log("sending toggle a message!");
    send_toggle(message);
  });


});
