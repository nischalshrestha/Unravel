
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

function setup_toggles() {
  // set up the toggle event css listeners
  $('#data_toggle').change(function() {
    //console.log('data_toggle: ' + $(this).prop('checked'));
    editor_wrapper = editors["data"].getWrapperElement();
  	// TODO programmatically grab the tippy, glyph, summary box, box row and col divs
    if ($(this).prop('checked')) {
      editor_wrapper.style.opacity = "1";
    	/*group_by_tippy.enable();
      group_by_glyph.style.opacity = "1";
      group_by_summary_box.style.opacity = "1";
      group_by_summary_box_row.style.opacity = "1";
      group_by_summary_box_col.style.opacity = "1";*/
    } else {
      editor_wrapper.style.opacity = "0.25";
    	/*group_by_tippy.disable();
      group_by_verb_editor_wrapper.style.opacity = "0.25";
      group_by_glyph.style.opacity = "0.25";
      group_by_summary_box.style.opacity = "0";
      group_by_summary_box_row.style.opacity = "0";
      group_by_summary_box_col.style.opacity = "0";*/
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
  $('#data_toggle').change(function() {
    Shiny.setInputValue("datawat-data_toggle", $(this).prop('checked'));
    //console.log('data_toggle: ' + $(this).prop('checked'));
  })
  Shiny.addCustomMessageHandler('data_toggle', function(message) {
    console.log("sending data_toggle a message!");
    send_toggle(message);
  });

  // TODO set up the dialog prompt which requires info from R side

});
