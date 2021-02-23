
/*
var data = CodeMirror.fromTextArea($('#data')[0], {
  mode: 'r',
  readOnly: 'nocursor',
  styleActiveLine: false,
  lineNumbers: false,
  firstLineNumber: 1
});
data.setSize(null, 50);
*/

var editors = [];
var sortable = null;

function create_editors() {
  $(".verb").each(function(index, element){
  	console.log(index, element.id);
  	if (editors.indexOf(element.id) == -1) {
      let verb_editor = CodeMirror.fromTextArea(element, {
        mode: 'r',
        readOnly: 'nocursor',
        styleActiveLine: false,
        lineNumbers: false,
        firstLineNumber: 1
      });
      verb_editor.setSize(null, 50);
      editors.push(element.id);
  	}
  });
}

function create_sortable() {
  sortable = Sortable.create(simpleList, { });
}

$(document).ready(function() {
  // The last verb will include the script that kicks everything off.
  console.log("ready!");
  // set up editors
  create_editors();
  // set up the sortablejs
  create_sortable();
});
