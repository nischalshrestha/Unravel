
/*
Functionalities to set properties of the CodeMirror editor such as stepper arrow movement.
*/

var code = null;
var doc = null;

function makeMarker() {
  const marker = document.createElement('div');
  marker.className = 'fas fa-arrow-right fa-s';
  marker.style.paddingLeft = '2px'
  marker.style.color = 'red';
  return marker;
}

function set_stepper_arrow(line_number) {
  if (code != null && doc != null) {
    doc.clearGutter('current_line');
    lh = doc.getLineHandle(line_number);
    doc.setGutterMarker(lh, 'current_line', makeMarker());
  }
}

$(document).ready(function(){
  code = CodeMirror.fromTextArea(document.getElementsByClassName('code')[0], {
    value: document.getElementsByClassName('code')[0].innerHTML,
    mode: 'python',
    readOnly: 'nocursor',
    styleActiveLine: true,
    lineNumbers: true,
    gutters: ['current_line']
  });
  doc = code.getDoc();
  set_stepper_arrow(0);
});

