/* console.log(document.getElementsByClassName('data')[0]); */

var data = CodeMirror.fromTextArea(document.getElementsByClassName('data')[0], {
  mode: 'r',
  readOnly: 'nocursor',
  styleActiveLine: false,
  lineNumbers: false,
  gutters: ['data_line']
});
data.setSize(null, 30); // this done so that width is auto but we manually set height on a tidyverse line


var verb1 = CodeMirror.fromTextArea(document.getElementsByClassName('verb')[0], {
  mode: 'r',
  readOnly: 'nocursor',
  styleActiveLine: false,
  lineNumbers: false,
  gutters: ['verb1_line'],
  firstLineNumber: 2
});
verb1.setSize(null, 30);

var verb2 = CodeMirror.fromTextArea(document.getElementsByClassName('verb')[1], {
  mode: 'r',
  readOnly: 'nocursor',
  styleActiveLine: false,
  lineNumbers: false,
  gutters: ['verb2_line'],
  firstLineNumber: 3
});
verb2.setSize(null, 30);

var verb3 = CodeMirror.fromTextArea(document.getElementsByClassName('verb')[2], {
  mode: 'r',
  readOnly: 'nocursor',
  styleActiveLine: false,
  lineNumbers: false,
  gutters: ['verb2_line'],
  firstLineNumber: 3
});
verb3.setSize(null, 30);

var verb4 = CodeMirror.fromTextArea(document.getElementsByClassName('verb')[3], {
  mode: 'r',
  readOnly: 'nocursor',
  styleActiveLine: false,
  lineNumbers: false,
  gutters: ['verb2_line'],
  firstLineNumber: 3
});
verb4.setSize(null, 30);

function makeMarker() {
  const marker = document.createElement('span');
  marker.className = 'glyphicon glyphicon-move';
  marker.style.paddingLeft = '2px'
  marker.style.paddingRight = '0px'
  marker.style.color = 'grey';
  marker.style.cursor = 'move';
  marker.style.fontSize = '0.8em';
  marker.style.color = '#2c3745';
  marker.style.cursor = '-webkit-grabbing';
  marker.style.float = 'left'
  return marker;
}

/* doc = data.getDoc()
lh = doc.getLineHandle(0);
doc.setGutterMarker(lh, 'data_line', makeMarker()); */

/* doc = verb1.getDoc()
lh = doc.getLineHandle(0);
doc.setGutterMarker(lh, 'verb1_line', makeMarker()); */

/* doc = verb2.getDoc()
lh = doc.getLineHandle(0);
doc.setGutterMarker(lh, 'verb2_line', makeMarker());  */

Sortable.create(simpleList, {
  /* options */
});

