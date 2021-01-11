/*
Functionalities to set properties of the CodeMirror editor such as stepper arrow movement.
*/

/*
<textarea class="code">nba = (nba.rename(columns = column_names)
  .dropna(thresh = 4)
  [['date', 'away_team', 'away_points', 'home_team', 'home_points']]
  .assign(date = lambda x: pd.to_datetime(x['date'], format = '%a, %b %d, %Y'))
  .set_index('date', append = True)
  .rename_axis(['game_id', 'date'])
  .sort_index()
)</textarea>
*/

// Resources! Include these in html head:
// https://codemirror.net/lib/codemirror.js
// https://codemirror.net/lib/codemirror.css
// https://codemirror.net/mode/python/python.js (for python language)
// @import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css); (you can do this in your script stylesheet definition)

var code = null;
var doc = null;
var currentLine = 0;

function makeMarker() {
  const marker = document.createElement('div');
  marker.className = 'fas fa-arrow-right fa-s';
  marker.style.paddingLeft = '2px'
  marker.style.color = 'red';
  return marker;
}

function set_stepper_arrow(lineNumber) {
  if (code != null && doc != null) {
    doc.clearGutter('current_line');
    lh = doc.getLineHandle(lineNumber);
    doc.setGutterMarker(lh, 'current_line', makeMarker());
    currentLine = lineNumber;
  }
}

function setup_editor() {
  code = CodeMirror.fromTextArea(document.getElementsByClassName('code')[0], {
    mode: 'python',
    readOnly: 'nocursor',
    styleActiveLine: true,
    lineNumbers: true,
    gutters: ['current_line']
  });
  doc = code.getDoc();
  set_stepper_arrow(0);
}

$(document).ready(function(){
  setup_editor();
});

