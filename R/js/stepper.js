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

function setup_callouts() {
  // TODO CLEAN UP THIS REDUNDANCY!!!!
  var snippet = "nba";
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  //console.log(snippet.indexOf("nba"));
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'callout_nba'
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )

  var snippet = "rename";
  //console.log(doc.getValue().indexOf(snippet));
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  //console.log(snippet.indexOf("nba"));
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'callout_rename'
  //htmlNode.style = "background-color: #ffff7f";
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )

  var snippet = "columns";
  //console.log(doc.getValue().indexOf(snippet));
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  //console.log(snippet.indexOf("nba"));
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'callout_columns'
  //htmlNode.style = "background-color: #ffff7f";
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )

  var snippet = "column_names";
  //console.log(doc.getValue().indexOf(snippet));
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  //console.log(snippet.indexOf("nba"));
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'callout_column_names'
  //htmlNode.style = "background-color: #ffff7f";
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )
}

function setup_linker() {
  // TODO CLEAN UP THIS REDUNDANCY!!!!
  $('.callout_text#A').hover(function(event) {
    $('.callout_nba').toggleClass('hover_yellow');
    console.log('callout_nba div was hovered');
  });
  $('.callout_text#B').hover(function(event) {
    $('.callout_rename').toggleClass('hover_yellow');
    console.log('callout_rename div was hovered');
  });
  $('.callout_text#C').hover(function(event) {
    $('.callout_columns').toggleClass('hover_yellow');
    console.log('callout_columns div was hovered');
  });
  $('.callout_text#D').hover(function(event) {
    $('.callout_column_names').toggleClass('hover_yellow');
    console.log('callout_column_names div was hovered');
  });
}

$(document).ready(function(){
  setup_editor();
  setup_callouts();
  setup_linker();
});
