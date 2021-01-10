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
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'initiator receiver callout_code'
  htmlNode.id = "A"  // this could be the name of the function / param / value to highlight
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )

  var snippet = "rename";
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'initiator receiver callout_code'
  htmlNode.id = "B"
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )

  var snippet = "columns";
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'initiator receiver callout_code'
  htmlNode.id = "C"
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )

  var snippet = "column_names";
  var lineNumber = 0;
  var charNumber = doc.getValue().indexOf(snippet);
  var htmlNode = document.createElement("span");
  htmlNode.innerHTML = snippet
  htmlNode.className = 'initiator receiver callout_code'
  htmlNode.id = "D"
  doc.markText(
  	{line: lineNumber, ch: charNumber},
    {line: lineNumber, ch: charNumber + snippet.length},
    {replacedWith: htmlNode}
  )
}

function setup_linker() {
  // TODO CLEAN UP THIS REDUNDANCY!!!!
  $(".initiator#A").hover(function(){
     $(".receiver#A").addClass('hover_text');
     console.log("A div was hovered");
  }, function(){
     $(".receiver#A").removeClass('hover_text');
  });

  $(".initiator#B").hover(function(){
     $(".receiver#B").addClass('hover_text');
     console.log("B div was hovered");
  }, function(){
     $(".receiver#B").removeClass('hover_text');
  });

   $(".initiator#C").hover(function(){
     $(".receiver#C").addClass('hover_text');
     console.log("C div was hovered");
  }, function(){
     $(".receiver#C").removeClass('hover_text');
  });

  $(".initiator#D").hover(function(){
     $(".receiver#D").addClass('hover_text');
     console.log("D div was hovered");
  }, function(){
     $(".receiver#D").removeClass('hover_text');
  });
}

$(document).ready(function(){
  setup_editor();
  setup_callouts();
  setup_linker();
});

