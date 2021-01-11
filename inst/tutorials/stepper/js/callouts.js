
function setup_callouts(calloutWords) {
  console.log("setup_callouts");
  calloutWords.forEach(function(c) {
    console.log(c);
    var snippet = c;
    var charNumber = doc.getValue().indexOf(snippet);
    var htmlNode = document.createElement("span");
    htmlNode.innerHTML = snippet
    htmlNode.className = 'initiator receiver callout_code'
    htmlNode.id = snippet  // this could be the name of the function / param / value to highlight
    doc.markText(
    	{line: currentLine, ch: charNumber},
      {line: currentLine, ch: charNumber + snippet.length},
      {replacedWith: htmlNode}
    )
  });
}

$(document).ready(function(){
  // TODO figure out how to get namespace id before this
  //Shiny.setInputValue(namespaceID.concat("-callout"), "ready");
  Shiny.setInputValue("nba_stepper-callout", "ready");
  Shiny.addCustomMessageHandler('setupCallouts', function(calloutWords) {
    console.log("setting up callouts");
    setup_callouts(calloutWords);
  });
});
