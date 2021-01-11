
function setup_linker(calloutWords) {
  calloutWords.forEach(function(c){
    $(".initiator#".concat(c)).hover(function(){
       $(".receiver#".concat(c)).addClass('hover_text');
       console.log(c.concat(" div was hovered"));
    }, function(){
       $(".receiver#".concat(c)).removeClass('hover_text');
    });
  });
}

$(document).ready(function(){
  Shiny.setInputValue("linker", "ready");
  Shiny.addCustomMessageHandler('setupLinker', function(calloutWords) {
    console.log("setting up callout links");
    setup_linker(calloutWords);
  });
});
