library(shiny)

shiny::tags$textarea(
"nba = (nba.rename(columns = column_names)
  .dropna(thresh = 4)
  [['date', 'away_team', 'away_points', 'home_team', 'home_points']]
  .assign(date = lambda x: pd.to_datetime(x['date'], format = '%a, %b %d, %Y'))
  .set_index('date', append = True)
  .rename_axis(['game_id', 'date'])
  .sort_index()
)",
  class = 'code'
) -> arrow_html

"
$(document).ready(function(){
  var code = CodeMirror.fromTextArea(document.getElementsByClassName('code')[0], {
    //value: document.getElementsByClassName('code')[0].innerHTML,
    mode: 'python',
    readOnly: 'nocursor',
    styleActiveLine: true,
    lineNumbers: true,
    gutters: ['current_line']
  });

  function makeMarker() {
    const marker = document.createElement('div');
    marker.className = 'fas fa-arrow-right fa-s';
    marker.style.paddingLeft = '2px'
    marker.style.color = 'red';
    return marker;
  }

  doc = code.getDoc()
  lh = doc.getLineHandle(0);
  doc.setGutterMarker(lh, 'current_line', makeMarker());
});" -> arrow_js

# // TODO now do this programmatically such that we change the line to set gutter marker for ^

# https://codemirror.net/lib/codemirror.js
# https://codemirror.net/lib/codemirror.css
# https://codemirror.net/mode/python/python.js (for python language)

ui <- fluidPage(
  shiny::tags$head(
    shiny::includeScript(here::here("R/js/codemirror.js")),
    shiny::includeCSS(here::here("R/css/codemirror.css")),
    shiny::includeScript(here::here("R/js/python.js")),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")
  ),
  fluidRow(shiny::htmlOutput("code"))
)

server <- function(input, output, session) {
  output$code <- renderUI({
    shiny::tagList(
      arrow_html,
      shiny::tags$script(
        shiny::HTML(arrow_js)
      )
    )
  })
}

shinyApp(ui, server)
