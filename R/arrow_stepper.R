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

# // TODO now do this programmatically such that we change the line to set gutter marker for ^

# https://codemirror.net/lib/codemirror.js
# https://codemirror.net/lib/codemirror.css
# https://codemirror.net/mode/python/python.js (for python language)

ui <- fluidPage(
  shiny::tags$head(
    shiny::includeScript(here::here("R/js/codemirror.js")),
    shiny::includeCSS(here::here("R/css/codemirror.css")),
    shiny::includeScript(here::here("R/js/python.js")),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    tags$script("
      Shiny.addCustomMessageHandler('step', function(number) {
        set_stepper_arrow(number);
      });
    ")
  ),
  fluidRow(shiny::htmlOutput("code")),
  column(12,
    shiny::actionButton("firstBtn", label = "First", icon = shiny::icon("fast-backward")),
    shiny::actionButton("previousBtn", label = "Previous", icon = shiny::icon("arrow-left")),
    shiny::actionButton("nextBtn", label = "Next", icon = shiny::icon("arrow-right")),
    shiny::actionButton("lastBtn", label = "Last", icon = shiny::icon("fast-forward"))
  )
)

server <- function(input, output, session) {

  current_line <- reactiveVal(0)

  # TODO: you would need to retrieve code text programmatically in real stepper

  observeEvent(input$firstBtn, {
    # set first line marker
    current_line(0)
    session$sendCustomMessage("step", current_line())
  })

  observeEvent(input$previousBtn, {
    # set previous line marker
    if (current_line() >= 1) {
     current_line(current_line() - 1)
    }
    session$sendCustomMessage("step", current_line())
  })

  observeEvent(input$nextBtn, {
    # set next line marker
    # TODO: hardcoded length for example but need to be according to snippet
    if (current_line() < 7) {
      current_line(current_line() + 1)
    }
    session$sendCustomMessage("step", current_line())
  })

  observeEvent(input$lastBtn, {
    # set last line marker
    current_line(7)
    session$sendCustomMessage("step", current_line())
  })

  output$code <- renderUI({
    shiny::tagList(
      arrow_html,
      shiny::includeScript(here::here("R/js/stepper.js"))
    )
  })
}

shinyApp(ui, server)
