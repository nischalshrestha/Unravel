library(shiny)
#
# code_summary(
#   "We create a variable ", callout_text("nba"), " to store the final DataFrame. First, we ",
#   callout_text("rename"), " the original ", callout_text("columns"), " by supplying the ",
#   callout_text("column_names"), " dictionary"
# )

# write a function such that:
# - for each argument passed in check if it's a character or a callout object (inherits)
# - if it is a character, simply add to the final list
# - if it is a callout object, collect the string into a regex list for the code part, then add to list
# - after iterating through all arguments, construct the div(p(...), class = "summary")

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
    shiny::includeCSS(here::here("R/css/callout.css")),
    tags$script("
      Shiny.addCustomMessageHandler('step', function(lineNumber) {
        set_stepper_arrow(lineNumber);
      });
    ")
  ),
  column(
    12,
    shiny::htmlOutput("code")
  ),
  shiny::br(),
  shiny::column(
    12,
    shiny::div(
      style = "height: auto;",
      shiny::htmlOutput("summary")
    )
  ),
  column(12,
    align = "center",
    shiny::actionButton("firstBtn", label = "First", icon = shiny::icon("fast-backward")),
    shiny::actionButton("previousBtn", label = "Previous", icon = shiny::icon("arrow-left")),
    shiny::actionButton("nextBtn", label = "Next", icon = shiny::icon("arrow-right")),
    shiny::actionButton("lastBtn", label = "Last", icon = shiny::icon("fast-forward"))
  )
)

server <- function(input, output, session) {
  current_line <- reactiveVal(0)

  # setup code html
"nba = (nba.rename(columns = column_names)
  .dropna(thresh = 4)
  [['date', 'away_team', 'away_points', 'home_team', 'home_points']]
  .assign(date = lambda x: pd.to_datetime(x['date'], format = '%a, %b %d, %Y'))
  .set_index('date', append = True)
  .rename_axis(['game_id', 'date'])
  .sort_index()
)" -> code_text

  # TODO mimic this cadence in stepper function for `explain`
  summary_info <- list(
    code_summary(
      "We create a variable ", callout_text("nba"), " to store the final DataFrame. First, we ",
      callout_text("rename"), " the original ", callout_text("columns"), " by supplying the ",
      callout_text("column_names"), " dictionary"
    )
  )

  observeEvent(input$callout, {
    message("woah callout")
    session$sendCustomMessage("setupCallouts", summary_info[[current_line() + 1]]$callout_words)
  })

  observeEvent(input$linker, {
    message("woah linker")
    session$sendCustomMessage("setupLinker", summary_info[[current_line() + 1]]$callout_words)
  })

  output$code <- renderUI({
    shiny::tagList(
      shiny::tags$textarea(
        code_text,
        class = 'code'
      ),
      shiny::includeScript(here::here("R/js/stepper.js"))
    )
  })

  output$summary <- shiny::renderUI({
    shiny::tagList(
      summary_info[[current_line() + 1]]$html,
      shiny::includeScript(here::here("R/js/callouts.js")),
      shiny::includeScript(here::here("R/js/linker.js"))
    )
  })

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
}

shinyApp(ui, server)

