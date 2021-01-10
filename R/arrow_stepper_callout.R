library(shiny)

"nba = (nba.rename(columns = column_names)
  .dropna(thresh = 4)
  [['date', 'away_team', 'away_points', 'home_team', 'home_points']]
  .assign(date = lambda x: pd.to_datetime(x['date'], format = '%a, %b %d, %Y'))
  .set_index('date', append = True)
  .rename_axis(['game_id', 'date'])
  .sort_index()
)" -> code_text

arrow_html <-
  shiny::tags$textarea(
    code_text,
    class = 'code'
  )

# spec for how the callouts are related:
# class for both code / text callout has to be <span class = "initiator receiver callout_[text|code]" id = "foo"></span>
# where callout_text is for the explanation, and
# where callout_code is for the code (bc we underline in this case instead of bubble), and
# the identifier that links the two for a given pair is `id`

# reasoning:
# this enables 2-way highlight on hover
# and clean way to link two related html spans using id

summary_html <-
  shiny::div(
    shiny::p(
      "We create a variable ",
      shiny::span("nba", class="initiator receiver callout_text", id = "A", .noWS = "outside"),
      " to store the final DataFrame. First, we ",
      shiny::span("rename", class="initiator receiver callout_text", id = "B", .noWS = "outside"),
      " the original ",
      shiny::span("columns", class="initiator receiver callout_text", id = "C", .noWS = "outside"),
      " by supplying the ",
      shiny::span("column_names", class="initiator receiver callout_text", id = "D", .noWS = "outside"),
      " dictionary",
      .noWS = "inside"
    ),
    class = "summary"
  )

# // TODO now do this programmatically such that we change the line to set gutter marker for ^

# https://codemirror.net/lib/codemirror.js
# https://codemirror.net/lib/codemirror.css
# https://codemirror.net/mode/python/python.js (for python language)

ui <- fluidPage(
  shiny::tags$head(
    shiny::includeScript(here::here("R/js/codemirror.js")),
    shiny::includeCSS(here::here("R/css/codemirror.css")),
    shiny::includeCSS(here::here("R/css/callout.css")),
    shiny::includeScript(here::here("R/js/python.js")),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    tags$script("
      Shiny.addCustomMessageHandler('step', function(number) {
        set_stepper_arrow(number);
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

  output$code <- renderUI({
    shiny::tagList(
      arrow_html,
      shiny::includeScript(here::here("R/js/stepper.js"))
    )
  })

  output$summary <- shiny::renderUI({
    shiny::tagList(
      summary_html
    )
  })

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
}

shinyApp(ui, server)

