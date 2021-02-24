library(shiny)

# TODO create a helper function that creates a div for a group item for SortableJS
# what are the unique identifiers?
# - Item ID: <id> for group item: data, verb1, verb2, ... verbn.
#   "verbN" is better for uniquely identifying since you can have multiple of same verb
# - Item Summary ID: <id>_summary_box, <id>_summary_box_row, <id>_summary_box_col
# - Item Box Type ID: <change>_square
# - Item Glyph ID: <id>_glyph
# - Item Toggle ID: <id>_toggle

datawatsUI <- function(id) {
  # namespace for module
  ns <- shiny::NS(id)
  shiny::fixedPage(
    # TODO: make sure these resources load in properly; you may have to have these in the tutorial folder
    shiny::tags$body(
      # bootstrap stuff
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/bootstrap.min.css")),
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/bootstrap4-toggle.min.css")),
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/bootstrap4-toggle.min.js")),
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/bootstrap3.min.css")),
      # codemirror stuff
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/codemirror.js")),
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/codemirror.css")),
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/r.js")),
      # fontawesome (for glyphicon for move)
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/all.css")),
      # Sortable.js
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/Sortable.js")),
      # custom css
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/style.css"))
    ),
    # TODO: this is now just hardcoded mockup but has to be generated UI from server side,
    # so you would basically have the simpleList div and an htmlOutput that includes a tagList
    # of the list-group-item divs
    shiny::fixedPage(
      id = "simpleList", class="list-group",
      # summary box 1
      div(class = "d-flex list-group-item", id="data", `data-id` = "0",
        # row div
        div(class = "justify-content-center align-self-baseline",
          div(class = "d-flex justify-content-center align-self-center",
            div(class = "row", style = "font-size:0.8em;",
              HTML("&nbsp;")
            )
          ),
          div(class = "data_summary_box_row d-flex empty-square justify-content-center",
            div(class="align-self-center", style="font-size: 0.8em;",
              # update element
              HTML("1.9M")
            )
          )
        ),
        # column div + square div
        div(class = "justify-content-center align-self-baseline",
          div(class = "data_summary_box_col d-flex justify-content-center align-self-center",
            div(class = "row", style = "font-size:0.8em;",
              # update element
              HTML("5")
            )
          ),
          # update element (class of square)
          div(class = "data_summary_box d-flex grey-square justify-content-center")
        ),
        # glyphicon
        div(class="data_glyph d-flex justify-content-center align-self-center",
            span(class="glyphicon glyphicon-move", style="opacity:1;")
        ),
        # codemirror empty div above
        div(class="d-flex justify-content-center align-self-center", style="padding:0.5em;",
            div(class="row", style="font-size: 1em;", HTML("&nbsp;"))
        ),
        # codemirror div (gets dynamically created); fixedPage keeps the width=100%
        fixedPage(
          shiny::htmlOutput(ns("code_mirror1"))
        ),
        # toggle checkbox
        div(style="opacity:1; padding-right:0.25em;",
            div(class="d-flex justify-content-center align-self-center",
                div(style="font-size: 0.8em;", HTML("&nbsp;"))
            ),
            # the value of `checked` is not meaningful, the existence of attribute turns on toggle by default
            shiny::tags$input(
              type = "checkbox",
              id = "data_toggle",
              `checked` = TRUE,
              `data-toggle`="toggle",
              `data-size`="xs",
              `data-height`="20", `data-width`="30",
              `data-on`=" ", `data-off`=" ",
              `data-style`="ios fast", `data-onstyle`="success", `data-offstyle`="secondary"
            )
        )
      ),
      # summary box 2... etc
      div(class = "d-flex list-group-item", id="group_by", `data-id` = "1",
        # row div
        div(class = "justify-content-center align-self-baseline",
          div(class = "d-flex justify-content-center align-self-center",
            div(class = "row", style = "font-size:0.8em;",
              HTML("&nbsp;")
            )
          ),
          div(class = "d-flex empty-square justify-content-center",
            div(class="align-self-center", style="font-size: 0.8em;",
              HTML("1.9M")
            )
          )
        ),
        # column div + square div
        div(class = "justify-content-center align-self-baseline",
          div(class = "d-flex justify-content-center align-self-center",
            div(class = "row", style = "font-size:0.8em;",
              HTML("5")
            )
          ),
          div(class = "d-flex blue-square justify-content-center")
        ),
        # glyphicon
        div(class="d-flex justify-content-center align-self-center",
            span(class="glyphicon glyphicon-move", style="opacity:1;")
        ),
        # codemirror empty div above
        div(class="d-flex justify-content-center align-self-center", style="padding:0.5em;",
            div(class="row", style="font-size: 1em;", HTML("&nbsp;"))
        ),
        # codemirror div (gets dynamically created); fixedPage keeps the width=100%
        fixedPage(
          shiny::htmlOutput(ns("code_mirror2"))
        ),
        # toggle checkbox
        div(style="opacity:1; padding-right:0.25em;",
            div(class="d-flex justify-content-center align-self-center",
                div(style="font-size: 0.8em;", HTML("&nbsp;"))
            ),
            shiny::tags$input(
              type = "checkbox",
              id = "group_by_toggle",
              `checked` = TRUE,
              `data-toggle`="toggle",
              `data-size`="xs",
              `data-height`="20", `data-width`="30",
              `data-on`=" ", `data-off`=" ",
              `data-style`="ios fast", `data-onstyle`="success", `data-offstyle`="secondary"
            )
        )
      )
    )
  )
}
# TODO df output area
# shiny::column(
#   12,
#   align = "center",
#   shiny::fluidRow(shiny::br()),
#   shiny::fluidRow(shiny::htmlOutput(ns("table")))
# )

datawatsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$code_mirror1 <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$textarea(
            "babynames %>%",
            class = "verb",
            id = "data"
          )
        )
      })

      # The last verb will include the script that kicks everything off.
      # TODO find a way to programmatically create each summary box
      # one idea is to create one giant tagList of textarea(s) elements
      # then in the end include the script.
      output$code_mirror2 <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$textarea(
            "group_by(year, sex) %>%",
            class = "verb",
            id = "group_by"
          ),
          shiny::includeScript(here::here("inst/tutorials/datawhats/js/script.js"))
        )
      })

      observeEvent(input$data_toggle, {
        # this lets us get the boolean value of the toggle from JS side!
        if (isTRUE(input$data_toggle)) {
          # TODO
          session$sendCustomMessage("data_toggle", "un-commenting line!")
        } else {
          session$sendCustomMessage("data_toggle", "commenting line!")
        }
      })

    }

  )
}

ui <- fluidPage(
  datawatsUI("datawat")
)

server <- function(input, output, session) {
  datawatsServer("datawat")
}

shinyApp(ui, server)
