library(shiny)

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
      div(
        class = "d-flex list-group-item", `data-id` = "0",
        # row div
        div(
          class = "justify-content-center align-self-baseline",
          div(
            class = "d-flex justify-content-center align-self-center",
            div(
              class = "row", style = "font-size:0.8em;",
              HTML("&nbsp;")
            )
          ),
          div(
            class = "d-flex empty-square justify-content-center",
            div(
              class="align-self-center", style="font-size: 0.8em;",
              HTML("1.9M")
            )
          )
        ),
        # column div + square div
        div(
          class = "justify-content-center align-self-baseline",
          div(
            class = "d-flex justify-content-center align-self-center",
            div(
              class = "row", style = "font-size:0.8em;",
              HTML("5")
            )
          ),
          div(class = "d-flex grey-square justify-content-center")
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
          shiny::htmlOutput(ns("code_mirror1"))
        ),
        # toggle checkbox
        div(style="opacity:1; padding-right:0.25em;",
            div(class="d-flex justify-content-center align-self-center",
                div(style="font-size: 0.8em;", HTML("&nbsp;"))
            ),
            shiny::tags$input(
              type = "checkbox",
              `data-toggle`="toggle",
              `data-size`="xs",
              `data-height`="20", `data-width`="30",
              `data-on`=" ", `data-off`=" ",
              `data-style`="ios fast", `data-onstyle`="success", `data-offstyle`="secondary"
            )
        )
      ),
      # summary box 2... etc
      div(
        class = "d-flex list-group-item", id="group_by", `data-id` = "1",
        # row div
        div(
          class = "justify-content-center align-self-baseline",
          div(
            class = "d-flex justify-content-center align-self-center",
            div(
              class = "row", style = "font-size:0.8em;",
              HTML("&nbsp;")
            )
          ),
          div(
            class = "d-flex empty-square justify-content-center",
            div(
              class="align-self-center", style="font-size: 0.8em;",
              HTML("1.9M")
            )
          )
        ),
        # column div + square div
        div(
          class = "justify-content-center align-self-baseline",
          div(
            class = "d-flex justify-content-center align-self-center",
            div(
              class = "row", style = "font-size:0.8em;",
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
