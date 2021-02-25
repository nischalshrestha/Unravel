library(shiny)

summary_button <- function(ns_id, inputId, lineId, value = 0) {
  ns <- shiny::NS(ns_id)
  # TODO this is just a demonstration that we can make a custom input binding
  # we would need to create such a button for each summary box div
  # browser()
  tagList(
    singleton(
      tags$head(
        tags$script(glue::glue("
          $(document).on('click', 'button.increment{{inputId}}', function(evt) {

            // evt.target is the button that was clicked
            var el = $(evt.target);

            // Set the button's text to its current value plus 1
            el.text(parseInt(el.text()) + 1);

            // Raise an event to signal that the value changed
            el.trigger('change');
          });
          var incrementBinding{{inputId}} = new Shiny.InputBinding();
          $.extend(incrementBinding{{inputId}}, {
            find: function(scope) {
              return $(scope).find('.increment{{inputId}}');
            },
            getValue: function(el) {
              // send back the lineid (number of line), the value is ignored on R side
              return {lineId: $(el).attr('lineid'), value: parseInt($(el).text())};
            },
            setValue: function(el, value) {
              $(el).text(value);
            },
            subscribe: function(el, callback) {
              $(el).on('change.incrementBinding{{inputId}}', function(e) {
                callback();
              });
            },
            unsubscribe: function(el) {
              $(el).off('.incrementBinding{{inputId}}');
            }
          });
          Shiny.inputBindings.register(incrementBinding{{inputId}});

        ", .open = "{{", .close = "}}"))
      )
    ),
    tags$button(id = ns(inputId),
                `lineid` = lineId,
                class = glue::glue("increment{inputId} d-flex grey-square justify-content-center"),
                style = "color:transparent;",
                type = "button", as.character(value))
  )
}

#' A helper function that creates a div for a group item for SortableJS
#'
#' In particular, the unique identifiers make up each item:
#' - Item ID: <id> for group item: data, verb1, verb2, ... verbn.
#' "verbN" is better for uniquely identifying since you can have multiple of same verb
#' - Item Summary ID: <id>_summary_box, <id>_summary_box_row, <id>_summary_box_col
#' - Item Box Type ID: <change>_square
#' - Item Glyph ID: <id>_glyph
#' - Item Toggle ID: <id>_toggle
#' - Item CodeMirror ID: <id>_code_mirror
#'
#' @param id the character id for the particular group item
#' @param ns_id the character for the Shiny module namespace id
#'
#' @return a shiny::div
#' @export
#'
#' @examples
group_item_div <- function(id, ns_id, code_elements = NULL) {
  ns <- shiny::NS(ns_id)
  # data_id is for SortableJS and is just a number of the line
  data_id <- id
  # whereas id is for a readable identifier for JS/jquery/CSS
  id <- paste0("line", id)
  # TODO use ns to namespace the reactive outputs (elements that react to events)
  # to stay consistent with naming the - is used for html ids, _ is used if we need to refer to it
  # by R in Shiny
  div(class = "d-flex list-group-item", id=id, `data-id` = data_id,
    # row div
    div(class = "justify-content-center align-self-baseline",
        div(class = "d-flex justify-content-center align-self-center",
            div(class = "row", style = "font-size:0.8em;",
                HTML("&nbsp;")
            )
        ),
        div(class = glue::glue("{id}-summary-box-row d-flex empty-square justify-content-center"),
            div(class="align-self-center", style="font-size: 0.8em;",
                # update element
                HTML("1.9M")
            )
        )
    ),
    # column div + square div
    div(class = "justify-content-center align-self-baseline",
        div(class = glue::glue("{id}-summary-box-col d-flex justify-content-center align-self-center"),
            div(class = "row", style = "font-size:0.8em;",
                # update element
                HTML("5")
            )
        ),
        # update element (class of square)
        summary_button(ns_id, id, data_id)
    ),
    # glyphicon
    div(class=glue::glue("{id}-glyph d-flex justify-content-center align-self-center"),
        span(class="glyphicon glyphicon-move", style="opacity:1;")
    ),
    # codemirror empty div above
    div(class="d-flex justify-content-center align-self-center", style="padding:0.5em;",
        div(class="row", style="font-size: 1em;", HTML("&nbsp;"))
    ),
    # codemirror div (gets dynamically created); fixedPage keeps the width=100%
    fixedPage(
      shiny::tags$textarea(
        # TODO get verb text (dynamically received from argument)
        "babynames %>%",
        class = "verb",
        id = id
      )
    ),
    # toggle checkbox
    div(style="opacity:1; padding-right:0.25em;",
        div(class="d-flex justify-content-center align-self-center",
            div(style="font-size: 0.8em;", HTML("&nbsp;"))
        ),
        # the value of `checked` is not meaningful, the existence of attribute turns on toggle by default
        shiny::tags$input(
          type = "checkbox",
          id = glue::glue("{id}-toggle"),
          `toggle-id` = id,
          `checked` = TRUE,
          `data-toggle`="toggle",
          `data-size`="xs",
          `data-height`="20", `data-width`="30",
          `data-on`=" ", `data-off`=" ",
          `data-style`="ios fast", `data-onstyle`="success", `data-offstyle`="secondary"
        )
    )
  )
}

create_group_tags <- function(lines, ns_id) {
  ataglist <- lapply(seq_len(length(lines)), group_item_div, ns_id = ns_id)
  class(ataglist) <- c("shiny.tag.list", "list")
  return(ataglist)
}

datawatsUI <- function(id, lines) {
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
      # tippy
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/popper.min.js")),
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/tippy-bundle.min.js")),
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/light.css")),
      # custom css
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/style.css"))
    ),
    # TODO: this is now just hardcoded mockup but has to be generated UI from server side,
    # so you would basically have the simpleList div and an htmlOutput that includes a tagList
    # of the list-group-item divs
    shiny::fixedPage(id = "simpleList", class="list-group",
      # summary box 1
      create_group_tags(lines, id),
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/script.js"))
    ),
    shiny::fixedPage(class="list-group",
      reactable::reactableOutput(ns("line_table"))
    )
  )
}

datawatsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      current <- reactiveVal(1)

      # demo of render reactable
      output$line_table <- reactable::renderReactable({
          value <- as.numeric(current())
          # TODO according to current() line be able to
          # get the intermediate dataframe and render it.
          reactable::reactable(data = mtcars[1:value, ],
                               compact = TRUE,
                               highlight = TRUE,
                               bordered = TRUE,
                               rownames = TRUE)
      })


      # TODO While this works in getting the lines, we cannot create these programmatically
      # it's worth exploring the JS -> R route as well.
      observeEvent(input$line1, {
        message(input$line1$lineId)
        current(input$line1$lineId)
      })

      observeEvent(input$line2, {
        message(input$line2$lineId)
        current(input$line2$lineId)
      })

      # TODO change this input id to be more general and we can grab value out to
      # know which line to comment/uncomment
      observeEvent(input$toggle, {
        # this lets us get the boolean value of the toggle from JS side!
        if (isTRUE(input$toggle)) {
          session$sendCustomMessage("toggle", "un-commenting line!")
        } else {
          session$sendCustomMessage("toggle", "commenting line!")
        }
      })
    }

  )
}

ui <- fluidPage(
  # TODO create a structure that gives us all the info required to create the initial UI
  # For each line:
  # - id - D
  # - code
  # - the line data information:
  #   - data row and col dimensions
  #   - change type information (to inform the color of square and textual annotations)
  #   - data prompt
  datawatsUI("datawat", seq_len(1))
)

server <- function(input, output, session) {
  datawatsServer("datawat")
}

shinyApp(ui, server)
