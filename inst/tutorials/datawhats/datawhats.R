library(shiny)

summary_button <- function(ns_id, inputId, lineid, change_type, value = 0) {
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
              return {lineid: $(el).attr('lineid'), value: parseInt($(el).text())};
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
                `lineid` = lineid,
                class = glue::glue("increment{inputId} d-flex {change_type}-square justify-content-center"),
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
group_item_div <- function(line, ns_id) {
  # browser()
  ns <- shiny::NS(ns_id)
  # line_id is for SortableJS and is just a number of the line
  line_id <- line$lineid
  line_code <- paste0(line$code, " %>%")
  if (line_id > 1) {
    line_code <- paste0("\t", line_code)
  }
  change_type <- line$change
  # line_summary <- line$summary
  # whereas id is for a readable identifier for JS/jquery/CSS
  id <- paste0("line", line$lineid)
  # TODO use ns to namespace the reactive outputs (elements that react to events)
  # to stay consistent with naming the - is used for html ids, _ is used if we need to refer to it
  # by R in Shiny
  div(class = "d-flex list-group-item", id=id, `data-id` = line_id,
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
        summary_button(ns_id, id, line_id, change_type)
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
        line_code,
        class = "verb",
        id = id,
        lineid = line_id
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
          `line-id` = line_id,
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
  ataglist <- lapply(lines, group_item_div, ns_id = ns_id)
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


datawatsServer <- function(id, summaries, outputs) {
  moduleServer(
    id,
    function(input, output, session) {

      # current line reactive value
      current <- reactiveVal(length(summary_outputs))

      # render a reactable of the current line output
      output$line_table <- reactable::renderReactable({
          value <- as.numeric(current())
          reactable::reactable(data = outputs[[value]],
                               compact = TRUE,
                               highlight = TRUE,
                               bordered = TRUE,
                               rownames = TRUE)
      })

      # this input is for JS to tell us we're ready to send summary info for data prompts
      observeEvent(input$ready, {
        message("ready: ", input$ready)
        session$sendCustomMessage("ready", summaries)
      })

      # this input tells us which data to display for a particular line
      observeEvent(input$square, {
        message("clicked on a square: ", input$square)
        current(input$square);
        session$sendCustomMessage("square", input$square)
      })

      # this input even tells us which line to (un)comment
      # TODO trigger a revaluation of the code of enabled lines
      observeEvent(input$toggle, {
        # this lets us get the boolean value of the toggle from JS side!
        if (isTRUE(input$toggle$checked)) {
          session$sendCustomMessage("toggle", paste0("un-commenting line ", input$toggle$lineid))
        } else {
          session$sendCustomMessage("toggle", paste0("commenting line ", input$toggle$lineid))
        }
      })
    }

  )
}

# example that works (save for row/col)
"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))" -> pipeline
quoted <- rlang::parse_expr(pipeline)
outputs <- get_dplyr_intermediates(quoted)
code_info <- lapply(outputs, function(x) list(lineid = x$line, code = x$code, change = x$change))
summaries <- lapply(outputs, function(x) list(lineid = paste0("line", as.character(x$line)), summary = x$summary))
outputs <- lapply(outputs, function(x) x$output)

ui <- fluidPage(
  # TODO create a structure that gives us all the info required to create the initial UI
  # For each line:
  # - id - D
  # - code
  # - the line data information:
  #   - data row and col dimensions
  #   - change type information (to inform the color of square and textual annotations)
  #   - data prompt
  datawatsUI("datawat", code_info)
)

server <- function(input, output, session) {
  datawatsServer("datawat", summaries, outputs)
}


shinyApp(ui, server)
