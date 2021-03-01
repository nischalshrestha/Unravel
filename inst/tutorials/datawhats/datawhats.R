library(DataTutor)
library(shiny)
library(tidyverse)
library(babynames)

summary_button <- function(ns_id, inputId, lineid, change_type, value = 0) {
  ns <- shiny::NS(ns_id)
  tags$button(id = ns(inputId),
              `lineid` = lineid,
              class = glue::glue("d-flex {change_type}-square noSelect justify-content-center"),
              style = "color:transparent; cursor:pointer;",
              type = "button", as.character(value))
}

#' A helper function that creates a div for a group item for SortableJS
#'
#' In particular, the unique identifiers make up each item:
#' - Item ID: <id> for group item: data, verb1, verb2, ... verbn.
#' "verbN" is better for uniquely identifying since you can have multiple of same verb
#' - Item Summary ID: <id>-summary-box, <id>-summary-box-row, <id>-summary-box-col
#' - Item Box Type ID: <change>-square
#' - Item Glyph ID: <id>-glyph
#' - Item Toggle ID: <id>-toggle
#' - Item CodeMirror ID: <id>-code_mirror
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
  # TODO this strategy of adding pipe won't work because we need to know if the line is the last one or not, we could
  # preprocess that info ahead of time
  line_code <- line$code
  change_type <- line$change
  row <- line$row
  col <- line$col
  # whereas id is for a readable identifier for JS/jquery/CSS
  id <- paste0("line", line$lineid)
  div(class = "d-flex list-group-item", id=id, `data-id` = line_id,
    # row div
    div(class = "justify-content-center align-self-baseline",
        div(class = "d-flex justify-content-center align-self-center",
            div(class = "row", style = "font-size:0.8em;",
                HTML("&nbsp;")
            )
        ),
        div(class = glue::glue("{id}-summary-box-row d-flex empty-square justify-content-center"),
            div(class=glue::glue("{id}-row-content align-self-center"), style="font-size: 0.8em;",
                # update element
                HTML(row)
            )
        )
    ),
    # column div + square div
    div(class = "justify-content-center align-self-baseline",
        div(class = glue::glue("{id}-summary-box-col d-flex justify-content-center align-self-center"),
            div(class = glue::glue("{id}-col-content row"), style = "font-size:0.8em;",
                # update element
                HTML(col)
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
    shiny::fixedPage(
      shiny::tags$textarea(
        shiny::HTML(line_code),
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

# helper function to create a group item div for SortableJS
create_group_item_tags <- function(lines, ns_id) {
  ataglist <- lapply(lines, group_item_div, ns_id = ns_id)
  class(ataglist) <- c("shiny.tag.list", "list")
  return(ataglist)
}

datawatsUI <- function(id) {
  # TODO this is a dummy pipeline just so we have an example to run when app starts
"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))" -> default_pipeline
# "babynames %>%
#    group_by(year, sex) %>%
#    summarise(total = sum(n)) %>%
#    spread(sex, total) %>%
#    mutate(percent_male = M / (M + F) * 100, ratio = M / F)" -> default_pipeline
  # namespace for module
  ns <- shiny::NS(id)
  shiny::fixedPage(
    # TODO: make sure these resources load in properly; you may have to have these in the tutorial folder
    shiny::tags$body(
      # bootstrap stuff
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/bootstrap.min.css")),
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
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/style.css")),
      # custom js for exploration of code
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/explorer.js")),
      # tippy
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/popper.min.js")),
      shiny::includeScript(here::here("inst/tutorials/datawhats/js/tippy-bundle.min.js")),
      shiny::includeCSS(here::here("inst/tutorials/datawhats/css/light.css"))
    ),
    shiny::br(),
    shiny::br(),
    div(class = "d-flex justify-content-center align-self-center",
        shiny::column(
        10,
        shiny::h4("Enter dplyr code:"),
        # shiny::h3("Enter dplyr code to explore:", class = "d-flex justify-content-center"),
        shiny::tags$textarea(
          # TODO get verb text (dynamically received from argument)
          class = "code_input",
          id = id,
          default_pipeline
        ),
        shiny::includeScript(here::here("inst/tutorials/datawhats/js/script.js"))
      )
    ),
    shiny::br(),
    div(class = "d-flex justify-content-center align-self-center",
      shiny::actionButton(inputId = ns("explore"), label = "Unravel", icon = shiny::icon("fas fa-layer-group"))
    ),
    shiny::htmlOutput(ns("code_explorer")),
    shiny::fixedPage(class="list-group",
      reactable::reactableOutput(ns("line_table"))
    )
  )
}

datawatsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # current line reactive value
      current <- reactiveVal(0)

      # these are reactive values related to code editors info, summary prompts, and df outputs
      rv <- reactiveValues()
      rv$code_info <-  NULL
      rv$summaries <- list()
      rv$outputs <- NULL

      # listen to button click and signal JS to give us code back from input editor
      observeEvent(input$explore, {
        message("Explore button")
        session$sendCustomMessage("need_code", "R needs the code!")
      })

      # listen for JS to tell us code is ready for us to be processed
      observeEvent(input$code_ready, {
        message("Receiving code from JS: ", input$code_ready)

        # TODO process lines function?
        # process lines
        if (!is.null(input$code_ready) && length(input$code_ready) > 0) {
          quoted <- rlang::parse_expr(input$code_ready)
          message(quoted)
          outputs <- get_dplyr_intermediates(quoted)
          # set reactive values
          rv$code_info <- lapply(outputs, function(x) {
            list(lineid = x$line, code = x$code, change = x$change, row = abbrev_num(x$row), col = abbrev_num(x$col))
          })
          rv$current_code_info <- lapply(outputs, function(x) {
            list(lineid = x$line, code = x$code, change = x$change, row = abbrev_num(x$row), col = abbrev_num(x$col), checked = TRUE)
          })
          rv$summaries <- lapply(outputs, function(x) list(lineid = paste0("line", x$line), summary = x$summary))
          rv$outputs <- lapply(outputs, function(x) list(lineid = paste0("line", x$line), output = x$output))
          # trigger data frame output of the very last line
          current(length(rv$outputs))
        }
      });

      # the observer for the code explorer which will get rendered once we have code information
      output$code_explorer <- renderUI({
        if (!is.null(rv$code_info)) {
          shiny::tagList(
            shiny::fixedPage(id = "simpleList", class="list-group",
              create_group_item_tags(rv$code_info, id),
              shiny::tags$script("setup_editors();"),
              shiny::tags$script("setup_sortable();"),
              # toggle
              shiny::includeCSS(here::here("inst/tutorials/datawhats/css/bootstrap4-toggle.min.css")),
              shiny::includeScript(here::here("inst/tutorials/datawhats/js/bootstrap4-toggle.min.js")),
              shiny::tags$script("setup_toggles();"),
              shiny::tags$script("setup_box_listeners();"),
              shiny::br(),
              # TODO if we want we could also add prompts to the data change scheme color
              shiny::div(class = "d-flex align-self-center", style = "margin-left: 8em;",
                         div(class = glue::glue("d-flex none-square-key justify-content-center")),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 1em; font-size: 0.8em; width: 80px;", "No change"),
                         div(class = glue::glue("d-flex internal-square-key justify-content-center")),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 1em; font-size: 0.8em; width: 100px;", "Internal change"),
                         div(class = glue::glue("d-flex visible-square-key justify-content-center")),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 1em; font-size: 0.8em; width: 100px;", "Visible change"),
                         div(class = glue::glue("d-flex error-square-key justify-content-left")),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 0.5em; font-size: 0.8em; width: 100px;", "Error"),
              ),
              shiny::br()
            )
          )
        }
      })

      # list for a trigger message input from JS input so we can send summary info for data prompts
      observeEvent(input$ready, {
        message("ready: ", input$ready)
        session$sendCustomMessage("prompts", rv$summaries)
      })

      # list for a click square input from JS to tells us which data to display for a particular line
      observeEvent(input$square, {
        message("clicked on a square: ", input$square)
        # make sure to only change current if the current code info has the line marked as enabled
        any_output <- Filter(function(x) { x$lineid == input$square && x$checked }, rv$current_code_info)
        # message(length(any_output))
        if (length(any_output) > 0) {
          current(input$square);
          session$sendCustomMessage("square", input$square)
        }
      })

      # render a reactable of the current line output
      output$line_table <- reactable::renderReactable({
        value <- as.numeric(current())
        if (value > 0) {
          # reactable can only efficiently display data of a certain size
          # if we enter into the 100K range, it starts to slow down
          message("changed data line output ", value)
          final_data <- rv$outputs[[value]]$output
          if (!is.null(final_data) && length(final_data) >= 1) {
            if (dim(final_data)[[1]] > 5e5) {
              final_data <- final_data[1:5e4, ]
            }
            reactable::reactable(data = final_data,
                                 compact = TRUE,
                                 highlight = TRUE,
                                 bordered = TRUE)
          }
        }
      })

      # this input even tells us which line to (un)comment
      # TODO trigger a revaluation of the code of enabled lines
      observeEvent(input$toggle, {
        message("TOGGLE", input$toggle)
        # this lets us get the boolean value of the toggle from JS side!
        message(typeof(input$toggle$lineid))
        if (isTRUE(input$toggle$checked)) {
          session$sendCustomMessage("toggle", paste0("un-commenting line ", input$toggle$lineid))
        } else {
          session$sendCustomMessage("toggle", paste0("commenting line ", input$toggle$lineid))
        }
        line_id <- as.numeric(input$toggle$lineid)
        checked <- input$toggle$checked

        # update the current code info with the checked flag
        rv$current_code_info <- lapply(rv$current_code_info, function(x) {
          if (x$lineid == line_id) {
            x$checked <- checked
          }
          x
        })

        # grab just the code info that are enabled
        new_code_info <- Filter(
          function(x) {
            return(isTRUE(x$checked))
          },
          rv$current_code_info
        )

        # get new code
        new_code_info <- lapply(seq_len(length(new_code_info)), function(i) {
          # update %>%> if it's either the only line or the last line
          if (i == length(new_code_info) || length(new_code_info) == 1) {
            new_code_info[[i]]$code <- unlist(strsplit(new_code_info[[i]]$code, split = "%>%"))
          }
          new_code_info[[i]]
        })
        new_code_source <- paste0(lapply(new_code_info, function(x) x$code), collapse = "\n")
        quoted <- rlang::parse_expr(new_code_source)

        # get new code intermediate info
        outputs <- get_dplyr_intermediates(quoted)
        # update the default lineid
        outputs <- lapply(seq_len(length(outputs)), function(i) {
          outputs[[i]]$line <- new_code_info[[i]]$lineid
          outputs[[i]]
        })

        # helper function for grabbing a particular new code
        get_code <- function(target, id) {
          # browser()
          result <- Filter(function(x) x$lineid == id, target)
          if (length(result) > 0) {
            return(result[[1]]$code)
          } else {
            return(result)
          }
        }

        # helper function for grabbing a particular new output
        get_output <- function(target, lineid) {
          Filter(function(x) x$line == lineid, target)
        }

        # prep data to send JS about box change type, row, and col
        new_rv_code_info <- lapply(rv$code_info, function(x) {
          out <- get_output(outputs, x$lineid)
          # error occurred before this line
          if (length(out) == 0) {
            # reset states
            x$change = "invisible"
            x$row = ""
            x$col = ""
            x$summary = ""
            x$output = NULL
          } else {
            # no error before this line
            new_rv <- out[[1]]
            # check if we need to update the code text for one of the editors regarding %>%
            new_code <- get_code(new_code_info, x$lineid)
            # check for error for this line
            if (!is.null(new_rv$err)) {
              # set error states
              x$code = new_code
              x$change = new_rv$change
              x$row = abbrev_num(new_rv$row)
              x$col = abbrev_num(new_rv$col)
              x$summary = new_rv$err
              x$output = NULL
            } else {
              # if no error, update states
              x$code = new_code
              x$change = new_rv$change
              x$row = abbrev_num(new_rv$row)
              x$col = abbrev_num(new_rv$col)
              x$summary = new_rv$summary
              x$output = new_rv$output
            }
          }
          x
        })
        send_js_code_info <- lapply(new_rv_code_info, function(x) {
          list(id = x$lineid, code = x$code, change = x$change, row = x$row, col = x$col)
        })
        # send JS the new summary box, row and col
        session$sendCustomMessage("update_line", send_js_code_info)

        # update the summaries and outputs as well
        rv$summaries <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), summary = x$summary))
        # send JS the new prompts
        session$sendCustomMessage("update_prompts", rv$summaries)
        rv$outputs <- lapply(new_rv_code_info, function(x) list(id = x$line, lineid = paste0("line", x$line), output = x$output))
        # update the data display to the last enabled output
        current(tail(outputs, 1)[[1]]$line)
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
