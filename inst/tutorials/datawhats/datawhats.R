library(DataTutor)
library(shiny)

#' Creates a summary button
#'
#' @param ns_id
#' @param inputId
#' @param lineid
#' @param change_type
#' @param value
#'
#' @return
#' @export
#'
#' @examples
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


#' Helper function to create a group item div for SortableJS
#'
#' @param lines
#' @param ns_id
#'
#' @return
#' @export
#'
#' @examples
create_group_item_tags <- function(lines, ns_id) {
  ataglist <- lapply(lines, group_item_div, ns_id = ns_id)
  class(ataglist) <- c("shiny.tag.list", "list")
  return(ataglist)
}

#' Datawats UI
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
datawatsUI <- function(id) {
  # TODO this is a dummy pipeline just so we have an example to run when app starts
"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))" -> default_pipeline
# "babynames" -> default_pipeline
  # namespace for module
  ns <- shiny::NS(id)
  shiny::fixedPage(
    # TODO: make sure these resources load in properly; you may have to have these in the tutorial folder
    shiny::tags$body(
      # bootstrap stuff
      shiny::includeCSS("css/bootstrap.min.css"),
      shiny::includeCSS("css/bootstrap3.min.css"),
      # codemirror stuff
      shiny::includeScript("js/codemirror.js"),
      shiny::includeCSS("css/codemirror.css"),
      shiny::includeScript("js/r.js"),
      # fontawesome (for glyphicon for move)
      shiny::includeCSS("css/all.css"),
      # Sortable.js
      shiny::includeScript("js/Sortable.js"),
      # custom css
      shiny::includeCSS("css/style.css"),
      # custom js for exploration of code
      shiny::includeScript("js/explorer.js"),
      # tippy
      shiny::includeScript("js/popper.min.js"),
      shiny::includeScript("js/tippy-bundle.min.js"),
      shiny::includeCSS("css/light.css"),
    ),
    shiny::br(),
    shiny::br(),
    div(class = "d-flex justify-content-center align-self-center",
        shiny::column(
        10,
        shiny::h4("Enter dplyr code:"),
        shiny::tags$textarea(
          class = "code_input",
          id = id,
          default_pipeline
        ),
        shiny::includeScript("js/script.js")
      )
    ),
    shiny::br(),
    div(class = "d-flex justify-content-center align-self-center",
      shiny::actionButton(inputId = ns("explore"), label = "Unravel", icon = shiny::icon("fas fa-layer-group"),style = "margin: 1em;"),
      shiny::actionButton(
        inputId = ns("feedback"),
        label = "Please click to provide us feedback!",
        icon = shiny::icon("fas fa-clipboard"),
        onclick = "window.open('https://bit.ly/2PsA7w9', '_blank')",
        style = "margin: 1em;"
      )
    ),
    shiny::htmlOutput(ns("code_explorer")),
    shiny::fixedPage(class="list-group",
      reactable::reactableOutput(ns("line_table"))
    )
  )
}

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

#' Given line order, and the reactive values, generate
#'
#' @param order
#' @param rv
#'
#' @return
#' @export
#'
#' @examples
generate_code_info_outputs <- function(order, rv) {
  new_code_info <- rv$current_code_info[order]
  # only grab the enabled lines
  new_code_info <- Filter(
    function(x) {
      return(isTRUE(x$checked))
    },
    new_code_info
  )

  # if no lines are enabled return early
  if (length(new_code_info) == 0) {
    rv$outputs <- list()
    return(NULL)
  }

  # get new code
  new_code_info <- lapply(seq_len(length(new_code_info)), function(i) {
    if (i < length(new_code_info) && !grepl("%>%", new_code_info[[i]]$code)) {
      # if in between lines and it doesn't have pipes, add it
      new_code_info[[i]]$code <- paste(new_code_info[[i]]$code, "%>%")
    } else if (i == length(new_code_info) && grepl("%>%", new_code_info[[i]]$code)) {
      # if last line and it contains pipe, remove it
      new_code_info[[i]]$code <- unlist(strsplit(new_code_info[[i]]$code, split = "%>%"))
    }
    new_code_info[[i]]
  })
  new_code_source <- paste0(lapply(new_code_info, function(x) x$code), collapse = "\n")
  quoted <- rlang::parse_expr(new_code_source)
  message("quoted code:")
  str(quoted)

  # get new code intermediate info
  outputs <- get_dplyr_intermediates(quoted)
  # we might run across an error while processing an invalid pipeline, for
  # this case we will get back the error message, a character, so we do this
  # silly check for now so that we return early and the UI does not know any better
  # it will just show the last error'd line
  if (!inherits(outputs, "list")) {
    return(NULL)
  }

  # update the default lineid
  outputs <- lapply(seq_len(length(outputs)), function(i) {
    outputs[[i]]$line <- new_code_info[[i]]$lineid
    outputs[[i]]
  })

  list(new_code_info = new_code_info, outputs = outputs)
}

#' Helper function that updates line information for R Shiny
#' reactive values and sends UI information to the JS side.
#'
#' @param order A numeric vector
#' @param outputs A list of outputs from `get_dplyr_intermediates`
#' @param current_code_info The current code info list structure
#' @param new_code_info The new code info list structure
#' @param session The Shiny session
#'
#' @return
#'
#' @noRd
update_lines <- function(order, outputs, current_code_info, new_code_info, rv, session) {
  # prep data to send JS about box change type, row, and col
  new_rv_code_info <- lapply(current_code_info, function(x) {
    out <- get_output(outputs, x$lineid)
    # error occurred before this line
    if (length(out) == 0) {
      # reset states
      x$change = "invalid" # this is different from error, so we'll call it invalid
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
  new_rv_code_info <- new_rv_code_info[order]
  send_js_code_info <- lapply(new_rv_code_info, function(x) {
    list(id = x$lineid, code = x$code, change = x$change, row = x$row, col = x$col)
  })
  # send JS the new summary box, row and col
  session$sendCustomMessage("update_line", send_js_code_info)

  # update the summaries and outputs as well
  rv$summaries <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), summary = x$summary))
  # send JS the new prompts
  session$sendCustomMessage("update_prompts", rv$summaries)
  rv$outputs <- lapply(outputs, function(x) list(id = x$line, lineid = paste0("line", x$line), output = x$output))
  # update the data display to the last enabled output
  rv$current <- length(rv$outputs)
}

#' Datawats server
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
datawatsServer <- function(id) {
  # load and attach packages
  require(DataTutor)
  require(tidyverse)
  require(tidylog)
  # set tidylog messages to re-route to our tidylog_cache environment so we can access it
  options("tidylog.display" = list(DataTutor:::store_verb_summary))
  moduleServer(
    id,
    function(input, output, session) {
      # these are reactive values related to current line, code info of all lines, summary prompts, and df outputs
      rv <- reactiveValues()
      rv$current <- 0
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
          str(outputs)
          # set reactive values
          rv$code_info <- lapply(outputs, function(x) {
            list(lineid = x$line, code = x$code, change = x$change, row = abbrev_num(x$row), col = abbrev_num(x$col))
          })
          # TODO reset current_code_info via a Reset button
          rv$current_code_info <- lapply(outputs, function(x) {
            list(lineid = x$line, code = x$code, change = x$change, row = abbrev_num(x$row), col = abbrev_num(x$col), checked = TRUE)
          })
          attr(rv$current_code_info, "order") <- seq_len(length(outputs))
          rv$summaries <- lapply(outputs, function(x) list(lineid = paste0("line", x$line), summary = x$summary))
          rv$outputs <- lapply(outputs, function(x) list(lineid = paste0("line", x$line), output = x$output))
          # trigger data frame output of the very last line
          rv$current <- length(rv$outputs)
        }
      });

      # the observer for the code explorer which will get rendered once we have code information
      output$code_explorer <- renderUI({
        if (!is.null(rv$code_info)) {
          shiny::tagList(
            shiny::br(),
            shiny::p("You can click on a summary box to the left to view the summary and dataframe output at each step of the dplyr pipeline. Click on the toggles to the right to enable/disable certain lines for re-evaluation. You can also click and drag the particular line on the move icon to rearrange lines for re-evaluation."),
            shiny::br(),
            shiny::fixedPage(id = "simpleList", class="list-group",
              create_group_item_tags(rv$code_info, id),
              shiny::tags$script("setup_editors();"),
              shiny::tags$script("setup_sortable();"),
              # toggle
              shiny::includeCSS("css/bootstrap4-toggle.min.css"),
              shiny::includeScript("js/bootstrap4-toggle.min.js"),
              shiny::tags$script("setup_toggles();"),
              shiny::tags$script("setup_box_listeners();")
            ),
            shiny::br(),
            # TODO if we want we could also add prompts to the data change scheme color
            shiny::div(class ="d-flex justify-content-center",
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
              )
            ),
            shiny::br()
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
        if (!is.null(input$square)) {
          rv$current <- input$square;
          session$sendCustomMessage("square", input$square)
        }
      })

      # render a reactable of the current line output
      output$line_table <- reactable::renderReactable({
        value <- as.numeric(rv$current)
        if (!is.null(value) && length(rv$outputs) > 0) {
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
        if (isTRUE(input$toggle$checked)) {
          session$sendCustomMessage("toggle", paste0("un-commenting line ", input$toggle$lineid))
        } else {
          session$sendCustomMessage("toggle", paste0("commenting line ", input$toggle$lineid))
        }
        line_id <- as.numeric(input$toggle$lineid)
        checked <- input$toggle$checked

        # set the current order of lines
        order <- attr(rv$current_code_info, "order")
        # update the current code info with the checked flag
        rv$current_code_info <- lapply(rv$current_code_info, function(x) {
          if (x$lineid == line_id) {
            x$checked <- checked
          }
          x
        })
        attr(rv$current_code_info, "order") <- order

        # generate new code info and dataframe outputs
        code_info_outputs <- generate_code_info_outputs(order, rv)
        # quit early when no lines are enabled
        if (is.null(code_info_outputs)) {
          return()
        }

        new_code_info <- code_info_outputs$new_code_info
        outputs <- code_info_outputs$outputs

        # update line information for both R and JS
        update_lines(order, outputs, rv$code_info, new_code_info, rv, session)
      })

      observeEvent(input$reorder, {
        message("REORDER", input$reorder)
        # this lets us get the boolean value of the toggle from JS side!
        order <- as.numeric(input$reorder)

        # set and get the new order from current code stat
        attr(rv$current_code_info, "order") <- order

        # generate new code info and dataframe outputs
        code_info_outputs <- generate_code_info_outputs(order, rv)
        # quit early when no lines are enabled
        if (is.null(code_info_outputs)) {
          return()
        }

        new_code_info <- code_info_outputs$new_code_info
        outputs <- code_info_outputs$outputs

        # update line information for both R and JS
        update_lines(order, outputs, rv$code_info, new_code_info, rv, session)
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
