
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny div
#' @importFrom shiny span
#' @importFrom shiny fluidPage
#' @importFrom shiny shinyApp
#' @importFrom shiny tags
#' @importFrom shiny moduleServer
#' @importFrom shiny renderUI
#' @importFrom shiny onStop
#' @importFrom reactable colDef
#' @importFrom utils getParseData
NULL

### Shiny App logic

#' Creates a summary button
#'
#' @param ns_id the \code{character} namespace id
#' @param inputId the \code{character} id for the button
#' @param lineid the \code{integer} id for the line
#' @param change_type the \code{character} for the change types
#' @param value the starting \code{integer} value for the button (0 by default)
#'
#' @return \code{shiny::tags$button}
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
#' @param line the list containing information about a particular line
#' @param ns_id the character for the Shiny module namespace id
#'
#' @return \code{shiny::div}
#' @noRd
group_item_div <- function(line, ns_id) {
  ns <- shiny::NS(ns_id)
  # line_id is for SortableJS and is just a number of the line
  line_id <- line$lineid
  # preprocess that info ahead of time
  line_code <- line$code
  change_type <- line$change
  row <- line$row
  col <- line$col
  # whereas id is for a readable identifier for JS/jquery/CSS
  id <- paste0("line", line$lineid)
  is_verb_line <- line_id != 1L
  core_divs <- list(
    # row div
    shiny::div(class = "justify-content-center align-self-baseline",
        shiny::div(class = "d-flex justify-content-center align-self-center",
            shiny::div(class = "row", style = "font-size:0.8em;",
                shiny::HTML("&nbsp;")
            )
        ),
        shiny::div(class = glue::glue("{id}-summary-box-row d-flex empty-square justify-content-center"),
            shiny::div(class=glue::glue("{id}-row-content align-self-center"), style="font-size: 0.8em;",
                # update element
                shiny::HTML(row)
            )
        )
    ),
    # column div + square div
    shiny::div(class = "justify-content-center align-self-baseline",
        shiny::div(class = glue::glue("{id}-summary-box-col d-flex justify-content-center align-self-center"),
            shiny::div(class = glue::glue("{id}-col-content row"), style = "font-size:0.8em;",
                # update element
                shiny::HTML(col)
            )
        ),
        # update element (class of square)
        summary_button(ns_id, id, line_id, change_type)
    )
  )
  disabled <- ifelse(is_verb_line, "", "static")
  core_divs <- append(
    core_divs,
    list(
      # glyphicon
      # add the move icon if it's not the first line, else make it transparent
      shiny::div(class=glue::glue("{id}-glyph d-flex justify-content-center align-self-center"),
          shiny::span(class=glue::glue("glyphicon glyphicon-move {disabled}"), style=glue::glue("opacity:{as.integer(is_verb_line)};"))
      ),
      # codemirror empty div above
      shiny::div(class="d-flex justify-content-center align-self-center", style="padding:0.5em;",
          div(class="row", style="font-size: 1em;", shiny::HTML("&nbsp;"))
      ),
      # codemirror div (gets dynamically created); fixedPage keeps the width=100%
      shiny::tags$textarea(
        shiny::HTML(line_code),
        class = "verb",
        id = id,
        lineid = line_id
      )
    )
  )

  if (is_verb_line) {
    core_divs <- append(
      core_divs,
      list(
        # toggle checkbox
        shiny::div(style="opacity:1; padding-right:0.25em;",
            shiny::div(class="d-flex justify-content-center align-self-center",
                shiny::div(style="font-size: 0.8em;", shiny::HTML("&nbsp;"))
            ),
            # the value of `checked` is not meaningful, the existence of attribute turns on toggle by default
            shiny::tags$label(
              class = "switch",
              shiny::tags$input(
                type = "checkbox",
                class = "slider",
                `toggle-id` = id,
                `line-id` = line_id,
                `checked` = TRUE
              ),
              span(class="slider round")
            )
        )
      )
    )
  }
  # if it's the data line (first line), disable it for SortableJS so we can't move it
  shiny::div(class = glue::glue("d-flex list-group-item {disabled}"), id=id, `data-id` = line_id,
    core_divs
  )
}

#' Helper function to create a group item div for SortableJS
#'
#' @param lines a `list(list(...))` containig information for all of the lines
#' @param ns_id a `character` id for the shiny module namespace
#'
#' @return `shiny::tagList`
#' @noRd
create_group_item_tags <- function(lines, ns_id) {
  ataglist <- lapply(lines, group_item_div, ns_id = ns_id)
  class(ataglist) <- c("shiny.tag.list", "list")
  return(ataglist)
}

#' Unravel UI
#'
#' @param id A \code{character}
#'
#' @return \code{shiny::fixedPage}
#' @noRd
unravelUI <- function(id) {
  package_path <- file.path(system.file(package = "Unravel"))
  package_css <- file.path(package_path, "css")
  package_js <- file.path(package_path, "js")
  # namespace for module
  ns <- shiny::NS(id)
  shiny::fixedPage(
    # fontawesome (for glyphicon for move)
    shiny::tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    # <script src=></script>
    tags$head(tags$script(src="https://raw.githack.com/SortableJS/Sortable/master/Sortable.js")),
    shiny::tags$body(
      # bootstrap stuff
      shiny::includeCSS(file.path(package_css, "bootstrap.min.css")),
      shiny::includeCSS(file.path(package_css, "bootstrap3.min.css")),
      # codemirror stuff
      shiny::includeScript(file.path(package_js, "codemirror.js")),
      shiny::includeCSS(file.path(package_css, "codemirror.css")),
      shiny::includeScript(file.path(package_js, "r.js")),
      # Sortable.js
      # shiny::tags$script("https://raw.githack.com/SortableJS/Sortable/master/Sortable.js"),
      # shiny::includeScript(file.path(package_js, "Sortable.js")),
      # custom css
      shiny::includeCSS(file.path(package_css, "style.css")),
      # custom js for exploration of code
      shiny::includeScript(file.path(package_js, "explorer.js")),
      # tippy
      shiny::includeScript(file.path(package_js, "popper.min.js")),
      shiny::includeScript(file.path(package_js, "tippy-bundle.min.js")),
      shiny::includeCSS(file.path(package_css, "light.css")),
    ),
    shiny::includeScript(file.path(package_js, "script.js")),
    shiny::htmlOutput(ns("code_explorer")),
    shiny::div(
      style = "width: 100%; height: 500px; margin: 10px;",
      shiny::htmlOutput(ns("generic_output")),
      reactable::reactableOutput(ns("line_table"))
    )
  )
}

# helper function for grabbing a particular new code
get_code <- function(target, id) {
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

#' Given line order, and the reactive values, generate new intermediate outputs
#' for Unravel
#'
#' @param order numeric vector
#' @param rv reactive values
#'
#' @return \code{list(new_code_info = list(list(...)), outputs = list(...))}
#' @noRd
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

  # format the code by stripping and re-applying `%>%`
  new_code_info <- lapply(seq_len(length(new_code_info)), function(i) {
    new_code_info[[i]]$code <- gsub("%>%", "", new_code_info[[i]]$code)
    new_code_info[[i]]
  })
  new_code_info <- lapply(seq_len(length(new_code_info)), function(i) {
    if (i < length(new_code_info)) {
      new_code_info[[i]]$code <- paste(new_code_info[[i]]$code, "%>%")
    }
    new_code_info[[i]]
  })
  new_code_source <- paste0(lapply(new_code_info, function(x) x$code), collapse = "\n")
  quoted <- rlang::parse_expr(new_code_source)
  # message("producing new code info")

  # get new code intermediate info
  outputs <- get_output_intermediates(quoted)
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
#' @param outputs A list of outputs from `get_output_intermediates`
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
      x$callouts = NULL
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
        x$callouts = NULL
      } else {
        # if no error, update states
        x$code = new_code
        x$change = new_rv$change
        x$row = abbrev_num(new_rv$row)
        x$col = abbrev_num(new_rv$col)
        x$summary = new_rv$summary
        x$output = new_rv$output
        x$callouts = new_rv$callouts
      }
    }
    x
  })
  new_rv_code_info <- new_rv_code_info[order]
  send_js_code_info <- lapply(new_rv_code_info, function(x) {
    list(id = x$lineid, code = x$code, change = x$change, row = x$row, col = x$col)
  })
  # NOTE: this starts the sequence again of requesting callouts, then prompts
  session$sendCustomMessage("update_line", send_js_code_info)

  # update the summaries, callouts, and outputs as well
  rv$summaries <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), summary = x$summary))
  rv$callouts <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), callouts = x$callouts))
  rv$outputs <- lapply(outputs, function(x) list(id = x$line, lineid = paste0("line", x$line), output = x$output))
  rv$cur_callouts <- lapply(outputs, function(x) x$callouts)
  # update the data display to the last enabled output
  rv$current <- length(rv$outputs)
}

#' Unravel server
#'
#' @param id the app \code{character} id
#' @param user_code the \code{character} code text
#'
#' @importFrom utils getParseData
#'
#' @return there are no values returned for this module server function
#'
#' @examples
#' \dontrun{
#' unravelServer("unravel", "mtcars %>% select(cyl)")
#' }
#' @export
unravelServer <- function(id, user_code = NULL) {
  # load and attach packages
  shiny::addResourcePath('www', system.file('www', package = 'Unravel'))
  moduleServer(
    id,
    function(input, output, session) {
      # these are reactive values related to current line, code info of all lines, summary prompts, and df outputs
      rv <- reactiveValues()
      rv$current <- 0
      rv$code_info <-  NULL
      rv$summaries <- list()
      rv$outputs <- NULL
      rv$table_output <- NULL
      rv$main_callout <- NULL

      # send signal to JS of the code text to display
      session$sendCustomMessage("set_code", paste0(user_code))

      # listen for JS to tell us code is ready for us to be processed
      observeEvent(input$code_ready, {
        # message("Receiving code from JS: ", input$code_ready)
        # TODO-refactor: process lines function?
        # process lines
        if (!is.null(input$code_ready) && nzchar(input$code_ready)) {
          err <- NULL
          tryCatch({
            # it could be possible that we receive multiple expressions
            # in this case, we only take the first one for now
            quoted <- rlang::parse_exprs(input$code_ready)
            outputs <- get_output_intermediates(quoted[[1]])
            # set reactive values
            rv$code_info <- lapply(outputs, function(x) {
              list(lineid = x$line, code = x$code, change = x$change, row = abbrev_num(x$row), col = abbrev_num(x$col), err = x$err)
            })
            # TODO-enhance: reset current_code_info via a Reset button
            # store the current code metadata for the UI/logic
            rv$current_code_info <- lapply(outputs, function(x) {
              list(
                lineid = x$line,
                code = x$code,
                change = x$change,
                row = abbrev_num(x$row),
                col = abbrev_num(x$col),
                err = x$err,
                checked = TRUE
              )
            })
            attr(rv$current_code_info, "order") <- seq_len(length(outputs))
            rv$callouts <- lapply(outputs, function(x) list(lineid = paste0("line", x$line), callouts = x$callouts))
            rv$cur_callouts <- lapply(outputs, function(x) x$callouts)
            rv$summaries <- lapply(outputs, function(x) {
              if (!is.null(x$err)) {
                x$summary <- x$err
              }
              list(lineid = paste0("line", x$line), summary = x$summary)
            })
            rv$outputs <- lapply(outputs, function(x) list(lineid = paste0("line", x$line), output = x$output))
            # trigger data frame output of the very last line
            rv$current <- length(rv$outputs)
            },
            error = function(e) {
              err <<- e
            }
          )
        }
      });

      # the observer for the code explorer which will get rendered once we have code information
      output$code_explorer <- renderUI({
        if (!is.null(rv$code_info)) {
          shiny::tagList(
            shiny::br(),
            shiny::fixedPage(id = "simpleList", class="list-group",
              create_group_item_tags(rv$code_info, id),
              shiny::tags$script("setup_editors();"),
              shiny::tags$script("setup_sortable();"),
              # toggle
              shiny::tags$script("setup_toggles();"),
              shiny::tags$script("setup_box_listeners();")
            ),
            shiny::br(),
            # TODO if we want we could also add prompts to the data change scheme color
            shiny::div(class ="d-flex justify-content-center",
              shiny::div(class = "d-flex align-self-center", style = "margin-left: 8em;",
                         # no change
                         div(class = glue::glue("d-flex none-square-key justify-content-center"), style = "cursor: default;"),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 1em; font-size: 0.8em; width: 80px;", "No change"),
                         # internal
                         div(class = glue::glue("d-flex internal-square-key justify-content-center"), style = "cursor: default;"),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 1em; font-size: 0.8em; width: 100px;", "Internal change"),
                         # visible
                         div(class = glue::glue("d-flex visible-square-key justify-content-center"), style = "cursor: default;"),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 1em; font-size: 0.8em; width: 100px;", "Visible change"),
                         # error
                         div(class = glue::glue("d-flex error-square-key justify-content-left"), style = "cursor: default;"),
                         div(class = glue::glue("d-flex empty-square justify-content-left align-self-center"),
                             style = "padding-left: 0.5em; font-size: 0.8em; width: 100px;", "Error"),
              )
            ),
            shiny::br()
          )
        }
      })

      # list for a trigger message input from JS input so we can send callout info for each line
      observeEvent(input$need_callouts, {
        # message("JS is ready for callouts: ", input$need_callouts)
        session$sendCustomMessage("callouts", rv$callouts)
      })

      # list for a trigger message input from JS input so we can send summary info for data prompts
      observeEvent(input$need_prompts, {
        # message("JS is ready for prompts: ", input$need_prompts)
        session$sendCustomMessage("prompts", rv$summaries)
      })

      # list for a click square input from JS to tells us which data to display for a particular line
      observeEvent(input$square, {
        # message("clicked on a square: ", input$square)
        # make sure to only change current if the current code info has the line marked as enabled
        if (!is.null(input$square)) {
          rv$current <- input$square;
          session$sendCustomMessage("square", input$square)
        }
      })

      observeEvent(input$line, {
        # message("clicked on a line: ", input$line)
        # make sure to only change current if the current code info has the line marked as enabled
        if (!is.null(input$line)) {
          rv$current <- input$line;
          session$sendCustomMessage("line", input$line)
        }
      })

      # a reactive expression that sets and determines the type of data we render on the UI for code output
      # this could be generic output like vectors and lists, or
      # this could be data.frame / tibble
      data <- reactive({
        value <- as.numeric(rv$current)
        out <- NULL
        if (!is.na(value) && length(rv$outputs) > 0 && value <= length(rv$outputs)) {
          # reactable can only efficiently display data of a certain size
          # if we enter into the 100K range, it starts to slow down
          # message("changed data line output ", value)
          out <- rv$outputs[[value]]$output
          # if it's a data.frame then set the table output reactive value
          if (is.data.frame(out)) {
            # this is a hack to reduce the amount of total rows displayed for performance
            if (dim(out)[[1]] > 5e5) {
              out <- out[1:5e4, ]
            }
            rv$table_output <- out
            rv$main_callout <- rv$cur_callouts[[value]]
          } else {
            # NOTE: we have to set table output to NULL if it's not a data.frame, otherwise it will
            # still appear below a generic output
            rv$table_output <- NULL
          }
        }
        out
      })

      # this is the output for non-dataframe and non-plot objects like vectors and lists
      output$generic_output <- renderUI({
        generic_output <- data()
        if (!is.data.frame(generic_output) && !is.null(generic_output)) {
          shiny::tagList({
            shiny::renderPrint(generic_output)
          })
        }
      })

      # shiny output of reactable for a data.frame / tibble
      output$line_table <- reactable::renderReactable({
        final_data <- rv$table_output
        if (!is.na(final_data) && !is.null(final_data) && length(final_data) >= 1) {
          # if we have a grouped dataframe, to facilitate understanding let's rearrange columns such that
          # the grouped variables appear to the very left
          if (is_grouped_df(final_data)) {
            reactable::reactable(data = dplyr::select(.data = final_data, group_vars(final_data), dplyr::everything()) %>% as.data.frame(),
                                 compact = TRUE,
                                 highlight = TRUE,
                                 bordered = TRUE,
                                 rownames = TRUE,
                                 defaultPageSize = 5,
                                 # we can do a custom thing for a particular column
                                 columns = reappend(
                                   list(.rownames = colDef(style = list(textAlign = "left"), maxWidth = 80)),
                                   get_column_css(final_data, rv$main_callout))
                                 )
          } else {
            rowname_background <- list()
            if (inherits(final_data, "rowwise_df")) {
              rowname_background <- list(`background-color` = "lightblue");
            }
            reactable::reactable(data = final_data %>% as.data.frame(),
                                 compact = TRUE,
                                 highlight = TRUE,
                                 bordered = TRUE,
                                 rownames = TRUE,
                                 defaultPageSize = 5,
                                 # we can do a custom thing for a particular column
                                 columns = reappend(
                                   list(.rownames = colDef(style = append(list(textAlign = "left"), rowname_background), maxWidth = 80)),
                                   get_column_css(final_data, rv$main_callout))
                                 )
          }
        }
      })

      # this input even tells us which line to (un)comment
      observeEvent(input$toggle, {
        # message("TOGGLE", input$toggle)
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
        # message("REORDER", input$reorder)
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
