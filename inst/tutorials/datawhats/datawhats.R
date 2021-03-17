library(shiny)
library(babynames)

### Built-in Datasets

# built-in toy datasets to explore dplyr / tidyr
# nice for demo-ing effect of `rowwise`
student_grades <- tibble::tibble(
  student_id = 1:3,
  test1 = 1:3,
  test2 = 4:6
)

# smaller size of babynames dataset
mini_babynames <- head(babynames, 100000)

# list of example code to play with
example_list <- list(
# example 1
# Unravel: the toggle off can help us understand data semantic mistakes, like not grouping before summarizing
# Unravel: reordering `arrange` before `summarise` reveals how `summarise` no longer respects order according to `arrange`
# Unravel: the toggle off on certain verbs can also not really affect verbs down the pipeline such as `select` here (redundancy)
# Unravel: code and data callouts help identify the new variables you should pay attention to (e.g. `n` and `price` via `summarise()`)
# group_by and summarize go hand in hand together, don't forget to group_by if summarising on certain variables
# note also how `select` in this example only gives us problems if placed after a line that removes one of the variables
diamonds =
"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))",

# example 2 (order matters)
# Unravel: the reorder of lines can reveal how verb order matters, e.g. `filter` has to follow `groupby` here.
# when `filter` line is placed before `group_by`, the summary box is grey indicating no change since we are now operating on
# the whole dataframe; we always have more than 1 row in the dataframe.
starwars1 =
"starwars %>%
  group_by(species) %>%
  filter(n() > 1) %>%
  summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))",

# example 3 (handle your NAs lest it bite you)
# Unravel: the toggle off on `drop_na` can highlight the importance of dropping NAs for columns which you operate on for
# other verbs like group_by/summarise (`mean` by default will not handle NAs)
# The `fill` line would replace NAs in column 'mass' (by default replacing values from top to bottom)
# since we `drop_na` the mass this line is redundant as-is, but if you toggle off the `drop_na` line you can see it
# replace NAs as noted by yellow summary box color, changed dimensions and data prompt pointing out fewer NAs.
starwars2 =
"starwars %>%
  drop_na(hair_color, mass) %>%
  group_by(hair_color) %>%
  fill(mass) %>%
  summarise(mass_mean = mean(mass))",

# example 4 (subtle function behavior -- grouping row)
# inspired by https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/#basic-operation
# Unravel: by calling this out via blue summary box and blue row column on output, rowwise becomes clearer.
# toggling the `rowwise` line on/off can help highlight `rowwise` effect on behavior in this example
# where it will properly calculate mean scores of test1, test2 across the row of each student instead of col-wise.
# built-in toy dataset
studentgrades =
"student_grades %>%
  rowwise() %>%
  mutate(avg_scores = mean(c(test1, test2)))",

# example 5 (subtle function behavior -- group dropping)
# example question (https://community.rstudio.com/t/understanding-group-by-order-matters/22685)
# Unravel: the data prompt + summary box color + toggle off on line 2 can point out how `summarise` drops the last grouping variable (`gear`)
# but keeps the rest (`cyl`) for anything downstream
mtcars1 =
"mtcars %>%
  group_by(cyl, gear) %>%
  summarise(mean_mpg = mean(mpg))",

# example 6 (important but easy to forget steps --- ungroup)
# "semantic parens": https://twitter.com/monkmanmh/status/1369691831403868160
# Unravel: the tool's summary box color is also useful in internal changes like rowwise(), and with the
# data prompt summary pointing out we're still grouped by rows + toggle off on `rowwise`, it can point
# out how rowwise affects subsequent verbs, until you do an explicit `ungroup`
mtcars2 =
"mtcars %>%
  rowwise() %>%
  mutate(mymean = mean(c(cyl, mpg))) %>%
  ungroup() %>% # toggle this off and notice how `select` keeps rows grouped
  select(cyl, mpg, mymean)",

# example 7 (general function behavior discovery --- group_by overrides previous groups )
# Unravel: the blue summary box + blue column on output reveals certain api behavior that the dimension numbers or code callouts
# alone might not help.
# For e.g. `group_by` overrides the previous grouped variables but this is only apparent when we see that for each case, the
# data is changed internally, and the output shows a different blue column being used as the grouped variable.
# "mtcars %>%
#   group_by(cyl) %>%
#   group_by(disp)",

# example 8 (re-shaping)
iris =
"iris %>%
  group_by(Species) %>%
  slice(1) %>%
  pivot_longer(-Species, names_to = \"flower_att\", values_to = \"measurement\")",


# example 9 (re-shaping and transforming columns)
# Lesson: summary box + data prompt is useful to show how `pivot_wider` reshapes the data (dimension change + explanation)
# Lesson: toggling off a verb shows column dependencies btw verbs: for e.g., the `pivot_wider` creates the M/F columns
# required to compute new variables (the box is handy in showing the drastic change)
# note the dimension change (pivot_wider)
# toggle off above line to see column dependency on M/F
minibabynames =
"mini_babynames %>%
   group_by(year, sex) %>%
   summarise(total = sum(n)) %>%
   pivot_wider(names_from = sex, values_from = total) %>%
   mutate(percent_male = M / (M + F) * 100, ratio = M / F)"
)

### Shiny App logic

#' Creates a summary button
#'
#' @param ns_id
#' @param inputId
#' @param lineid
#' @param change_type
#' @param value
#'
#' @return
#'
#' @noRd
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
#'
#' @noRd
group_item_div <- function(line, ns_id) {
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
      shiny::tags$textarea(
        shiny::HTML(line_code),
        class = "verb",
        id = id,
        lineid = line_id
      ),
      # toggle checkbox
      div(style="opacity:1; padding-right:0.25em;",
          div(class="d-flex justify-content-center align-self-center",
              div(style="font-size: 0.8em;", HTML("&nbsp;"))
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
}


#' Helper function to create a group item div for SortableJS
#'
#' @param lines
#' @param ns_id
#'
#' @return
#'
#' @noRd
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
#' Datawats UI
#'
#' @param id A character
#' @param dplyr_code A character
#'
#' @return
#'
#' @noRd
datawatsUI <- function(id, code) {
  # "babynames %>%
  #   group_by(year, sex) %>%
  #   summarise(total = sum(n)) %>%
  #   spread(sex, total) %>%
  #   mutate(percent_male = M / (M + F) * 100, ratio = M / F)" -> code
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
    shiny::column(
      12,
      align = "center",
      titlePanel("Unravel")
    ),
    shiny::column(
      12,
      shiny::selectInput(ns("examples"), label = "Examples",
                         choices = list("diamonds" = "diamonds",
                                        "starwars 1" = "starwars1",
                                        "starwars 2" = "starwars2",
                                        "student grades" = "studentgrades",
                                        "mtcars 1" = "mtcars1",
                                        "mtcars 2" = "mtcars2",
                                        "iris" = "iris",
                                        "mini babynames" = "minibabynames")
      )
    ),
    shiny::column(
      12,
        shiny::tags$textarea(
          class = "code_input",
          id = id
        ),
        shiny::includeScript("js/script.js")
    ),
    shiny::br(),
    shiny::column(
      12,
      align = "center",
    # div(class = "d-flex justify-content-center align-self-center",
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
#'
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
  # send JS the new summary box, row and col
  session$sendCustomMessage("update_line", send_js_code_info)

  # update the summaries, callouts, and outputs as well
  rv$summaries <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), summary = x$summary))
  rv$callouts <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), callouts = x$callouts))
  # send JS the new prompts
  session$sendCustomMessage("update_callouts", rv$callouts)
  session$sendCustomMessage("update_prompts", rv$summaries)
  rv$outputs <- lapply(outputs, function(x) list(id = x$line, lineid = paste0("line", x$line), output = x$output))
  rv$cur_callouts <- lapply(outputs, function(x) x$callouts)
  # update the data display to the last enabled output
  rv$current <- length(rv$outputs)
}

#' Datawats server
#'
#' @param id
#'
#' @return
#'
#' @noRd
datawatsServer <- function(id) {
  # load and attach packages
  require(DataTutor)
  require(tidyverse)
  require(babynames)
  require(tidylog)
  # set tidylog messages to re-route to our tidylog_cache environment so we can access it
  options(
    "tidylog.display" = list(DataTutor:::store_verb_summary),
    "tidylog.callouts" = DataTutor:::store_line_callouts
  )
  require(reactable)
  moduleServer(
    id,
    function(input, output, session) {
      # these are reactive values related to current line, code info of all lines, summary prompts, and df outputs
      rv <- reactiveValues()
      rv$current <- 0
      rv$code_info <-  NULL
      rv$summaries <- list()
      rv$outputs <- NULL

      observeEvent(input$examples, {
        message("Example picked ", input$examples)
        if (nzchar(input$examples)) {
          code <- example_list[input$examples]
          message(code)
          # message(example_list[input$examples])
          session$sendCustomMessage("set_code", paste0(code))
        }
      })

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
            list(lineid = x$line, code = x$code, change = x$change, row = abbrev_num(x$row), col = abbrev_num(x$col), err = x$err)
          })
          # TODO reset current_code_info via a Reset button
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

      # list for a trigger message input from JS input so we can send callout info for each line
      observeEvent(input$need_callouts, {
        message("JS is ready for callouts: ", input$need_callouts)
        session$sendCustomMessage("callouts", rv$callouts)
      })

      # list for a trigger message input from JS input so we can send summary info for data prompts
      observeEvent(input$need_prompts, {
        message("JS is ready for prompts: ", input$need_prompts)
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

      observeEvent(input$line, {
        message("clicked on a line: ", input$line)
        # make sure to only change current if the current code info has the line marked as enabled
        if (!is.null(input$line)) {
          rv$current <- input$line;
          session$sendCustomMessage("line", input$line)
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
            # if we have a grouped dataframe, to facilitate understanding let's rearrange columns such that
            # the grouped variables appear to the very left
            if (is_grouped_df(final_data)) {
              reactable::reactable(data = select(.data = final_data, group_vars(final_data), everything()) %>% as.data.frame(),
                                   compact = TRUE,
                                   highlight = TRUE,
                                   bordered = TRUE,
                                   rownames = TRUE,
                                   # we can do a custom thing for a particular column
                                   columns = DataTutor:::reappend(
                                     list(.rownames = colDef(style = list(textAlign = "left"), maxWidth = 80)),
                                     get_column_css(final_data, rv$cur_callouts[[value]]))
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
                                   # we can do a custom thing for a particular column
                                   columns = DataTutor:::reappend(
                                     list(.rownames = colDef(style = append(list(textAlign = "left"), rowname_background), maxWidth = 80)),
                                     get_column_css(final_data, rv$cur_callouts[[value]]))
                                   )
            }
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

"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))" -> code

ui <- fluidPage(
  datawatsUI("datawat", code)
)

server <- function(input, output, session) {
  datawatsServer("datawat")
}

shinyApp(ui, server)
