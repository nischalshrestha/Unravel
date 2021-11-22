library(shiny)
library(tidyverse)
library(babynames)
library(gapminder)

### Built-in Datasets

# built-in toy datasets to explore dplyr / tidyr
# nice for demo-ing effect of `rowwise`
student_grades <- tibble::tibble(
  student_id = 1:3,
  test1 = 1:3,
  test2 = 4:6
)

# smaller size of babynames dataset
mini_babynames <-
  babynames %>%
    group_by(year, sex) %>%
    group_split() %>%
    head(50) %>%
    bind_rows()

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
"# things you can try
# task: try toggling off `group_by` to see its effect down the chain
# task: try reordering `select` after `summarise`
# task: try reordering `arrange` before `summarise` to show how `summarise` does not respect order anymore
diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))",

  # example 2 (order matters)
  # Unravel: the reorder of lines can reveal how verb order matters, e.g. `filter` has to follow `groupby` here.
  #
  # task1: toggle off the `group_by` (you get aggregate of whole data now)
  # task2: toggle off the `filter` (you consider all species)
  # task3: put `filter` line before `group_by`
  #
  # the summary box is grey indicating no change since we are now operating on the whole dataframe; we always have more
  # than 1 row in the dataframe.
  starwars1 =
"# things you can try
# task: try toggling off `group_by` to see its effect on `summarise`
# task: try reordering `filter` before `group_by` and note how it has no effect anymore, and affects final output
starwars %>%
  group_by(species) %>%
  filter(n() > 1) %>%
  summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))",

  # example 3 (handle your NAs lest it bite you)
  # Unravel:
  # task1: toggle off on `drop_na`
  # (can highlight the importance of dropping NAs for columns which you operate on for other verbs like group_by/summarise
  # (`mean` by default will not handle NAs))
  #
  # The `fill` line would replace NAs in column 'mass' (by default replacing values from top to bottom)
  # since we `drop_na` the mass this line is redundant as-is, but
  #
  # task2: if you toggle off the `drop_na` line
  # you can see it replace NAs as noted by yellow summary box color, changed dimensions and data prompt pointing out fewer NAs.
  starwars2 =
"# things you can try
# task: see if you can glean information on the `fill` that's not doing anything by clicking the summary box besides `fill`
# task: try toggling off `drop_na` or reordering `fill` before `drop_na` to see why it's not doing anything initially
starwars %>%
  drop_na(hair_color, mass) %>%
  group_by(hair_color) %>%
  fill(mass) %>%
  summarise(mass_mean = mean(mass))",

  # example 4 (subtle function behavior -- grouping row)
  # inspired by https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/#basic-operation
  # Unravel: by calling this out via blue summary box and blue row column on output, rowwise becomes clearer.
  #
  # task: toggle the `rowwise` line on/off
  # can help highlight `rowwise` effect on behavior in this example
  # where it will properly calculate mean scores of test1, test2 across the row of each student instead of col-wise.
  # built-in toy dataset
  studentgrades =
"# things you can try
# task: try toggling off `rowwise` and see the effect on `mutate` output
student_grades %>%
  rowwise() %>%
  mutate(avg_scores = mean(c(test1, test2)))",

  # example 5 (subtle function behavior -- group dropping)
  # example question (https://community.rstudio.com/t/understanding-group-by-order-matters/22685)
  #
  # task: toggle off on line 2
  # `summarise` drops the last grouping variable (`gear`) but keeps the rest (`cyl`) for anything downstream
  mtcars1 =
"# things you can try
# task: try clicking on summary box besides `summarise` and note its output to see how it drops one grouping variable
# task: now try changing the code above by removing `gear` and click Unravel button to examine the effect
mtcars %>%
  group_by(cyl, gear) %>%
  summarise(mean_mpg = mean(mpg))",

  # example 6 (important but easy to forget steps --- ungroup)
  # "semantic parens": https://twitter.com/monkmanmh/status/1369691831403868160
  # task: reorder `ungroup` so that it's between the two mutate
  # tip: you should do an explicit `ungroup` and if you don't you might get unexpected results
  gapminder =
"# things you can try
# task: try toggling off `ungroup` and check out the summary box on the second `mutate` to see
# how grouping is retained until `ungroup()`
# task: try reordering `ungroup` after the first `mutate` and notice the change in output as you
# toggle `ungroup()` on and off
gapminder %>%
  group_by(country) %>%
  mutate(mean_pop = mean(pop)) %>%
  mutate(mean_life = mean(lifeExp)) %>%
  ungroup()",

  # example 8 (re-shaping)
  # task: toggle `group_by`, `slice`
  # task: drag and drop `slice` before `groupby`
  iris =
"# things you can try
# try toggling off `group_by` or reordering `slice` before `group_by` and notice how `slice` behaves
# differently on grouped vs ungrouped dataframes
iris %>%
  group_by(Species) %>%
  slice(1) %>%
  pivot_longer(-Species, names_to = \"flower_att\", values_to = \"measurement\")",

  # example 9 (re-shaping and transforming columns)
  # task: toggle off `pivot_wider` line to see column dependency on M/F
  #
  # Lesson: summary box + data prompt is useful to show how `pivot_wider` reshapes the data (dimension change + explanation)
  # Lesson: toggling off a verb shows column dependencies btw verbs: for e.g., the `pivot_wider` creates the M/F columns
  # required to compute new variables (the box is handy in showing the drastic change)
  # note the dimension change (pivot_wider)
  minibabynames =
"# things you can try
# try toggling off `pivot_wider` to see how `mutate` depended on newly created columns `M` / `F` from the `pivot_wider`
mini_babynames %>%
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
#' @export
#'
#' @examples
create_group_item_tags <- function(lines, ns_id) {
  ataglist <- lapply(lines, group_item_div, ns_id = ns_id)
  class(ataglist) <- c("shiny.tag.list", "list")
  return(ataglist)
}

#' Unravel UI
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
#' Unravel UI
#'
#' @param id A character
#' @param dplyr_code A character
#'
#' @return
#' @export
#'
#' @examples
unravelUI <- function(id) {
  package_path <- file.path(system.file(package = "DataTutor"))
  package_css <- file.path(package_path, "css")
  package_js <- file.path(package_path, "js")
  # namespace for module
  ns <- shiny::NS(id)
  shiny::fixedPage(
    # fontawesome (for glyphicon for move)
    shiny::tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    shiny::tags$body(
      # bootstrap stuff
      shiny::includeCSS(file.path(package_css, "bootstrap.min.css")),
      shiny::includeCSS(file.path(package_css, "bootstrap3.min.css")),
      # codemirror stuff
      shiny::includeScript(file.path(package_js, "codemirror.js")),
      shiny::includeCSS(file.path(package_css, "codemirror.css")),
      shiny::includeScript(file.path(package_js, "r.js")),
      # Sortable.js
      shiny::includeScript(file.path(package_js, "Sortable.js")),
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
  # message("producing new code info")
  # str(quoted)

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
#' @param id
#'
#' @return
#' @export
#'
#' @examples
unravelServer <- function(id, user_code = NULL) {
  # load and attach packages
  suppressMessages(library(reactable))
  library(DataTutor)
  shiny::addResourcePath('www', system.file('www', package = 'DataTutor'))
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
            outputs <- get_dplyr_intermediates(quoted[[1]])
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
            reactable::reactable(data = select(.data = final_data, group_vars(final_data), everything()) %>% as.data.frame(),
                                 compact = TRUE,
                                 highlight = TRUE,
                                 bordered = TRUE,
                                 rownames = TRUE,
                                 defaultPageSize = 5,
                                 # we can do a custom thing for a particular column
                                 columns = DataTutor:::reappend(
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
                                 columns = DataTutor:::reappend(
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
