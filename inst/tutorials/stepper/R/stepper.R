
#' This is the first stage of creating the stepper Shiny app.
#' We return a list structure that will then be rendered with a `knitr::knit_print`
#' on the rendering stage of the app.
#'
#' @param explanations
#'
#' @param start_expr
#' @param explanations
#'
#' @return a list structure with a class of "tutorial_stepper":
#'  list(
#'    id,
#'    setup_code,
#'    start_expr,
#'    stepper_code = list(
#'      source_code,
#'      eval_code
#'    ),
#'    summaries,
#'    callouts
#'  )
#'
#' @export
#'
#' @examples
stepper <- function(start_expr = "", explanations = list()) {

  # create an id for stepper based on current chunk label
  label <- knitr::opts_current$get("label")
  # stepper chunk must have a label
  if (is.null(label) || grepl("unnamed-chunk", label)) {
    stop("A stepper chunk must include a label.", call. = FALSE)
  }

  # TODO eventually be able to walk through either Python or R based on engine info
  # grab the setup and code chunks
  code_label <- paste0(label, "-code")
  # we use -warmup so as not to conflict with learnr's -setup
  setup_chunk <- knitr::knit_code$get(paste0(label, "-warmup"))
  code_chunk <- knitr::knit_code$get(code_label)

  # must require code
  if (is.null(code_chunk)) {
    stop(glue::glue(
      "The code chunk with label, {code_label}, could not be found. Make sure you have included it."
    ), call. = FALSE)
  }

  # must require explanations
  if (!length(explanations)) {
    stop("The `explanations` list must be provided for the stepper to render.")
  }
  # TODO also check if the length of list matches code vector length
  # (we want 1:1 mapping of explanation to snippet line)

  # one time tutor initialization
  initialize_tutorial()

  # make sure we have one string for setup code (handy when running `reticulate::py_eval_string`)
  setup_code <- paste0(setup_chunk, collapse = "\n")
  # we will maintain the source code as it was written so we can display it verbatim
  source_code <- paste0(code_chunk, collapse = "\n")
  # but we will also maintain a code vector so we can step through each line
  eval_code <- purrr::map_chr(code_chunk, stringr::str_trim)

  # collect summaries, turning text into markdown if needed
  summaries = lapply(
    explanations,
    function(explain) stepper_text(explain$summary)
  )
  # collect callouts, turning them into proper list structure
  callouts = lapply(
    explanations,
    function(explain) explain$callouts
  )

  ret <- list(
    id = label,
    setup_code = setup_code,
    start_expr = start_expr,
    stepper_code = list(
      source_code = source_code,
      eval_code = eval_code
    ),
    summaries = summaries,
    callouts = callouts
  )
  class(ret) <- "tutorial_stepper"
  ret
}


#' Constructor for an explanation of a given line of code
#'
#' @param summary the explanation text
#' @param ... variable number of `callout`
#'
#' @return
#' @export
#'
#' @examples
explain <- function(summary = "", ...) {
  tryCatch({
    lapply(
      list(...),
      function(x) checkmate::assert_true(class(x) %in% c("callout_rows", "callout_cols", "callout_col_labels"))
    )
    },
    error = function(e) {
      stop("Callouts must inherit from one of `callout_rows`, `callout_cols, `callout_col_labels`", e)
    }
  )
  list(
    summary = summary,
    callouts = list(...)
  )
}

# callout default method
#' Title
#'
#' @param spec
#' @param color
#' @param background
#' @param class
#'
#' @return
#' @export
#'
#' @examples
callout <- function(spec = NULL, color = "black", background = "white", class = "") {
  structure(
    list(spec = spec, color = color, background = background),
    class = class
  )
}

#' Callout specification for rows
#'
#' @param spec
#' @param color
#' @param background
#'
#' @return
#' @export
#'
#' @examples
callout_rows <- function(spec = NULL, color = "black", background = "white") {
  # perform any additional checks here for row styling
  callout(spec, color, background, "callout_rows")
}

#' Callout specification for columns
#'
#' @param spec
#' @param color
#' @param background
#'
#' @return
#' @export
#'
#' @examples
callout_cols <- function(spec = NULL, color = "black", background = "white") {
  # perform any additional checks here for columns styling
  callout(spec, color, background, "callout_cols")
}

#' Callout specification for column labels
#'
#' @param spec
#' @param color
#' @param background
#'
#' @return
#' @export
#'
#' @examples
callout_col_labels <- function(spec = NULL, color = "black", background = "white") {
  # perform any additional checks here for columns styling
  callout(spec, color, background, "callout_col_labels")
}

# render markdown (including equations) for stepper_text
stepper_text <- function(text) {
  if (inherits(text, "html")) {
    return(text)
  }
  if (is_tags(text)) {
    return(text)
  }
  if (!is.null(text)) {
    # first return the text after executing inlined code for the
    # styled code bits
    text <- rinline_to_html(text)
    # convert markdown
    md <- markdown::markdownToHTML(
      text = text,
      options = c("use_xhtml", "fragment_only", "mathjax"),
      extensions = markdown::markdownExtensions(),
      fragment.only = TRUE,
      encoding = "UTF-8"
    )
    # remove leading and trailing paragraph
    md <- sub("^<p>", "", md)
    md <- sub("</p>\n?$", "", md)
    shiny::HTML(md)
  }
  else {
    NULL
  }
}

#' knitr print for stepper
#'
#' @param x the stepper
#' @param ... additional fields
#' @inheritParams knitr::knit_print
#' @importFrom knitr knit_print
#' @method knit_print tutorial_stepper
#' @rdname knit_print
#' @return
#'
#' @export
knit_print.tutorial_stepper <- function(x, ...) {
  stepper <- x
  ui <- stepper_module_ui(stepper$id)
  # in context="server-start", register the event hook
  rmarkdown::shiny_prerendered_chunk(
    "server-start",
    "event_register_handler('clicked_button', function(session, event, data) {
      message('custom event: clicked_button ', data$btn)
    })"
  )
  # setup the stepper Shiny module in context="server"
  rmarkdown::shiny_prerendered_chunk(
    "server",
    sprintf(
      "stepper_prerendered_chunk(%s)",
      learnr:::dput_to_string(stepper)
    )
  )
  # knit print the UI
  knitr::knit_print(ui)
}

#' Generates the HTML for the stepper UI
#'
#' @param id
#' @return list that is a shiny.tag.list containing HTML and other Shiny meta data
#'
#' @examples
#' \dontrun{
#' stepper_module_ui("hello")
#' }
#' @export
stepper_module_ui <- function(id) {
  # namespace for module
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::column(
      12,
      shiny::htmlOutput(ns("text"))
    ),
    shiny::br(),
    shiny::column(
      12,
      shiny::div(
        style = "height:100px;",
        shiny::htmlOutput(ns("summary"))
      )
    ),
    shiny::verbatimTextOutput(ns("value")),
    shiny::column(12,
      align = "center",
      shiny::fluidRow(
        shiny::actionButton(ns("firstLine"), label = "First", icon = shiny::icon("fast-backward")),
        shiny::actionButton(ns("previousLine"), label = "Previous", icon = shiny::icon("arrow-left")),
        shiny::actionButton(ns("nextLine"), label = "Next", icon = shiny::icon("arrow-right")),
        shiny::actionButton(ns("lastLine"), label = "Last", icon = shiny::icon("fast-forward"))
      )
    ),
    shiny::column(12,
      align = "center",
      shiny::fluidRow(shiny::br()),
      shiny::fluidRow(
        shiny::htmlOutput(ns("line_table"))
        # reactable::reactableOutput(ns("line_table"))
      )
    )
  )

}

#' This invokes the stepper Shiny module
#'
#' @param stepper the list structure returned by the `stepper` function
#' @param ... additional fields
#'
#' @return nothing
#' @export
stepper_prerendered_chunk <- function(stepper, ...) {
  shiny::callModule(
    stepper_module_server,
    stepper$id,
    stepper = stepper
  )
  invisible(TRUE)
}

# TODO use stepper as a data container to pass in relevant information
# like setup chunk, code chunk, explanations etc.
stepper_module_server <- function(input, output, session, stepper) {
  # current index of code lines
  # we make it a reactiveVal because we will update it via stepper button clicks
  current <- shiny::reactiveVal(1)

  # extract the setup, eval + source code, and the explanation list
  setup_code <- stepper$setup_code
  eval_code <- stepper$stepper_code$eval_code
  source_code <- stepper$stepper_code$source_code
  # summary +
  summaries <- stepper$summaries
  all_callouts <- stepper$callouts
  # to assist with parsing
  start_expr <- stepper$start_expr

  # last line
  lastLine <- length(eval_code) - 1

  # TODO gracefully handle python errors
  generate_df_outputs <- function(eval_code, setup_code) {
    # run the setup code first
    if (!is.null(setup_code)) {
      reticulate::py_run_string(setup_code)
    }
    # skip the last line for parens
    last_index <- length(eval_code) - 1
    # skip the first line so we can handle start_expr first
    range <- 2:last_index
    # eval the starting expression, `start_expr`
    outputs <- list(reticulate::py_eval(start_expr, convert = FALSE))
    for (i in range) {
      start_expr <- paste0(start_expr, eval_code[[i]])
      df <- reticulate::py_eval(start_expr, convert = FALSE)
      outputs <- append(outputs, df)
    }
    outputs
  }
  # prepopulate all df outputs
  df_outputs <- generate_df_outputs(eval_code, setup_code)

  # get next summary
  get_summary <- function(idx) {
    summaries[[idx]]
  }
  # get next annotation
  get_callouts <- function(idx) {
    if (idx <= length(all_callouts)) {
      all_callouts[[idx]]
    } else {
      list()
    }
  }

  # TODO try out the kable way, maybe just make a fresh function so it's easier to test
  df_kable <- function(idx) {
    # first get the raw pandas dataframe
    raw_df <- df_outputs[[idx]]
    callouts <- get_callouts(idx)
    # check if df is a MultiIndex
    is_multi_index <- "pandas.core.indexes.multi.MultiIndex" %in% class(raw_df$index)
    kable_pandas(
      df = raw_df,
      show_rownames = !is_multi_index,
      callouts = callouts
    )
  }

  df_reactable <- function(idx) {
    # first get the raw pandas dataframe
    raw_df <- df_outputs[[idx]]
    # check if df is a MultiIndex
    is_multi_index <- "pandas.core.indexes.multi.MultiIndex" %in% class(raw_df$index)
    converted_result <- python_df(raw_df)
    if (is_multi_index) {
      show_index <- !identical(converted_result[[1]], rownames(converted_result))
      grp_cols <- group_columns(
        colnames(converted_result),
        start = ifelse(show_index, 1, 2)
      )
      return(
        reactable::reactable(
          converted_result,
          rownames = show_index,
          compact = TRUE,
          minRows = 8,
          defaultPageSize = 8,
          showPagination = FALSE,
          # height = 300,
          highlight = TRUE,
          bordered = TRUE,
          columns = grp_cols[[1]],
          columnGroups = grp_cols[[2]]
        )
      )
    }

    reactable::reactable(
      converted_result,
      compact = TRUE,
      minRows = 8,
      defaultPageSize = 8,
      showPagination = FALSE,
      # height = 300,
      highlight = TRUE,
      bordered = TRUE,
    )
  }

  flair_line <- function(idx) {
    # look for the pattern of the current line in code
    # currently, this is just whatever line we are on
    pattern <- eval_code[[idx]]
    # flair the whole source code with the pattern line highlighted
    highlighted_code <- flair::flair(source_code, pattern)
    # TODO this does not provide syntax highlighting so maybe look into
    # using prism programmatically
    shiny::HTML(
      paste0(
        "<br><pre><code class=\"language-python\">",
        highlighted_code,
        "</code></pre>"
      )
    )
  }

  # handlers for each button
  shiny::observeEvent(input$firstLine, {
    if (current() > 1) {
      current(1)
    }
    message("clicked First button: ", current())
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "first"))
  })

  shiny::observeEvent(input$previousLine, {
    if (current() > 1) {
      current(current() - 1)
    }
    message("clicked Previous button: ", current())
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "previous"))
  })

  shiny::observeEvent(input$nextLine, {
    if (current() < lastLine) {
      current(current() + 1)
    }
    message("clicked Next button: ", current())
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "next"))
  })

  shiny::observeEvent(input$lastLine, {
    if (current() < lastLine) {
      current(lastLine)
    }
    message("clicked Last button: ", current())
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "last"))
  })

  output$text <- shiny::renderUI({
    flair_line(current())
  })

  output$summary <- shiny::renderUI({
    shiny::tagList(
      shiny::tags$script(
        shiny::HTML(
          "$(document).ready(function(){
    $('.popover-dismiss').popover({
      trigger: 'hover'
    })
    $('[data-toggle=\"popover\"]').popover({
      trigger: 'hover',
      html: true
    });
});"
        )
      ),
      get_summary(current())
    )
  })


  output$line_table <- function() {
    message("rendering table: ", current())
    df_kable(current())
  }

  # output$line_table <- renderReactable({
  #   df_reactable(current())
  # })
}
