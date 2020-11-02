

# TODO: a main entry function for stepper functionality

# TODO: a data structure that contains pairs of explanation and callouts on dataframes
# explain_all <- list(
#   list(line = 1, text = "The first thing we want to do is groupby the CID, FE, and select the FID column."),
#   list(line = 2, text = "Then we will count the numbers by the groups.")
# )

# df = pd.DataFrame({'CID':[2,2,3],
#   'FE':[5,5,6],
#   'FID':[1,7,9]})
#
# print (df)
# CID  FE  FID
# 0    2   5    1
# 1    2   5    7
# 2    3   6    9
#
# df = df.groupby(by=['CID','FE'])['FID']
#   .count()
#   .unstack()
#   .reset_index()
#   .rename_axis(None, axis=1)

#' Title
#'
#' @param id
#' @param explanations
#'
#' @return
#' @export
#'
#' @examples
#' stepper(
#'   id = "stepper-ex",
#'   explanations = list(
#'     list(line = 1, text = "The first thing we want to do is groupby the CID, FE, and select the FID column."),
#'     list(line = 2, text = "Then we will count the numbers by the groups.")
#'     )
#'  )
#'
#' @param id
#' @param explanations
stepper <- function(id, explanations = list()) {
  # one time tutor initialization
  initialize_tutorial()

  # TODO: figure out the setup and code chunks and whether they exist
  chunk_label <- id
  # can not guarantee that `label` exists
  # label <- knitr::opts_current$get(chunk_label)
  # q_id <- label %||% random_question_id()

  # TODO: pass that chunk info in the ret structure to be used in `stepper_module_server`


  # TODO: grab setup chunk for the id (chunk )
  ret <- list(
    id = chunk_label,
    explanations = explanations
  )
  class(ret) <- "tutorial_stepper"
  ret
}

#' Title
#'
#' @param x
#' @param ...
#' @inheritParams knitr::knit_print
#' @export
#' @importFrom knitr knit_print
#' @method knit_print tutorial_stepper
#' @rdname knit_print
#' @return
#'
#'
#' @examples
knit_print.tutorial_stepper <- function(x, ...) {
  stepper <- x
  ui <- stepper_module_ui(stepper$id)

  rmarkdown::shiny_prerendered_chunk(
    "server",
    sprintf(
      "DataTutor:::stepper_prerendered_chunk(%s)",
      learnr:::dput_to_string(stepper)
    )
  )

  # regular knit print the UI
  knitr::knit_print(ui)
}

stepper_module_ui <- function(id) {
  # need to namespace for module
  ns <- shiny::NS(id)
  shiny::fluidPage(
    # shiny::renderText("hello"),
    shiny::column(12,
      align = "center",
      shiny::fluidRow(shiny::br()),
      shiny::fluidRow(
        reactable::reactableOutput(ns("base_table"))
      )
    ),
    shiny::column(
      12,
      shiny::fluidRow(shiny::br()),
      shiny::htmlOutput(ns("text"))
    ),
    shiny::br(),
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
        reactable::reactableOutput(ns("line_table"))
      )
    )
  )
}

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
  current <- 1
  # last line (the last index: TODO do it programmatically)
  lastLine <- 1
  # hardcoded example for now
  # TODO: build this programmatically from another reference chunk?
  # e.g. highlight: <span style=\"background-color:#ffff7f\">.set_index('date', append=True)</span>
  " nba = (nba.rename(columns=column_names)
      .dropna(thresh=4)
      [['date', 'away_team', 'away_points', 'home_team', 'home_points']]
      .assign(date=lambda x: pd.to_datetime(x['date'], format='%a, %b %d, %Y'))
      .set_index('date', append=True)
      .rename_axis([\"game_id\", \"date\"])
      .sort_index()
    )" -> code
  "column_names = {'Date': 'date', 'Start (ET)': 'start',
    'Unamed: 2': 'box', 'Visitor/Neutral': 'away_team',
    'PTS': 'away_points', 'Home/Neutral': 'home_team',
    'PTS.1': 'home_points', 'Unamed: 7': 'n_ot'}" -> setup_code

  # TODO:
  # for each relevant line(s):
  # - index/indices
  # - summary of the action
  # - output of the expression so far
  # TODO this is hacky and there's gotta be better solution to read first line (or maybe hold off until clicking the first next button)
  base_expr <- "nba"
  start_expr <- "nba.rename(columns=column_names)"

  generate_df_outputs <- function(code, setup_code = NULL, base_expr = "", start_expr = "") {
    if (!is.null(setup_code)) {
      reticulate::py_run_string(setup_code)
    }
    code_vec <- unlist(strsplit(code, split = "\n"))
    code_vec <- purrr::map_chr(code_vec, stringr::str_trim)
    last_index <- length(code_vec) - 1
    range <- 2:last_index
    outputs <- list(reticulate::py_eval(start_expr))
    for (i in range) {
      start_expr <- paste0(start_expr, code_vec[[i]])
      df <- reticulate::py_eval(start_expr, convert = FALSE)
      outputs <- append(outputs, df)
    }
    outputs
    # handle custom indices
    # TODO hold off on the index and explanation bit for now
    # but revisit when the output stuff can be generated
  }

  df_outputs <- generate_df_outputs(code, setup_code, start_expr = start_expr)

  df_reactable <- function() {
    # first get the raw pandas dataframe
    raw_df <-
      if (current == lastLine) {
        df_outputs[[current - 1]]
      } else {
        df_outputs[[current]]
      }
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

  flair_line <- function() {
    # look for the pattern of the current line in code
    pattern <- unlist(strsplit(code, "\n"))[[current]]
    # and flair it
    code <- flair::flair(code, pattern)

    # hardcoded example for now
    # TODO: build this programmatically
    shiny::HTML(
      "<br>",
      "<pre>",
      "<code class=\"language-python\">",
      code,
      "</code>",
      "</pre>"
    )
  }

  # TODO accept the text from either an existing knitr chunk
  # first time initialization of the text area
  output$text <- shiny::renderUI({
    # Note: for now, length will always just be the initial code rendered in `output$text`
    lastLine <<- length(unlist(strsplit(code, "\n")))
    flair_line()
  })

  output$base_table <- renderReactable({
    out <- python_df(reticulate::py_eval(base_expr, convert = FALSE))
    reactable::reactable(
      out,
      defaultPageSize = 8,
      minRows = 8,
      showPagination = FALSE,
      # height = 300,
      compact = TRUE,
      highlight = TRUE,
      bordered = TRUE,
    )
  })

  output$line_table <- renderReactable({
    df_reactable()
  })

  # handlers for each button
  shiny::observeEvent(input$firstLine, {
    if (current > 1) {
      current <<- 1
    }
    message("clicked First button: ", current)
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "first"))
    output$text <- shiny::renderUI({
      flair_line()
    })
    output$line_table <- renderReactable({
      df_reactable()
    })
  })

  shiny::observeEvent(input$previousLine, {
    if (current > 1) {
      current <<- current - 1
    }
    message("clicked Previous button: ", current)
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "previous"))
    output$text <- shiny::renderUI({
      flair_line()
    })
    output$line_table <- renderReactable({
      df_reactable()
    })
  })

  shiny::observeEvent(input$nextLine, {
    if (current < lastLine) {
      current <<- current + 1
    }
    message("clicked Next button: ", current)
    # trigger an event for learnr (handled in event handler)
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "next"))
    output$text <- shiny::renderUI({
      flair_line()
    })
    output$line_table <- renderReactable({
      df_reactable()
    })
  })

  shiny::observeEvent(input$lastLine, {
    if (current < lastLine) {
      current <<- lastLine
    }
    message("clicked Last button: ", current)
    learnr:::event_trigger(session = session, event = "clicked_button", data = list(btn = "last"))
    output$text <- shiny::renderUI({
      flair_line()
    })
    output$line_table <- renderReactable({
      df_reactable()
    })
  })
}
