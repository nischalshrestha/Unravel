
#' Debug print if options$debug flag is set to TRUE
#'
#' @param options
#' @param msg
#'
#' @return
#'
#' @examples
debug_print <- function(options, msg) {
  if (length(options$debug) && isTRUE(options$debug)) {
    message(msg)
  }
}

# helper function to get the last line of the code in an exercise
get_last_line <- function(options) {
  # TODO: strip comment string characters, only return last viable line
  code <- c(options$code)
  non_empty <- code[code != "" & !startsWith(code, "#")]
  tail(non_empty, n = 1)
}

get_exercise_code <- function(exercise_cache, setup = FALSE) {
  all_chunks <- exercise_cache$chunks
  if (setup) {
    all_chunks <- all_chunks[-length(all_chunks)]
  }
  code <- paste0(
    vapply(all_chunks, function(x) x$code, character(1)),
    collapse = "\n"
  )
  code
}

#' Takes in an unconverted pandas.DataFrame via `reticulate` and returns
#' an R data.frame that has the look of a pandas.DataFrame
#'
#' @param pydf
#'
#' @return
#'
#' @examples
python_df <- function(pydf) {
  # TODO: handle repr for GroupBy dataframe
  # TODO: write some tests for this
  # First handle rownames
  original_rownames <- rownames(reticulate::py_to_r(pydf))
  # 0) You need to check if rownames are already numeric or not
  is_numeric_index <- all(!is.na(as.numeric(original_rownames)))

  # TODO check if dataframe is using MultiIndex
  if ("pandas.core.indexes.multi.MultiIndex" %in% class(pydf$index)) {
    rdf <- as.data.frame(
      readr::read_csv(
        as.character(reticulate::py_call(pydf$reset_index)$to_csv(index = FALSE))
      )
    )
    # TODO investigate whether we can get away with this for more than 2 levels etc.
    # in the case that we only set one index with something like `set_index('date', append=True)`
    # let's leave it alone, but if we see a column like "level_" let's set the rownames
    if (grepl("level_", colnames(rdf)[[1]])) {
      rownames(rdf) <- rdf[[1]]
      rdf <- rdf[2:length(rdf)]
    }
  } else if (is_numeric_index) {
    # 1) First, read it as csv to preserve types (except for NaNs)
    rdf <- as.data.frame(readr::read_csv(as.character(pydf$to_csv(index=FALSE))))
  } else {
    rdf <- reticulate::py_to_r(pydf)
  }
  # 2) Turn some data types back to Python representation
  rdf <- rdf %>%
    purrr::map_df(~ ifelse(is.na(.), "NaN", as.character(.))) %>%
    as.data.frame
  # 3) change to 0-indexing for row index
  rownames(rdf) <- as.numeric(rownames(rdf)) - 1
  rdf
}

group_columns <- function(columns, start = 1) {
  colDefs <- list()
  start <- start + 1
  range <- start:length(columns)
  # make first column be as is
  colDefs[[columns[1]]] <- reactable::colDef(name = columns[1])
  # make
  for (i in range) {
    colDefs[[columns[i]]] <- reactable::colDef(
      name = " "
    )
  }
  colGroups <- list()
  colGroups <-
    lapply(
      columns[start:length(columns)],
      function(column) {
        reactable::colGroup(name = column, columns = c(column))
      }
    )
  list(colDefs, colGroups)
}

format_python_df <- function(options, raw_result) {
  is_multi_index <- "pandas.core.indexes.multi.MultiIndex" %in% class(raw_result$index)

  # wizard of oz pandas dataframe by changing index as well
  converted_result <- python_df(raw_result)
  debug_print(options, "prepped a dataframe")

  # handle reactable for df with MultiIndex
  if (isTRUE(is_multi_index)) {
    cat('multiindex\n')
    # handle the case where we might already have renamed axis for Index
    # in this case, we should not show index, and group one more
    show_index <- !identical(rdf[[1]], rownames(rdf))
    grp_cols <- group_columns(
      colnames(converted_result),
      start = ifelse(show_index, 1, 2)
    )
    return(
      htmltools::knit_print.shiny.tag.list(
        reactable::reactable(
          converted_result,
          compact = TRUE,
          rownames = show_index,
          columns = grp_cols[[1]],
          columnGroups = grp_cols[[2]]
        )
      )
    )
  } else {
    return(
      htmltools::knit_print.shiny.tag.list(
        reactable::reactable(
          converted_result,
          pagination = TRUE,
          minRows = 10,
          compact = TRUE,
          # height = 300,
          highlight = TRUE,
          bordered = TRUE,
          rownames = TRUE,
          resizable = TRUE,
          theme = reactable::reactableTheme(
            borderWidth = "2px"
          ),
          defaultColDef = reactable::colDef(
            cell = function(value, index) {
              # turn NULL into NaN
              if (is.na(value) || is.null(value) || value == "") {
                return(as.character("NaN"))
              }
              return(value)
            }
          ),
          columns = list(
            .rownames = reactable::colDef(
              style = list(
                textAlign = "left"
              )
            )
          )
        )
      )
    )
  }
}

#' Takes a number of lists and appends them into a single list.
#' Instead of doing nested append calls, this will automate that
#' by doing the series of append calls for you.
#'
#' @param ...
#'
#' @return named list
#'
#' @examples
reappend <- function(...) {
  myList <- list()
  items <- list(...)
  range <- 1:length(items)
  # # Now the new experiments
  for (i in seq_along(range)) {
    myList <- append(myList, items[[i]])
  }
  myList
}
