
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
  if (is_numeric_index) {
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
