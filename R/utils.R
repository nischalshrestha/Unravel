
#' Debug print if options$debug flag is set to TRUE
#'
#' @param options
#' @param msg
#'
#' @return
#'
#' @examples
debug_print <- function(options, msg) {
  if (length(options$debug) && isTRUE(options$debug))
    message(msg)
}

# helper function to get the last line of the code in an exercise
get_last_line <- function(options) {
  non_empty <- options$code[options$code != ""]
  tail(non_empty, n = 1)
}

get_exercise_code <- function(exercise_cache, setup=FALSE) {
  all_chunks <- exercise_cache$chunks
  if (setup)
    all_chunks = all_chunks[-length(all_chunks)]
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
  # TODO: hanlde repr for dfs that already have rownames not numeric
  # TODO: handle repr for GroupBy dataframe
  # First handle rownames
  original_rownames <- rownames(reticulate::py_to_r(pydf))
  original_colnames <- colnames(reticulate::py_to_r(pydf))
  # 0) You need to check if rownames are already numeric or not
  is_numeric_index <- all(!is.na(as.numeric(original_rownames)))
  # !is.na(as.numeric(rownames(reticulate::py_to_r(pydf))))
  if (is_numeric_index) {
    # cat("is numeric\n")
    # 1) First, read it as csv to preserve types (except for NaNs)
    rdf <- read.csv(text=as.character(pydf$to_csv()))
    #     a      b
    # 0  1.0   True
    # 1  NaN  False
    # 2  3.0   True
    # 2) make X the rownames and delete it
    original_rownames <- rdf$X
    rownames(rdf) <- rdf$X
    rdf <- rdf[-1]
    colnames(rdf) <- original_colnames
    # print(rdf)
  } else {
    # cat("is not numeric\n")
    rdf <- reticulate::py_to_r(pydf)
    # print(rdf)
  }
  #    a     b
  # 0  1  True
  # 1 NA False
  # 2  3  True
  # NA to NaN
  # 3) Turn data types back to Python representation
  # - for each column:
  #   - check type of data
  convert_na <- function(x) {
    prev_class <- class(x)
    if (any(is.na(x))) {
      x[is.na(x)] <- "NaN"
    }
    class(x) <- prev_class
    x
  }
  rdf <- as.data.frame(lapply(rdf, convert_na))
  rownames(rdf) <- original_rownames
  colnames(rdf) <- original_colnames
  rdf
}
