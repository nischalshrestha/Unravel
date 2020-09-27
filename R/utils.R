
# helper function to get the last line of the code in an exercise
get_last_line <- function(options) {
  non_empty <- options$code[options$code != ""]
  tail(non_empty, n = 1)
}

#' Takes in an unconverted pandas.DataFrame via `reticulate` and returns
#' an R data.frame that has the look of a pandas.DataFrame
#'
#' @param pydf 
#'
#' @return
#' @export
#'
#' @examples
python_df <- function(pydf) {
  # Strategy: 
  # 1) First, read it as csv to preserve types (except for NaNs)
  rdf <- read.csv(text=pydf$to_csv())
  #     a      b
  # 0  1.0   True
  # 1  NaN  False
  # 2  3.0   True
  # 2) make X the rownames and delete it
  rownames(rdf) <- rdf$X
  rdf <- rdf[-1]
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
  rownames(rdf) <- vapply(rownames(rdf), function(x) as.character(as.numeric(x) - 1), FUN.VALUE = character(1))
  rdf
}
