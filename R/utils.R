
### Verb summary

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# empty out the tidylog summary env initially
tidylog_cache <- new.env(parent=emptyenv())

# helper function to get the current verb summary out of tidy log
get_verb_summary <- function() {
  if (exists("verb_summary", envir = tidylog_cache)) {
    m <- get("verb_summary", envir = tidylog_cache)
    # message("getting tidylog message", m)
    return(m)
  }
  return(NULL)
}

# this is a quick way to store a tidylog summary instead of `message`ing it.
store_verb_summary <- function(m) {
  # message("assigning tidylog message: ", m)
  assign("verb_summary", as.character(m), envir = tidylog_cache)
}

# helper function to clear the tidylog_cache envir
clear_verb_summary <- function(){
  # message("clearing tidylog messages")
  rm(list=ls(tidylog_cache, all.names=TRUE), envir=tidylog_cache)
}

### Callouts

# callout cache for highlighting code text
callout_cache <- new.env(parent=emptyenv())

# helper function to get the current callout words as set by tidylog
get_line_callouts <- function() {
  if (exists("callouts", envir = callout_cache)) {
    callouts <- get("callouts", envir = callout_cache)
    # message("getting callouts ", callouts)
    return(callouts)
  }
  return(NULL)
}

# helper function to store callout words from tidylog
store_line_callouts <- function(callouts) {
  # message("storing callouts ", callouts)
  assign("callouts", callouts, envir = callout_cache)
}

# helper function to clear the callout_cache envir
clear_callouts <- function() {
  # message("clearing callouts")
  rm(list=ls(callout_cache, all.names=TRUE), envir=callout_cache)
}

#' Returns an abbreviated version of a number (K for thousndas and M for millions)
#'
#' We won't do more than millions of rows for now.
#'
#' @export
#' @param x a numeric
#' @return a character
#' @examples
#' abbrev_num(1950) # 1.95K
#' abbrev_num(1950000) # 1.95M
abbrev_num <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (x >= 1e3 && x < 1e6) {
    return(paste0(format(round(x / 1e3, 1), trim = TRUE), "K"))
  }
  else if (x >= 1e6) {
    return(paste0(format(round(x / 1e6, 1), trim = TRUE), "M"))
  }
  return(as.character(x))
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
#' reappend(list(a = 1), list(b = 2))
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

match_gregexpr <- function(pattern, string, which_one = 1) {
  gregexpr(pattern, string)[[1]][[which_one]]
}
