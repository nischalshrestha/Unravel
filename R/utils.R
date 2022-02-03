
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

### Function summaries

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

### Linking of Function to Help page

# empty out the tidylog summary env initially
fns_help_cache <- new.env(parent=emptyenv())

# A helper function to store function words from tidylog
# and their code text to use when replacing them in
# the Unravel front-end.
#
# For e.g. in `mutate(mtcars, mpg_round = round(mpg))`
# the `mutate` and `round` are functions for which we
# have help pages for. So the list could look like:
#
# list(
#   mutate = "<a id = 'mutate' class = 'fn_help'>mutate</a>"),
#   round = "<a id = 'round' class = 'fn_help'>round</a>"),
# )
store_fns_help <- function(fns) {
  # message("storing fns_help")
  assign("fns_help", fns, envir = fns_help_cache)
}

# helper function to get the current function help words as set by tidylog
get_fns_help <- function() {
  if (exists("fns_help", envir = fns_help_cache)) {
    fns_help <- get("fns_help", envir = fns_help_cache)
    return(fns_help)
  }
  return(NULL)
}

# helper function to clear the fns_help_cache envir
clear_fns_help <- function() {
  # message("clearing fns_help")
  rm(list=ls(fns_help_cache, all.names=TRUE), envir=fns_help_cache)
}

### Logging

log_info <- function(message, context = "unravel") {
  log_unravel("INFO", message, context)
}

log_event <- function(message, context = "unravel") {
  log_unravel("EVENT", message, context)
}

log_help <- function(message, context = "unravel") {
  log_unravel("HELP", message, context)
}

#' Log a code event
#'
#' A context either executed some code on the IDE or the Unravel app.
#'
#' @param code A code snippet that was executed
#' @param context In which context the code was run in ('rstudio' or 'unravel')
#' @param path The path of the editor
#'
#' @return Nothing
#' @export
log_code <- function(code, path = "", context = "unravel") {
  log_unravel("CODE", code, path, context)
}

#' Log a content change from the RStudio editor and Unravel.
#'
#' @param content The code within editor/Unravel
#' @param context The context of where the code originates from ("rstudio" or "unravel")
#' @param path The path of the editor
#'
#' @return Nothing
#' @export
log_content_change <- function(content, path = "", context = "rstudio") {
  log_unravel("CONTENT_CHANGE", content, path, context)
}

log_unravel <- function(type, message, path = "", context = "unravel", storage = "sqlite") {
  # first, check if all the required options are set, and if not quit early
  unravel_log_options <- c("db.file", "unravel.logdir", "unravel.logfile", "unravel.logging")
  are_options_set <- all(unravel_log_options %in% names(options()))
  if (!are_options_set) return(invisible())
  # if logging is enabled, log
  if (getOption("unravel.logging")) {
    timestamp <- format(Sys.time())
    store_log(
      list(
        timestamp = timestamp,
        type = type,
        message = message,
        path = path,
        context = context
      ),
      storage = storage
    )
  }
}

store_log <- function(..., storage = "sqlite",
                      db_cols = c("timestamp", "type", "message", "path", "context")) {
  variables <- force(...)
  if (length(variables) == 0) {
    stop("Log contained no values!")
  } else if (!identical(names(variables), db_cols)) {
    stop("Columns do not line up. Please use these: ", db_cols)
  }

  # grab the dir and file name from options so it can be customized
  dir_name <- getOption("unravel.logdir")
  if (identical(storage, "file")) {
    dir.create(dir_name, showWarnings = FALSE)
    logfile_name <- getOption("unravel.logfile")
    cat(
      paste0(c(variables), collapse = "|"), "\n",
      file = file.path(dir_name, logfile_name), sep = "", append = TRUE
    )
  } else if (identical(storage, "sqlite")) {
    db_path <- file.path(dir_name, getOption("db.file"))
    if (file.exists(db_path)) {
      mydb <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
      if (length(RSQLite::dbListTables(mydb)) == 0) {
        RSQLite::dbWriteTable(mydb, "events", tibble::as_tibble(variables))
      } else {
        old <- RSQLite::dbReadTable(mydb, "events")
        RSQLite::dbAppendTable(mydb, "events", tibble::as_tibble(variables))
      }
      RSQLite::dbDisconnect(mydb)
    }
  }
  invisible()
}

### Miscellaneous

#' Returns an abbreviated version of a number (K for thousndas and M for millions)
#'
#' We won't do more than millions of rows for now.
#'
#' @param x a numeric
#' @return a character
#' @examples
#' abbrev_num(1950) # 1.95K
#' abbrev_num(1950000) # 1.95M
#'
#' @export
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

match_gregexpr <- function(pattern, string, which_one = 1) {
  gregexpr(pattern, string)[[1]][[which_one]]
}
