
cached_deltas <- new.env(parent=emptyenv())
# Store setup chunks for an exercise or non-exercise chunk.
store_delta_cache <- function(code, diff, overwrite = FALSE){
  if (!overwrite && exists(code, envir = cached_deltas)) {
    return(FALSE)
  }
  if (is.null(diff)){
    return(FALSE)
  }
  assign(code, diff, envir = cached_deltas)
  TRUE
}

# Return a list of knitr chunks for a given exercise label (exercise + setup chunks).
get_delta_cache <- function(code){
  if (exists(code, envir = cached_deltas)) {
    diff <- get(code, envir = cached_deltas)
    return(diff)
  }
  NULL
}