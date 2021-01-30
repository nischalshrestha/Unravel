library(tidyverse)

#' Given a dplyr chain, return a list of intermediate expressions, including
#' the dataframe name expression.
#'
#' @param lhs
#' @param outputs
#'
#' @return [`language`]
#'
#' @examples
recurse_lhs <- function(lhs, outputs = list()) {
  if (inherits(lhs, "name")) {
    return(list(lhs))
  }
  # get the output of the quoted expression so far
  base <- append(list(lhs), outputs)
  return(
    append(recurse_lhs(lhs[[2]]), base)
  )
}

#' Given a quoted dplyr chained code, return a list of intermediate outputs.
#'
#' If there is an error, \code{get_intermediates} will return outputs up to that
#' line, with an error message for the subsequent line at fault.
#'
#' @param pipeline quoted dplyr code
#'
#' @return list(
#'   intermediates = list(`tibble`),
#'   error = character(),
#' )
#' @export
#'
#' @examples
get_intermediates <- function(pipeline) {
  # if first part of ast is not a %>% just quit
  if (!identical(pipeline[[1]], as.symbol("%>%"))) {
    stop("`pipeline` input is not a pipe call!")
  }
  # first grab all of the lines as a list of of language objects
  lines <- recurse_lhs(quoted)
  results <- list()
  for (i in seq_len(length(lines))) {
    err <- NULL
    tryCatch(
      # TODO alter tidylog and get the log output to attach as well
      results <- append(results, list(eval(lines[[i]]))),
      error = function(e) {
        err <<- e
      }
    )
    if (!is.null(err)) {
      # Thought: we could make even more readable messages
      # Error: Must group by variables found in `.data`.
      # * Column `colorr` is not found.
      # for e.g. we could replace the `.data` with the actual expression
      message(err$message)
      msg <- ifelse(
        nzchar(err$message),
        crayon::strip_style(err$message),
        crayon::strip_style(paste0(err))
      )
      return(list(
        intermediates = results,
        error = msg
      ))
    }
  }
  return(results)
}

# example
"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))" -> pipeline
quoted <- rlang::parse_expr(pipeline)
outputs <- get_intermediates(quoted)

