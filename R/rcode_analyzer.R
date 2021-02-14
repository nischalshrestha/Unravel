library(tidyverse)

#' Given a quoted dplyr chain code, return a list of intermediate expressions, including
#' the dataframe name expression.
#'
#' @param dplyr_tree quoted dplyr code
#' @param outputs list
#'
#' @return [`language`]
#'
#' @examples
recurse_dplyr <- function(dplyr_tree, outputs = list()) {
  if (inherits(dplyr_tree, "name")) {
    return(list(dplyr_tree))
  }
  # get the output of the quoted expression so far
  base <- append(list(dplyr_tree), outputs)
  return(
    append(recurse_dplyr(dplyr_tree[[2]]), base)
  )
}

#' Given a quoted dplyr chained code, return a list of intermediate outputs.
#'
#' If there is an error, \code{get_dplyr_intermediates} will return outputs up to that
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
#' "diamonds %>%
#'   select(carat, cut, color, clarity, price) %>%
#'   group_by(colorr) %>%
#'   summarise(n = n(), price = mean(price)) %>%
#'   arrange(desc(color))" -> pipeline
#' quoted <- rlang::parse_expr(pipeline)
#' outputs <- get_dplyr_intermediates(quoted)
get_dplyr_intermediates <- function(pipeline) {
  # if first part of ast is not a %>% just quit
  if (!identical(pipeline[[1]], as.symbol("%>%"))) {
    stop("`pipeline` input is not a pipe call!")
  }
  # first grab all of the lines as a list of of language objects
  lines <- recurse_dplyr(quoted)
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

#' Given a quoted dplyr code, return a list of <expression, columns used> pairs.
#'
#' @param quoted dplyr code
#'
#' @return A list of "expr" / "columns"
#'   $expr
#'   [1] "group_by(year, sex)"
#'   $columns
#'   text start_col end_col
#'   1 year        10      13
#'   2  sex        16      18
#'
#' @export
#' @examples
#' require(babynames)
#' "babynames %>%
#'    group_by(year, sex) %>%
#'    summarise(total = sum(n)) %>%
#'    spread(sex, total) %>%
#'    mutate(percent_male = M / (M + F) * 100, ratio = M / F)" -> pipeline
#' quoted <- rlang::parse_expr(pipeline)
#' columns_in_verbs(quoted)
columns_in_verbs <- function(quoted) {
  lines <- recurse_dplyr(quoted)
  outputs <- get_dplyr_intermediates(quoted)
  all_columns <- list()
  for (i in seq_len(length(lines))) {
    if (!inherits(lines[[i]], "name")) {
      verb <- lines[[i]][[3]]
      # get the deparsed character version
      deparsed <- rlang::expr_deparse(verb)
      # then feed it to parse which will parse and return an expression that getParseData likes
      # now we have a syntax parse tree with types of token labeled
      parsed_tree <- getParseData(parse(text = deparsed))
      # from the parse tree dataframe, only grab SYMBOL tokens to try and determine if the
      # SYMBOL is a valid column.
      symbols <- parsed_tree[parsed_tree$token == "SYMBOL", c("text", "col1", "col2")]
      valid_symbols <- Filter(
        function(s) {
          # we first try to see if the SYMBOL is a column in original dataframe
          tryCatch({
              out <- NULL
              if (i == 1) {
                # for first line, we just subset the first output line (dataframe)
                out <- outputs[[i]][[s]]
              } else {
                # for subsequent lines, we subset previous output line (dataframe)
                out <- outputs[[i - 1]][[s]]
              }
              !is.null(out)
            },
            error = function(e) FALSE
          )
        },
        symbols$text
      )
      # only retrieve rows for which symbols were valid columns, and rename col1 + col2
      valid_columns <- symbols %>%
        filter(text == valid_symbols) %>%
        rename(start_col = col1, end_col= col2)
      all_columns <- append(all_columns, list(
          expr = deparsed,
          columns = valid_columns
      ))
    }
  }
  return(all_columns)
}
