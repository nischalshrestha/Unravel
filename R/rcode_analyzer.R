library(tidyverse)

#' Given a tidyverse expression, return a list containing
#' all the intermediate dataframes/tibbles, and potentially
#' an error message if a certain subexpression cannot be evaluated.
#'
#' TODO override tidylog to format better messages for data prompts.
#'
#' NOTE: this is a quick and dirty version that relies on character wrangling
#' and string building / evaling. A better approach to build a more robust
#' function might be to use rlang functions like parse_expr and use a tree walking
#' approach instead. For that, we can make this function take in a quoted expr instead.
#'
#' @param expr
#'
#' @return `[results = [tibble], error = character]`
#' @export
#'
#' @examples
pipeline_intermediates <- function(expr) {
  expr_lines <- unlist(lapply(strsplit(expr, "%>%"), trimws))
  results <- list()
  start_expr <- expr_lines[[1]]
  # TODO pull this into a function
  if (length(expr_lines) == 1) {
    err <- NULL
    tryCatch(
      results <- append(results, list(eval(parse(text = start_expr)))),
      error = function(e) {
        err <<- e
      }
    )
    return(list(
      results = results,
      error = ""
    ))
  }
  for (i in 1:(length(expr_lines) - 1)) {
    # Thought: we could get rid of mistakes entirely, and only focus on tinkering with
    # what is valid ala structured editing ethos. E.g. dropdown on column names in UI.
    # TODO pull this into a function
    err <- NULL
    tryCatch({
        results <- append(results, list(eval(parse(text = start_expr))))
      },
      error = function(e) {
        err <<- e
      }
    )
    if (!is.null(err)) {
      # Thought: we could make even more readable messages
      # Error: Must group by variables found in `.data`.
      # * Column `colorr` is not found.
      # for e.g. we could replace the `.data` with the actual expression
      print(err$message)
      msg <- ifelse(
        nzchar(err$message),
        crayon::strip_style(err$message),
        crayon::strip_style(paste0(err))
      )
      return(list(
        results = results,
        error = msg
      ))
    }
    start_expr <- paste(start_expr, "%>%", expr_lines[[i + 1]])
  }
  results
}

"diamonds %>%
  select(carat, cut, color, clarity, price) %>%
  group_by(color) %>%
  summarise(n = n(), price = mean(price)) %>%
  arrange(desc(color))" -> expr

# expr <- "select(diamonds, carat, cut, color, clarity, price)"

x <- pipeline_intermediates(expr)
x
