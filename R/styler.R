
# style all the lines of dplyr style code
style_dplyr_code <- function(quoted, char_threshold = 50) {
  # extract parts of the chain
  chains <- recurse_dplyr(quoted)
  # only extract the lines after the dataframe line
  op_chains <- lapply(
    chains[2:length(chains)],
    function(x) x[[3]]
  )
  # collect and format all lines
  final_code_text <- vapply(
    op_chains,
    function(expr) style_long_line(expr, char_threshold),
    character(1)
  )
  # combine the dataframe line with the other lines with pipes/newlines
  paste0(c(chains[[1]], final_code_text), collapse=" %>%\n")
}

# style a long line such that arguments get placed in their own newlines
# if they cross a certain character length threshold (default of 50 chars)
# return the formmated string
style_long_line <- function(expr, char_threshold = 50) {
  # determine if the whole line char length exceeds threshold (including the function part e.g. "mutate")
  exceeds_t <- stringr::str_count(rlang::expr_deparse(expr)) > (char_threshold + 10)
  # if it exceeds length, proceed to style
  if (any(exceeds_t)) {
    # extract function name and the arguments
    func_name <- expr[[1]]
    select_args <- rlang::call_args(expr)
    # prep parts of the final string
    arg_line <- ""
    # keep track of the previous arg expr char count
    prev_count <- 0
    char_count <- 0
    # maintains the total vector of args to be placed on their own lines
    args_list <- c()
    # the current vector of args that hits threshold
    arg_line <- c()
    for (i in seq_len(length(select_args))) {
      arg <- rlang::expr_deparse(select_args[[i]])
      # NOTE: since we could have named args we need to
      # make sure we include the argument name
      names <- names(select_args[i])
      if (nchar(names) > 0) {
        arg <- paste0(c(names, select_args[i]), collapse = " = ")
      }
      arg_char_count <- stringr::str_count(arg)
      char_count <- char_count + arg_char_count
      # if threshold has been reached either based on total char count
      # or on char count with the previous arg count, reset
      arg_line <- paste0(c(arg_line, arg), collapse = ", ")
      if (char_count >= char_threshold || (char_count + prev_count) >= char_threshold) {
        char_count <- 0
        args_list <- indent_line(args_list, arg_line)
        arg_line <- c()
      } else if (i == length(select_args)) {
        # if we've run out of args then go ahead and create the last line
        args_list <- indent_line(args_list, arg_line)
      }
      prev_count <- arg_char_count
    }
    return(
      paste0(
        c(paste0("\t", func_name, "("), args_list, "\t)"),
        collapse ="\n"
      )
    )
  }
  return(paste0("\t", rlang::expr_deparse(expr)))
}

# helper function to indent a line with args
indent_line <- function(args_list, line) {
  indented <- paste0("\t\t", line)
  paste0(c(args_list, indented), collapse = ",\n")
}

