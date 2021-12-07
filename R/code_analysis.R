
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr group_vars
NULL

#' Given a quoted dplyr chain code, return a list of intermediate expressions.
#' The returned list would include the dataframe name expression.
#'
#' @param dplyr_tree quoted dplyr code
#' @param outputs list
#'
#' @return a list
recurse_dplyr <- function(dplyr_tree, outputs = list()) {
  if (inherits(dplyr_tree, "name")) {
    return(list(dplyr_tree))
  }
  # get the output of the quoted expression so far
  base <- append(list(dplyr_tree), outputs)
  if (length(dplyr_tree) < 2) {
    stop("Error: Detected a verb in pipeline that is not a function call for:<br><pre><code>", rlang::expr_deparse(dplyr_tree), "</code></pre>")
  }
  # if there are no pipes, return the tree early
  # this will return just the expression itself for single verb calls (e.g. select(diamonds, carat))
  if (!identical(dplyr_tree[[1]], as.symbol("%>%"))) {
    return(list(dplyr_tree))
  }
  return(
    append(recurse_dplyr(dplyr_tree[[2]]), base)
  )
}

#' Based on the type of tidyr/dplyr function used, return whether or not
#' the type of change was internal (no visible change), visible, or none.
#'
#' @param verb_name the \code{character} representing the function name
#'
#' @return a character
#'
#' @examples
#' \dontrun{
#' get_change_type("group_by")
#' }
#' @export
get_change_type <- function(verb_name) {
  # rn this is just a fail-safe if we for some reason have not supported the correct
  # change type based on actual data; remove it in the future when we are confident of support.
  internal_verbs <- c(
    "group_by", "rowwise"
  )
  visible_verbs <- c(
    "select", "filter", "mutate", "transmute", "summarise", "summarize", "arrange", "rename", "rename_with", "distinct",
    "spread", "gather", "pivot_wider", "pivot_longer",  "distinct", "nest", "unnest"," hoist", "unnest_longer", "unnest_wider",
    "drop_na"
  )
  if (verb_name %in% internal_verbs) {
    return("internal")
  } else if (verb_name %in% visible_verbs) {
    return("visible")
  } else {
    return("none")
  }
}

# helper function to get the type of change from previous and current dataframe
get_data_change_type <- function(verb_name, prev_output, cur_output) {
  # set the change type for summary box
  change_type <- "none"
  data_same <- identical(prev_output, cur_output)
  prev_rowwise <- inherits(prev_output, "rowwise_df")
  cur_rowwise <- inherits(cur_output, "rowwise_df")
  prev_grouped <- is_grouped_df(prev_output)
  cur_grouped <- is_grouped_df(cur_output)
  if (data_same) {
    change_type <- "none"
  } else {
    change_type <- "visible"
    # check for an internal (invisible) change
    if(!verb_name %in% c("summarize", "summarise")) {
      if (verb_name %in% c("as_tibble", "as.tibble")) {
        change_type <- "internal"
      } else if ((!prev_rowwise && cur_rowwise) || (prev_rowwise && !cur_rowwise)) {
        # rowwise case
        change_type <- "internal"
      } else if(
        (verb_name %in% c("group_by", "ungroup")) &&
        # grouped vs ungrouped or grouped vs grouped case
        ((!prev_grouped && cur_grouped) || (prev_grouped && !cur_grouped) ||
        (prev_grouped && cur_grouped && !identical(group_vars(prev_output), group_vars(cur_output))))
      ) {
        change_type <- "internal"
      }
    }
  }
  return(change_type)
}

#' Given an expression of fluent code, return a list of intermediate outputs.
#'
#' If there is an error, \code{get_output_intermediates} will return outputs up to that
#' line, with an error message for the subsequent line at fault.
#'
#' @param pipeline an \code{expression}
#'
#' TODO-refactor: make the returned object an R6 class or a structure list so we can have one location
#' for modification
#'
#' @return \code{list(
#'   intermediates = list(tibble),
#'   error = character(),
#' )}
#'
#'
#' @examples
#' \dontrun{
#' require(tidyverse)
#' "diamonds %>%
#'   select(carat, cut, color, clarity, price) %>%
#'   group_by(color) %>%
#'   summarise(n = n(), price = mean(price)) %>%
#'   arrange(desc(color))" -> pipeline
#' quoted <- rlang::parse_expr(pipeline)
#' outputs <- get_output_intermediates(quoted)
#'
#' quoted <- rlang::parse_expr("select(diamonds, carat, cut, color, clarity, price)")
#' outputs <- get_output_intermediates(quoted)
#' }
#' @export
get_output_intermediates <- function(pipeline) {
  clear_verb_summary()
  clear_callouts()
  old_verb_summary <- ""

  # check if only a name has been passed for full expression which is
  # potentially a dataframe
  if (inherits(pipeline, "name") && is.data.frame(eval(pipeline))) {
    output <- eval(pipeline)
    return(list(
      list(
        line = 1,
        code = rlang::expr_deparse(pipeline),
        change = "none",
        output = output,
        row = dim(output)[[1]],
        col = dim(output)[[2]],
        summary = paste("<strong>Summary:</strong>", tidylog::get_data_summary(output))
      )
    ))
  }

  has_pipes <- identical(pipeline[[1]], as.symbol("%>%"))
  # check if we have a dataframe as first argument for single verb code
  if (!has_pipes) {
    err <- NULL
    tryCatch(
      first_arg_data <<- is.data.frame(eval(pipeline)),
      error = function(e) {
        err <<- crayon::strip_style(e$message)
      }
    )
    if (!is.null(err)) {
      return(list(
        list(
          line = 1,
          code = rlang::expr_deparse(pipeline),
          change = "error",
          summary = paste("<strong>Summary:</strong>", err)
        )
      ))
    }
  }
  # if we don't have pipes and we don't have a function that has a first argument as dataframe
  # quit early and surface error
  if (!has_pipes && !first_arg_data) {
    # message("`pipeline` input is not a pipe call!")
    return(list(
      list(
        line = 1,
        code = rlang::expr_deparse(pipeline),
        change = "error",
        summary = "<strong>Summary:</strong> Your code does not use functions that take in a dataframe."
      )
    ))
  }

  lines <- NULL
  # first grab all of the lines as a list of of language objects
  # potentially we could error out when trying to recursive invalid pipelines (for e.g. bad order of lines)
  err <- NULL
  tryCatch({
      lines <- recurse_dplyr(pipeline)
    },
    error = function(e) {
      err <<- crayon::strip_style(e$message)
    }
  )
  # if so, just return with error message (currently not being used in front-end)
  if (!is.null(err)) {
    return(err)
  }

  results <- list()
  for (i in seq_len(length(lines))) {
    if (i != 1) {
      verb <- lines[[i]][[3]]
      verb_name <- rlang::expr_deparse(verb[[1]])
    } else if (!has_pipes && first_arg_data) {
      verb <- lines[[i]]
      verb_name <- rlang::expr_deparse(verb[[1]])
    } else {
      verb <- lines[[i]]
      verb_name <- ""
    }

    # get the deparsed character version
    # NOTE: rlang::expr_deparse breaks apart long strings into multiple character vector
    # we collapse it before further processing to avoid extra \t
    deparsed <- paste0(rlang::expr_deparse(verb), collapse = "")

    # append a pipe character %>% unless it's the last line
    if (i < length(lines)) {
      deparsed <- paste0(deparsed, " %>%")
    }
    # also append a tab character if not the first line
    if (i > 1) {
      deparsed <- paste0("\t", deparsed)
    }
    intermediate <- list(line = i, code = deparsed, change = get_change_type(verb_name))
    err <- NULL
    tryCatch({
        # store the intermediate output, and set the change type based on difference
        # between previous and current output
        change_type <- "none"
        data_changed <- FALSE
        if (i == 1) {
          intermediate["output"] <- list(eval(lines[[i]]))
          data_changed <- TRUE
        } else if (i > 1) {
          # check if the previous line had an error, and skip evaluation if it does
          if (identical(results[[i - 1]]$change, "error")) {
            intermediate["output"] <- NULL
            intermediate["change"] <- "error"
            # for now, simply point out that the previous lines have an error
            intermediate["summary"] <- paste("<strong>Summary:</strong>", "Previous lines have problems!")
            results <- append(results, list(intermediate))
            next
          }
          prev_output <- results[[i - 1]]["output"][[1]]
          # for the current line, take the previous output and feed it as the `.data`, and the rest of the args
          # NOTE: because we are not evaluating a `lhs %>% rhs()`, and only `rhs()` we miss out on the '.' pronoun
          # so this is a little hack that binds a name "." to the previous output in a new environment
          e <- rlang::new_environment(parent = rlang::current_env())
          e[["."]] <- prev_output
          # construct a call for the function such that we use the previous output as the input, and rest of the args
          call_expr <- rlang::call2(verb_name, !!!append(list(prev_output), rlang::call_args(verb)))
          # evaluate the final function call expression within the new environment that holds the "pronoun"
          cur_output <- eval(call_expr, envir = e)
          # wrap output as list so it can be stored properly
          intermediate["output"] <- list(cur_output)
          change_type <- get_data_change_type(verb_name, prev_output, cur_output)
          data_changed <- !identical(prev_output, cur_output)
        }
        # for single verb code simply get the change type based on verb for now
        if (!has_pipes && first_arg_data) {
          change_type <- get_change_type(verb_name)
        }
        # store the dimensions of dataframe
        intermediate["row"] <- dim(intermediate["output"][[1]])[[1]]
        intermediate["col"] <- dim(intermediate["output"][[1]])[[2]]
        # if the data was not a dataframe, grab the length (for now lists/vectors)
        # Note: we will need a different way to support complex types like ggplot2 objects
        if (is.null(intermediate$row)) {
          intermediate["row"] <- length(intermediate["output"][[1]])
        }

        # store the function summary
        verb_summary <- get_verb_summary()
        if(is.na(intermediate["output"])) {
          change_type <- "error"
          verb_summary <- "This step produced an `NA`."
        }
        # store the final change type
        intermediate["change"] <- change_type
        # store the column strings so we can highlight them as callouts
        intermediate["callouts"] <- list(get_line_callouts())
        # if we have a dataframe %>% verb() expression, the 'dataframe' summary is simply
        # the dataframe/tibble with dimensions reported (we could expand that if we want)
        if ((i == 1 && (has_pipes || first_arg_data)) || is.null(verb_summary)) {
          verb_summary <- tidylog::get_data_summary(intermediate["output"][[1]])
        }
        # store the final function summary and set it to empty string if we do not yet have
        # a summary support for the function
        intermediate["summary"] <-
          ifelse(
            data_changed,
            paste("<strong>Summary:</strong>", verb_summary),
            ""
          )
        old_verb_summary <- verb_summary
      },
      error = function(e) {
        err <<- e
      }
    )
    # if we have an error, strip the crayon formatting and store the error message
    if (!is.null(err)) {
      # Thought: we could make even more readable messages
      # Error: Must group by variables found in `.data`.
      # * Column `colorr` is not found.
      # for e.g. we could replace the `.data` with the actual expression
      intermediate[["change"]] <- "error"
      msg <- ifelse(
        nzchar(err$message),
        crayon::strip_style(err$message),
        crayon::strip_style(paste0(err))
      )
      msg <- gsub("Error:", "<strong>Error:</strong>", msg)
      msg <- ifelse(!grepl("Error:", msg), paste("<strong>Error:</strong>", msg), msg)
      # try to retain the format as much as possible by keeping it as HTML string
      # style back the x's, i's, and *
      msg <- gsub("\nx", "<br><span style='color:red'>x</span>", msg)
      msg <- gsub("\n\u2139", "<br><span style='color:DodgerBlue'>\u2139</span>", msg)
      msg <- gsub("\n\\*", "<br>*", msg)
      intermediate[["err"]] <- msg
    }
    results <- append(results, list(intermediate))
  }

  return(results)
}

#' Given an tidyverse code expression, this will return a list of all of the outputs.
#' It calls \code{get_output_intermediates} to return a simpler representation of all
#' of the intermediate outputs. We simply extract the outputs only from the larger
#' intermediate data structure.
#'
#' @param pipeline tidyverse code expression
#'
#' @return a list of outputs corresponding to each verb in tidyverse code
#'
#'
#' @examples
#' \dontrun{
#' require(tidyverse)
#' quoted <- rlang::parse_expr(
#' diamonds %>%
#'   select(carat, cut, color, clarity, price) %>%
#'   group_by(color) %>%
#'   summarise(n = n(), price = mean(price)) %>%
#'   arrange(desc(color))
#' )
#' outputs <- get_chain_outputs(quoted)
#' }
#' @export
get_chain_outputs <- function(pipeline) {
  all_intermediates <- get_output_intermediates(pipeline)
  only_outputs <- list()
  return(
    lapply(all_intermediates, function(intermediate) {
      intermediate$output
    })
  )
}

