
#' Given line order, and the reactive values, generate new intermediate outputs
#' for Unravel
#'
#' @param order numeric vector
#' @param rv reactive values
#'
#' @return \code{list(new_code_info = list(list(...)), outputs = list(...))}
#' @noRd
generate_code_info_outputs <- function(order, rv) {
  new_code_info <- rv$current_code_info[order]
  # only grab the enabled lines
  new_code_info <- Filter(
    function(x) {
      return(isTRUE(x$checked))
    },
    new_code_info
  )

  # if no lines are enabled return early
  if (length(new_code_info) == 0) {
    rv$outputs <- list()
    return(NULL)
  }

  # format the code by stripping and re-applying `%>%` / `+`
  new_code_info <- lapply(seq_len(length(new_code_info)), function(i) {
    classes <- new_code_info[[i]]$class
    code <- new_code_info[[i]]$code
    classes <- new_code_info[[i]]$class
    # replace any %>% and + at the end of the line
    code <- gsub("%>%\\s*$", "", code)
    code <- gsub("\\+\\s*$", "", code)
    new_code_info[[i]]$code <- code
    new_code_info[[i]]
  })
  # then, apply it on every line except the last
  new_code_info <- lapply(seq_len(length(new_code_info)), function(i) {
    if (i < length(new_code_info)) {
      classes <- new_code_info[[i]]$class
      new_code_info[[i]]$code <-
        if ("ggplot" %in% classes) {
          paste0(new_code_info[[i]]$code, " +")
        } else {
          paste0(new_code_info[[i]]$code, " %>%")
        }
    }
    new_code_info[[i]]
  })
  new_code_source <- paste0(lapply(new_code_info, function(x) x$code), collapse = "\n")
  quoted <- rlang::parse_expr(new_code_source)
  # message("producing new code info")

  # get new code intermediate info
  outputs <- get_output_intermediates(quoted)
  # we might run across an error while processing an invalid pipeline, for
  # this case we will get back the error message, a character, so we do this
  # silly check for now so that we return early and the UI does not know any better
  # it will just show the last error'd line
  if (!inherits(outputs, "list")) {
    return(NULL)
  }

  # update the default lineid
  outputs <- lapply(seq_len(length(outputs)), function(i) {
    outputs[[i]]$line <- new_code_info[[i]]$lineid
    outputs[[i]]
  })

  list(new_code_info = new_code_info, outputs = outputs)
}


#' Helper function that updates line information for R Shiny
#' reactive values and sends UI information to the JS side.
#'
#' @param order A numeric vector
#' @param outputs A list of outputs from `get_output_intermediates`
#' @param current_code_info The current code info list structure
#' @param new_code_info The new code info list structure
#' @param session The Shiny session
#'
#' @return
#'
#' @noRd
update_lines <- function(order, outputs, current_code_info, new_code_info, rv, session) {
  # prep data to send JS about box change type, row, and col
  new_rv_code_info <- lapply(current_code_info, function(x) {
    out <- get_output(outputs, x$lineid)
    # error occurred before this line
    if (length(out) == 0) {
      # reset states
      x$change = "invalid" # this is different from error, so we'll call it invalid
      x$row = ""
      x$col = ""
      x$summary = ""
      x$output = NULL
      x$callouts = NULL
    } else {
      # no error before this line
      new_rv <- out[[1]]
      # check if we need to update the code text for one of the editors regarding %>%
      new_code <- get_code(new_code_info, x$lineid)
      # check for error for this line
      if (!is.null(new_rv$err)) {
        # set error states
        x$code = new_code
        x$change = new_rv$change
        x$row = abbrev_num(new_rv$row)
        x$col = abbrev_num(new_rv$col)
        x$summary = new_rv$err
        x$output = NULL
        x$callouts = NULL
      } else {
        # if no error, update states
        x$code = new_code
        x$change = new_rv$change
        x$row = abbrev_num(new_rv$row)
        x$col = abbrev_num(new_rv$col)
        x$summary = new_rv$summary
        x$output = new_rv$output
        x$callouts = new_rv$callouts
      }
    }
    x
  })
  new_rv_code_info <- new_rv_code_info[order]
  send_js_code_info <- lapply(new_rv_code_info, function(x) {
    list(id = x$lineid, code = x$code, change = x$change, row = x$row, col = x$col)
  })
  # NOTE: this starts the sequence again of requesting callouts, then prompts
  session$sendCustomMessage("update_line", send_js_code_info)

  # update the summaries, callouts, and outputs as well
  rv$summaries <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), summary = x$summary))
  rv$callouts <- lapply(new_rv_code_info, function(x) list(lineid = paste0("line", x$lineid), callouts = x$callouts))
  rv$outputs <- lapply(outputs, function(x) list(id = x$line, lineid = paste0("line", x$line), output = x$output))
  rv$cur_callouts <- lapply(outputs, function(x) x$callouts)
  # update the data display to the last enabled output
  rv$current <- length(rv$outputs)
}

# helper function for grabbing a particular new code
get_code <- function(target, id) {
  result <- Filter(function(x) x$lineid == id, target)
  if (length(result) > 0) {
    return(result[[1]]$code)
  } else {
    return(result)
  }
}

# helper function for grabbing a particular new output
get_output <- function(target, lineid) {
  Filter(function(x) x$line == lineid, target)
}
