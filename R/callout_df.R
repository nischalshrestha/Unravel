
# global definition of change type css
none_change_css <- "background-color: white;"
internal_change_css <- "background-color: lightblue;"
visible_change_css <- "background-color: #fff1ce;"

# helper function that returns the css for each type of change
get_change_css <- function(change) {
  if (identical(change, "visible-change")) {
    return(visible_change_css)
  } else if (identical(change, "internal-change")) {
    return(internal_change_css)
  }
  return("")
}

#' Helper function that reutnrs the column, css pairs for callout words given data
#'
#' @param data data.frame or tibble
#' @param callout_words A list of lists of callout word and change type
#'   e.g., list(list(word = "carat", change = "internal-change"), list(word = "cut", change = "visible-change"))
#'
#' @return a list structure to supply for reactable(columns = ...)
#' @export
#'
#' @examples
get_column_css <- function(data, callout_words) {
  if (length(callout_words) < 1) {
    return(list())
  }

  columns <- names(data)
  column_positions <- seq_len(length(columns))

  callout_words <- Map(
    function(i, x) {
      x[["pos"]] <- column_positions[columns %in% x$word]
      x
    },
    seq_along(callout_words),
    callout_words
  )

  last_pos <- NULL
  columns_css <- list()
  for (i in seq_len(length(callout_words))) {
    c <- callout_words[[i]]
    cur_column <- list()
    border_left_css <- ""
    # know when to not have a left border if a column is right next to another
    if (isTRUE(c$pos - 1 != last_pos) || is.null(last_pos)) {
      border_left_css <- "border-left: 1px dashed black;"
    }
    # construct the css
    change_css <-
      glue::glue(
        "{border_left_css}",
        "border-right: 1px dashed black;",
        "{get_change_css(c$change)}",
        .sep = "\n"
      )
    # construct the column css info
    cur_column[[c$word]] <- colDef(
      headerStyle = paste0(change_css, "border-top: 1px dashed black;", collapse = "\n"),
      style = change_css
    )
    columns_css <- append(columns_css, cur_column)
    # update last callout column's position
    last_pos <- c$pos
  }
  columns_css
}

# final_data <- diamonds %>% group_by(color)
# col_defs <- get_column_css(
#   data = final_data,
#   callout_words = list(
#     # list(word = "carat", change = "internal-change"),
#     list(word = "color", change = "internal-change")
#     # list(word = "cut", change = "internal-change")
#   )
# )
#
# # we should automatically rearrange data if grouping
# if (is_grouped_df(final_data)) {
#   reactable(
#     data = select(.data = final_data, group_vars(final_data), everything()) %>% as.data.frame(),
#     rownames = TRUE,
#     # we can do a custom thing for a particular column
#     columns = DataTutor:::reappend(
#       list(.rownames = colDef(style = list(textAlign = "left"), maxWidth = 80)),
#       col_defs
#     )
#   )
# } else {
#   reactable(
#     data = final_data,
#     # we can do a custom thing for a particular column
#     columns = col_defs
#   )
# }
