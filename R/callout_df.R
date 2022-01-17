
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

get_list_columns <- function(data) {
  names(Filter(x = data, f = function(x) is.list(x)))
}

get_list_col_styling <- function(data) {
  list_cols <- get_list_columns(data)
  all_styles <- list()
  common_styles <- list(
    compact = TRUE,
    highlight = TRUE,
    defaultPageSize = 5
  )
  for (lc in list_cols) {
    # print(lc)
    all_styles[lc] <- list(colDef(
        cell = function(value) {
          # tibble formatting e.g. <int>
          return(glue::glue("<{pillar::tbl_sum(value)}>"))
        },
        details = function(index) {
          value <- data[[lc]][[index]]
          if (identical(typeof(value), "list")) {
            htmltools::div(
              reactable::reactable(
                as.data.frame(value),
                compact = TRUE,
                highlight = TRUE,
                defaultPageSize = 5
              )
            )
          } else  {
            htmltools::div(
              reactable::reactable(
                as.data.frame(list(value)),
                compact = TRUE,
                highlight = TRUE,
                defaultPageSize = 5,
                defaultColDef = colDef(
                  html = TRUE,
                  header = function(value) htmltools::HTML(lc)
                )
              )
            )
          }
        }
      )
    )
  }
  all_styles
}

#' Constructs the column types and the cell rendering style for list columns.
#'
#'
#' @param data A `data.frame` / `tibble`
#'
#' @return A list of `reactable::colDef`
#'
#' @examples
#' df_list <- tibble::tibble(
#'   id=1:2,
#'   comment=list(c("michele", Sys.time(), "hello"),
#'                c("michele", Sys.time(), "world")))
#'
#' df_list <- tibble::tibble(id=1:10, comment=list(c(rep("michele", 10))))
#'
#' library(gapminder)
#' df_list <-
#'   gapminder %>%
#'   filter(country == 'Afghanistan') %>%
#'   group_by(country) %>%
#'   nest()
#'
#' reactable(
#'   as.data.frame(df_list),
#'   columns = get_col_type_headers(df_list)
#' )
#' @noRd
get_common_styles <- function(data) {
  column_type <- function(value) {
    htmltools::tagList(
      htmltools::div(value),
      htmltools::div(
        style = list(color = "grey"),
        htmltools::div(
          paste0("<", vctrs::vec_ptype_abbr(data[[value]]), ">")
        )
      )
    )
  }
  ls_styling <- get_list_col_styling(data)
  type_styling <- lapply(data, function(column) {
    colDef(
      header = column_type,
      na = "NA"
    )
  })
  common_style <- modifyList(type_styling, ls_styling)
  return(common_style)
}

#' Helper function that returns the column, css pairs for callout words given data
#'
#' @param data data.frame or tibble
#' @param callout_words A list of lists of callout word and change type
#'   e.g., list(list(word = "carat", change = "internal-change"), list(word = "cut", change = "visible-change"))
#'
#' @return a list structure to supply for reactable(columns = ...)
#'
#' @examples
#' get_column_css(mtcars %>% group_by(cyl), list(list(word = "cyl", change = "internal-change")))
#' @noRd
get_column_css <- function(data, callout_words = list()) {
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

  # then, apply any callout css needed if there are callouts
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

