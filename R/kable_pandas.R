library(magrittr)
library(kableExtra)
library(reticulate)
library(tidyverse)

source(here::here("R/utils.R"))

modified_color <- "#c6c0fd"
inserted_color <- "#c8ff9e"
deleted_color <- "#fea5a3"

#' Takes in an unconverted Python dataframe and outputs a kableExtra table.
#'
#' @param df The unocvertend Python dataframe
#' @param rmd This is a flag to render in the RStudio IDE for TRUE, FALSE in a rendered document
#' @param show_rownames A flag to indicate whether to show the rownames for MultiIndex dataframes
#' @param rows a list structure
#' list(
#         spec = 3:7,
#         color = "black",
#         background = inserted_color
#       )
#' @param cols same as `rows`
#' @param col_labels same as `col_labels`
#'
#' @return A character vector of the table source code (very similar to `kable`)
#' @export
#'
#' @examples
kable_pandas <- function(
                         df,
                         rmd = FALSE,
                         show_rownames = FALSE,
                         rows = list(),
                         cols = list(),
                         col_labels = list()) {
  is_multi_index <- class(df$index)[[1]] == "pandas.core.indexes.multi.MultiIndex"

  # to curb performance costs, let's slice
  if (df$shape[[1]] > 100 && is_multi_index) {
    # unfortunately, we don't seem to have the ability to use slices
    # cleanly with `reticulate` yet, this hack assumes your dataframe with be
    # named `df` ... yikes! but works!
    df <- reticulate::py_eval("df.loc[slice(None, ), ].iloc[:20,]")
  } else if (df$shape[[1]] > 100) {
    df <- df$head(20)
  }

  # setup an R dataframe based on `df`
  rdf <- python_df(df)

  # grab the regular columns not part of the Index
  columns <- as.list(df$columns$values)
  # then grab the Index columns
  rdf_column_names <- colnames(rdf)
  row_index_column_names <- rdf_column_names[!(rdf_column_names %in% columns)]

  # if it's a MultiIndex, mock the added space underneath regular columns
  if (is_multi_index) {
    rdf <- dplyr::rename_with(rdf, ~ paste0(.x, "<h2></h2>"), !dplyr::any_of(row_index_column_names))
  }

  # apply any annotations there might be
  setup_kbl <- annotate_dataframe(rdf, show_rownames, rows, cols, col_labels)

  # if in RStudio IDE using rmd interactively, use `kable_paper`
  if (rmd) {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_paper(full_width = T) %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "hover", "responsive"), position = "center")
  } else {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "hover", "responsive"), position = "center")
  }

  # if MultiIndex try collapsing the rows of first column
  if (is_multi_index) {
    setup_kbl <- setup_kbl %>%
      kableExtra::collapse_rows(columns = 1, valign = "top")
  }

  # add scroll box
  setup_kbl %>%
    kableExtra::scroll_box(width = "100%", height = "400px")
}

#' Annotate a dataframe and return a kableExtra::kbl with any styling there might be for
#' rows, columns, and column labels.
#'
#' @param df
#' @param show_rownames
#' @param rows
#' @param cols
#' @param col_labels
#'
#' @return
#' @export
#'
#' @examples
annotate_dataframe <- function(df, show_rownames, rows = list(), cols = list(), col_labels = list()) {
  # TODO handle list of lists for each rows/cols/col_labels
  # if there are any cols annotation apply those first
  if (length(col_labels)) {
    df <- dplyr::rename_with(
      df,
      ~ kableExtra::cell_spec(.x, "html", color = col_labels$color, background = col_labels$background),
      col_labels$spec
    )
  }

  # create base kbl
  base_kbl <- kableExtra::kbl(df, format = "html", align = "l", escape = F, row.names = show_rownames)

  # then, the row specs
  if (length(rows)) {
    base_kbl <- kableExtra::row_spec(base_kbl, rows$spec, color = rows$color, background = rows$background)
  }
  # then, the column specs
  if (length(cols)) {
    if (!length(col_labels)) {
      df <- dplyr::rename_with(
        df,
        ~ kableExtra::cell_spec(.x, "html", color = cols$color, background = cols$background),
        cols$spec
      )
    }
    base_kbl <- kableExtra::column_spec(base_kbl, cols$spec, color = cols$color, background = cols$background)
  }

  return(base_kbl)
}
