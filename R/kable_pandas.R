library(magrittr)
library(tidyverse)
library(kableExtra)
library(reticulate)

source(here::here("R/utils.R"))

#' Takes in an unconverted Python dataframe and outputs a kableExtra table.
#'
#' @param df The unocvertend Python dataframe
#' @param rmd This is a flag to render in the RStudio IDE for TRUE, FALSE in a rendered document
#' @param show_rownames A flag to indicate whether to show the rownames for MultiIndex dataframes
#'
#' @return A character vector of the table source code (very similar to `kable`)
#' @export
#'
#' @examples
kable_pandas <- function(df, rmd = FALSE, show_rownames = FALSE) {
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
  row_index_cols <- rdf_column_names[!(rdf_column_names %in% columns)]

  # this is the amount of space for the Index columns for grouping
  idx_column_space <- sum(!(colnames(rdf) %in% columns))
  # this is the MultiIndex column we want to use for `kableExtra::collapse_rows`
  # by default, it's the first, but will be the second if we want to show rownames (indices)
  collapse_column <- 1
  if (show_rownames) {
    idx_column_space <- idx_column_space + 1
    collapse_column <- collapse_column + 1
  }
  # this is the space for the rest of columns for grouping
  column_space <- rep(1, length(columns))
  names(column_space) <- columns

  # base kbl
  setup_kbl <- rdf %>%
    # this retains color for the row Index columns
    dplyr::rename_with(function(x) kableExtra::cell_spec(x, "html", color = "black"), dplyr::any_of(row_index_cols)) %>%
    kableExtra::kbl(format = "html", align = "l", escape = F, row.names = show_rownames)

  if (rmd) {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_paper(full_width = T) %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "hover", "responsive"), position = "center")
  } else {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "hover", "responsive"), position = "center")
  }

  final_kbl <- setup_kbl
  # check if it is a MultiIndex, `reticulate` will list other classes but the very first
  # one is the Python class
  if (is_multi_index) {
    # TODO a check to see if we have many more levels in which case we have to stack
    # the column levels and sometimes above the row indices. In such a case, we do
    # not "raise" the columns and can skip coloring rows white

    # if so, we "raise" the regular columns by making the columns invisible
    # and moving them to a header above
    final_kbl <-
      setup_kbl %>%
      kableExtra::row_spec(0, color = "white") %>%
      kableExtra::add_header_above(c(" " = idx_column_space, column_space), align = "c", bold = T, line = F) %>%
      kableExtra::collapse_rows(columns = collapse_column, valign = "top")
  }

  final_kbl %>%
    kableExtra::scroll_box(width = "100%", height = "400px")
}
