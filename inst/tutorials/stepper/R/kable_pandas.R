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
#' @param show_rownames Whether to display rownames or not
#' @param callouts  `callout_`* list structures
#'
#' @return A character vector of the table source code (very similar to `kable`)
#' @export
#'
#' @examples
kable_pandas <- function(df, rmd = FALSE, show_rownames = FALSE, callouts) {
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
  setup_kbl <- annotate_dataframe(rdf, show_rownames, callouts)

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

apply_col_labels_styles <- function(df, callouts) {
  # if there are any `callout_col_lables`, apply the styles to the column labels of `df`
  col_label_styles <- lapply(callouts, function(x) if (inherits(x, "callout_col_labels")) x)
  col_label_styles <- col_label_styles[lengths(col_label_styles) != 0]
  for (cl in col_label_styles) {
    df <- dplyr::rename_with(
      df,
      ~ kableExtra::cell_spec(.x, "html", color = cl$color, background = cl$background),
      cl$spec
    )
  }
  df
}

apply_cols_styles <- function(df, show_rownames, callouts) {
  # TODO apply column label styling as well
  # col_label_styles <- lapply(callouts, function(x) if (inherits(x, "callout_col_labels")) x)
  # col_label_styles <- col_label_styles[lengths(col_label_styles) != 0]
  # df <- apply_col_labels_styles(df, list(callout_col_labels(cols_styles[[1]])))

  # create base kbl
  base_kbl <- kableExtra::kbl(df, format = "html", align = "l", escape = F, row.names = show_rownames)
  # then, apply column styles
  cols_styles <- lapply(callouts, function(x) if (inherits(x, "callout_cols")) x)
  cols_styles <- cols_styles[lengths(cols_styles) != 0]
  for (c in cols_styles) {
    base_kbl <- kableExtra::column_spec(base_kbl, c$spec, color = c$color, background = c$background)
  }
  base_kbl
}

apply_rows_styles <- function(kbl_input, callouts) {
  rows_styles <- lapply(callouts, function(x) if (inherits(x, "callout_rows")) x)
  rows_styles <- rows_styles[lengths(rows_styles) != 0]
  for (r in rows_styles) {
    kbl_input <- kableExtra::row_spec(kbl_input, r$spec, color = r$color, background = r$background)
  }
  kbl_input
}

#' Annotate a dataframe and return a kableExtra::kbl with any styling there might be for
#' rows, columns, and column labels.
#'
#' @param df The unocvertend Python dataframe
#' @param show_rownames A flag to indicate whether to show the rownames for MultiIndex dataframes
#' @param callouts `callout_`* list structures
#'
#' @return
#' @export
#'
#' @examples
annotate_dataframe <- function(df, show_rownames, callouts) {
  # if there are no annotations just create base kbl and return it
  if (length(callouts) == 0)
    return(kableExtra::kbl(df, format = "html", align = "l", escape = F, row.names = show_rownames))

  # if there are any `callout_col_lables`, apply the styles to the column labels of `df`
  df <- apply_col_labels_styles(df, callouts)

  # then, apply column specs
  base_kbl <- apply_cols_styles(df, show_rownames, callouts)

  # then, the row specs
  final_kbl <- apply_rows_styles(base_kbl, callouts)

  return(final_kbl)
}
