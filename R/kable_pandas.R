library(magrittr)
library(kableExtra)
library(reticulate)
library(tidyverse)

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
  row_index_column_names <- rdf_column_names[!(rdf_column_names %in% columns)]

  # if it's a MultiIndex, mock the added space underneath regular columns
  if (is_multi_index) {
    rdf <- dplyr::rename_with(rdf, ~ paste0(.x, "<h2></h2>"), !dplyr::any_of(row_index_column_names))
  }

  # base kbl
  setup_kbl <- rdf %>%
    # this retains color for the row Index columns
    kableExtra::kbl(format = "html", align = "l", escape = F, row.names = show_rownames)

  # if in RStudio IDE using rmd interactively, use `kable_paper`
  if (rmd) {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_paper(full_width = T) %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "hover", "responsive"), position = "center")
  } else {
    setup_kbl <- setup_kbl %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "hover", "responsive"), position = "center")
  }

  setup_kbl %>%
    # kableExtra::row_spec(0, extra_css ="height: 100px; ") %>%
    kableExtra::scroll_box(width = "100%", height = "400px")
}

