
#' Takes in knitr chunk options and uses the previous dataframe and the
#' current one to generate a data diff.
#'
#' @param code 
#' @param include_summary 
#' @param include_diff_table 
#'
#' @return
#' @export
#'
#' @examples
data_diff <- function(code, include_summary = FALSE, include_diff_table = TRUE) {

  # colors for data diffing
  modified <- "#a0a0ff"
  green <- "#74ff74"
  red <- "#ff7374"
  
  old_pew <- as.data.frame(py$pew)
  new_pew <- as.data.frame(py$pew_long)
  diff <- daff::diff_data(old_pew, new_pew)
  
  summary_table_html <- ""
  
  if (include_summary) {
    #### Render the data diff summary table
    s <- daff:::summary.data_diff(diff)
    
    # Note: there are all the summaries that are available to us:
    # > ls(s)
    #  [1] "col_count_change_text"         "col_count_final"               "col_count_initial"
    #  [4] "col_deletes"                   "col_inserts"                   "col_renames"
    #  [7] "col_reorders"                  "col_updates"                   "data"
    # [10] "row_count_change_text"         "row_count_final"               "row_count_final_with_header"
    # [13] "row_count_initial"             "row_count_initial_with_header" "row_deletes"
    # [16] "row_inserts"                   "row_reorders"                  "row_updates"
    # [19] "source_name"                   "target_name"
    # These are the main ones we care about
    summary_tbl <- tibble::tribble(
      ~ "", ~ "#", ~Modified, ~Reordered, ~Deleted, ~Added,
      "Rows", s$row_count_change_text, s$row_updates, s$row_reorders, s$row_deletes, s$row_inserts,
      "Cols", s$col_count_change_text, s$col_updates, s$col_reorders, s$col_deletes, s$col_inserts,
    )
    
    # summary table
    summary_table_html <-
      kableExtra::kbl(summary_tbl) %>%
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "responsive")) %>%
      kableExtra::column_spec(3:4, background = modified) %>%
      kableExtra::column_spec(5, background = red) %>%
      kableExtra::column_spec(6, background = green)
  }
  
  diff_table_html <- ""
  if (include_diff_table) {
    
    #### Now render the data diff table
    library(dplyr)
    
    # Note: this file will only be used in the temporary dir for the
    # learnr evaluation stage
    daff::write_diff(diff, "patch.csv")
    diff_data <- readr::read_csv("patch.csv")
    diff_data <- as.data.frame(diff_data)
    
    # TODO: incorporate modified as well
    
    # get inserted rows
    row_vectors <- 1:length(rownames(diff_data))
    inserted_rows <- row_vectors[diff_data$`!` == "+++"]
    
    # get deleted cols
    deleted_col_names <- diff_data %>%
      select(starts_with("---")) %>%
      colnames()
    col_vectors <- 1:length(colnames(diff_data))
    deleted_cols <- col_vectors[colnames(diff_data) %in% deleted_col_names]
    
    # get inserted cols
    inserted_col_names <- diff_data %>%
      select(starts_with("+++")) %>%
      colnames()
    inserted_cols <- col_vectors[colnames(diff_data) %in% inserted_col_names]
    
    # get rid of daff-related schema row
    cols_to_add <- diff_data[1, 2:length(colnames(diff_data))]
    
    colnames(diff_data) <- diff_data[1, ]
    # get rid of daff-related ! column
    diff_data$`@@` <- NULL
    diff_data <- diff_data[-1, ]
    # wizard of oz python indexing
    rownames(diff_data) <- vapply(
      rownames(diff_data), 
      function(x) as.character(as.numeric(x) - 2), 
      character(1)
    )
    
    # TODO: see if you can also highlight column parts
    
    # TODO: instead of NULL for the columns that are deleted while overlapped with inserted rows, 
    # maybe we could replace with empty string
    # render a pretty printed table with highlighted modified, inserted, deleted rows/columns
    diff_table_html <- kableExtra::kbl(diff_data) %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "responsive")) %>%
      kableExtra::row_spec(inserted_rows - 1, background = green) %>%
      kableExtra::column_spec(inserted_cols, background = green) %>%
      kableExtra::column_spec(deleted_cols, background = red) %>%
      kableExtra::scroll_box(width = "100%", height = "400px")
  }
  
  return(c(summary_table_html, diff_table_html))
}