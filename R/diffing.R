
#' Given knitr chunk options for exercise, return the daff output.
#'
#' @param options
#'
#' @return
#' @export
#'
#' @examples
get_daff_output <- function(options) {
  include_summary <- ifelse(is.null(options$summary), FALSE, options$summary)
  include_diff_table <- ifelse(is.null(options$diff_table), FALSE, options$diff_table)
  diff_out <- data_diff(include_summary = include_summary, include_diff_table = include_diff_table)
  # TODO to build this summary programmatically
  # - it's possible to list out column variables that were pivoted
  # - it's possible to insert summary numbers of added, deleted, modified_color, reordered rows/cols/cells
  # - it's possible to make a statement about how there were no changes in any change category
  summary_statement <-
    "All **9** `income` variables (`<$10K-20K, ... , 150K, Don't Know/refused`) have been deleted (red) and
    have been \"molten\" into column `variable` (green), which added **162** rows. In other words,
    we changed the data from a wide format to a long format. We weren't modifying any
    existing values so there are no modified_color/reordered rows/columns or cells."
  return(c("<br>", "**Data Diff Summary:**<br>", summary_statement, "<br>", diff_out))
}

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
  modified_color <- "#a0a0ff"
  inserted_color <- "#74ff74"
  deleted_color <- "#ff7374"

  # TODO: programmatically get the old and new data set to compare
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
      ~ " ", ~ "#", ~Modified, ~Reordered, ~Deleted, ~Added,
      "Rows", s$row_count_change_text, s$row_updates, s$row_reorders, s$row_deletes, s$row_inserts,
      "Cols", s$col_count_change_text, s$col_updates, s$col_reorders, s$col_deletes, s$col_inserts,
    )

    reactable_table <-
      reactable::reactable(summary_tbl, columns = list(
        Modified = reactable::colDef(
          headerStyle = list(background = modified_color),
          style = function(value) {
            list(background = modified_color)
          }
        ),
        Deleted = reactable::colDef(
          headerStyle = list(background = deleted_color),
          style = function(value) {
            list(background = deleted_color)
          }
        ),
        Added = reactable::colDef(
          headerStyle = list(background = inserted_color),
          style = function(value) {
            list(background = inserted_color)
          }
        )
      ),
      bordered = TRUE
    )

    summary_table_html <- htmltools::knit_print.shiny.tag.list(reactable_table)
  }

  diff_table_html <- ""
  if (include_diff_table) {

    #### Now render the data diff table
    library(dplyr)

    # Note: this file will only be used in the temporary dir for the
    # learnr evaluation stage
    path <- "patch.csv"
    daff::write_diff(diff, path)
    path <- here::here("inst/tutorials/data_diff/patch.csv")
    diff_data <- readr::read_csv(path)
    diff_data <- as.data.frame(diff_data)

    # TODO: modified rows/columns

    # 1) get all rows that have been inserted
    row_vectors <- 1:length(rownames(diff_data))
    inserted_rows <- row_vectors[diff_data$`!` == "+++"]

    # TODO: inserted rows

    # 2) get all cols that have been deleted
    col_vectors <- 1:length(colnames(diff_data))

    deleted_col_names <- diff_data %>%
      select(starts_with("---")) %>%
      colnames()
    deleted_cols <- col_vectors[colnames(diff_data) %in% deleted_col_names]


    # 3) get inserted cols
    inserted_col_names <- diff_data %>%
      select(starts_with("+++")) %>%
      colnames()
    inserted_cols <- col_vectors[colnames(diff_data) %in% inserted_col_names]

    # 4) get rid of daff-related schema row / cols

    # get rid of top daff row
    colnames(diff_data) <- diff_data[1, ]
    inserted_rows <- inserted_rows - 1

    # get rid of @@ column
    diff_data$`@@` <- NULL
    # get rid of daff-related ! column
    diff_data <- diff_data[-1, ]
    inserted_cols <- inserted_cols - 1
    deleted_cols <- deleted_cols - 1

    # wizard of oz python indexing
    rownames(diff_data) <- vapply(
      rownames(diff_data),
      # 2 since we deleted a row (schema row)
      function(x) as.character(as.numeric(x) - 2),
      character(1)
    )

    format_columns <- function(columns, change_type) {
      col_color <- switch(
          change_type,
          inserted = inserted_color,
          deleted = deleted_color,
          modified = modified_color
      )
      rtn <- list()
      column_names <- names(columns)
      range <- 1:length(columns)
      for(i in seq_along(range)) {
        rtn[[column_names[i]]] <- reactable::colDef(
          headerStyle = list(background = col_color),
          style = function(value) {
            list(background = col_color)
          }
        )
      }
      return(rtn)
    }
    # diff_data[, inserted_cols, drop=FALSE]
    # TODO add other defs
    inserted_column_defs <- format_columns(diff_data[, inserted_cols, drop=FALSE], "inserted")
    deleted_column_defs <- format_columns(diff_data[, deleted_cols, drop=FALSE], "deleted")

    # construct reactable
    reactable_table <-
      reactable::reactable(diff_data,
        pagination = FALSE,
        height = 300,
        rowStyle = function(index) {
          if (index %in% inserted_rows) {
            list(background = inserted_color)
          }
        },
        columns = append(inserted_column_defs, deleted_column_defs),
        rownames = TRUE,
        bordered = TRUE,
        resizable = TRUE
      )
    # knit_print for rendering
    diff_table_html <- htmltools::knit_print.shiny.tag.list(reactable_table)
  }

  return(c(summary_table_html, "<br>", diff_table_html))
}
