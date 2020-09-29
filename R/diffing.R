
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
  diff_out <- daff_html(options, include_summary = include_summary, include_diff_table = include_diff_table)
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
daff_html <- function(options, include_summary = FALSE, include_diff_table = FALSE) {
  # colors for data diffing
  modified_color <- "#a0a0ff"
  inserted_color <- "#74ff74"
  deleted_color <- "#ff7374"

  # TODO: programmatically get the old and new data set to compare
  old_pew <- as.data.frame(py$pew)
  new_pew <- as.data.frame(py$pew_long)

  # new_pew <- as.data.frame(py$pew_long)
  diff <- daff::diff_data(old_pew, new_pew)
  s <- attr(diff, "summary")

  summary_table_html <- ""
  if (include_summary) {
    #### Render the data diff summary table
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
    # path <- here::here("inst/tutorials/data_diff/patch.csv")
    diff_data <- as.data.frame(readr::read_csv(path))

    # note if we changed the header's schema
    has_schema_change <- !is.null(diff_data$`!`)

    # 1) get all rows that have been inserted / modified
    row_vectors <- 1:length(rownames(diff_data))
    inserted_rows <-
      if (has_schema_change) {
        row_vectors[diff_data$`!` == "+++"]
      } else {
        rows = row_vectors[diff_data$`@@` == "+++"]
        rows[!is.na(rows)]
      }

    modified_rows <-
      if (has_schema_change) {
        # print('woa')
        row_vectors[diff_data$`!` == "->"]
      } else {
        rows = row_vectors[diff_data$`@@` == "->"]
        rows[!is.na(rows)]
      }
    # TODO: deleted rows

    # 2) get all cols that have been deleted / inserted
    col_vectors <- 1:length(colnames(diff_data))

    # deleted
    deleted_col_names <- diff_data %>%
      select(starts_with("---")) %>%
      colnames()
    deleted_cols <- col_vectors[colnames(diff_data) %in% deleted_col_names]

    # inserted cols
    inserted_col_names <- diff_data %>%
      select(starts_with("+++")) %>%
      colnames()
    inserted_cols <- col_vectors[colnames(diff_data) %in% inserted_col_names]

    # Note: we won't do modify until we get to cell-level when detecting ->

    # 3) get rid of daff-related schema row / cols

    # get rid of top daff row

    colnames(diff_data) <-
      if (has_schema_change) {
        inserted_rows <- inserted_rows - 1
        diff_data[1, ]
      } else {
        colnames(diff_data)
      }

    # get rid of @@ column
    diff_data$`@@` <- NULL
    col_vectors <- col_vectors[-length(col_vectors)]
    # get rid of daff-related ! column (if any)
    diff_data <-
      if (has_schema_change) {
        inserted_cols <- inserted_cols - 1
        deleted_cols <- deleted_cols - 1
        diff_data[-1, ]
      } else {
        diff_data
      }

    # wizard of oz python indexing
    rownames(diff_data) <- 1:nrow(diff_data) - 1

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
        if (identical(change_type, "modified")) {
          rtn[[column_names[i]]] <- reactable::colDef(
            headerStyle = list(background = col_color),
            style = function(value) {
              # we will need to use something like this to color cell for rows
              if (grepl("->", value))
                return(list(background = col_color))
            }
          )
        } else {
          rtn[[column_names[i]]] <- reactable::colDef(
            headerStyle = list(background = col_color),
            style = function(value) {
              if (identical(value, "NULL")) {
                print(value)
                return(list(background = col_color, color = col_color))
              }
              return(list(background = col_color))
            }
          )
        }
      }
      return(rtn)
    }

    # collect column definitions for reactable for styling
    modified_cols <- col_vectors[!(col_vectors %in% c(inserted_cols, deleted_cols))]
    modified_column_defs <- format_columns(diff_data[, modified_cols, drop=FALSE], "modified")
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
        defaultColDef = reactable::colDef(
          headerStyle = list(background = modified_color)
        ),
        columns = append(append(modified_column_defs, inserted_column_defs), deleted_column_defs),
        rownames = TRUE,
        bordered = TRUE,
        resizable = TRUE
      )
    # knit_print for rendering
    diff_table_html <- htmltools::knit_print.shiny.tag.list(reactable_table)
  }

  return(c(summary_table_html, "<br>", diff_table_html))
}
