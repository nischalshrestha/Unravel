
#### Constants
# colors for data diffing
modified_color <- "#a0a0ff"
inserted_color <- "#74ff74"
deleted_color <- "#ff7374"

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
  # TODO need to rethink how to get the data_ref and data to compare for daff so we can pass them
  diff_out <- daff_html(
    options,
    include_summary = include_summary,
    include_diff_table = include_diff_table
  )
  # TODO to build this summary programmatically
  # - it's possible to list out column variables that were pivoted
  # - it's possible to insert summary numbers of added, deleted, modified_color, reordered rows/cols/cells
  # - it's possible to make a statement about how there were no changes in any change category
  # - what else could we automate?!
  summary_statement <-
    "All **9** `income` variables (`<$10K-20K, ... , 150K, Don't Know/refused`) have been deleted (red) and
    have been \"molten\" into column `variable` (green), which added **162** rows. In other words,
    we changed the data from a wide format to a long format. We weren't modifying any
    existing values so there are no modified_color/reordered rows/columns or cells."
  return(c("<br>", "**Data Diff Summary:**<br>", summary_statement, "<br>", diff_out))
}

#' Takes a daff summary table to extract pertinent information and returns a
#' customized table visualizing the changes.
#'
#' @param s "data_diff_summary"
#'
#' @return knit_print
#'
#' @examples
diff_summary_html <- function(s, reactable_output = FALSE) {
  # Note: there are all the summaries that are available to us:
  # > ls(s)
  #  [1] "col_count_change_text"         "col_count_final"               "col_count_initial"
  #  [4] "col_deletes"                   "col_inserts"                   "col_renames"
  #  [7] "col_reorders"                  "col_updates"                   "data"
  # [10] "row_count_change_text"         "row_count_final"               "row_count_final_with_header"
  # [13] "row_count_initial"             "row_count_initial_with_header" "row_deletes"
  # [16] "row_inserts"                   "row_reorders"                  "row_updates"
  # [19] "source_name"                   "target_name"
  s <- purrr::map_df(s, ~ as.character(.))
  # These are the main ones we care about
  summary_tbl <- tibble::tribble(
    ~" ", ~"#", ~Modified, ~Reordered, ~Deleted, ~Added,
    "Rows", s$row_count_change_text, s$row_updates, s$row_reorders, s$row_deletes, s$row_inserts,
    "Cols", s$col_count_change_text, s$col_updates, s$col_reorders, s$col_deletes, s$col_inserts,
  )

  reactable_table <-
    reactable::reactable(summary_tbl,
      columns = list(
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
  if (reactable_output) {
    return(reactable_table)
  }
  # return knit_print for rendering
  htmltools::knit_print.shiny.tag.list(reactable_table)
}

#' Takes the diff information from daff and reconstructs its information to consume
#' later for custom formatting.
#'
#' @param diff
#'
#' @return list with data, and vectors of inserted/modified/deteleted rows/cols
#' list(
#'   data = diff_data,
#'   inserted_rows = inserted_rows,
#'   inserted_cols = inserted_cols,
#'   modified_rows = modified_rows,
#'   deleted_cols = deleted_cols
#' )
#' @examples
construct_diff_data <- function(diff) {
  library(dplyr)
  # TODO reordered rows/columns
  # 0) get a dataframe form of diff
  # Note: this file will only be used in the temporary dir for the
  # learnr evaluation stage
  path <- "patch.csv"
  daff::write_diff(diff, path)
  # path <- here::here("inst/tutorials/data_diff/patch.csv") # for testing within function
  diff_data <- as.data.frame(readr::read_csv(path))
  # cat("diff_data:\n")
  # print(diff_data)
  # note if we changed the header's schema
  has_schema_change <- !is.null(diff_data$`!`)

  # 1) get all rows that have been inserted / deleted / modified
  row_vectors <- 1:length(rownames(diff_data))
  inserted_rows <-
    if (has_schema_change) {
      row_vectors[diff_data$`!` == "+++"]
    } else {
      rows <- row_vectors[diff_data$`@@` == "+++"]
      rows[!is.na(rows)]
    }

  deleted_rows <-
    if (has_schema_change) {
      row_vectors[diff_data$`!` == "---"]
    } else {
      rows <- row_vectors[diff_data$`@@` == "---"]
      rows[!is.na(rows)]
    }

  modified_rows <-
    if (has_schema_change) {
      row_vectors[diff_data$`!` == "->"]
    } else {
      rows <- row_vectors[diff_data$`@@` == "->"]
      rows[!is.na(rows)]
    }

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
      deleted_rows <- deleted_rows - 1
      diff_data[1, ]
    } else {
      colnames(diff_data)
    }

  # get rid of @@ column
  diff_data$`@@` <- NULL
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
  list(
    data = diff_data,
    inserted_rows = inserted_rows,
    inserted_cols = inserted_cols,
    modified_rows = modified_rows,
    deleted_rows = deleted_rows,
    deleted_cols = deleted_cols
  )
}

#' Takes a dataframe with subset of columns and a daff change type
#' to format each column appropriate for the given columns.
#'
#' @param columns data.frame
#' @param change_type character
#'
#' @return
#'
#' @examples
format_columns <- function(columns, change_type) {
  # TODO reordered rows/columns
  col_color <- switch(
    change_type,
    inserted = inserted_color,
    deleted = deleted_color,
    modified = modified_color
  )
  rtn <- list()
  column_names <- names(columns)
  range <- 1:length(columns)
  for (i in seq_along(range)) {
    if (identical(change_type, "modified")) {
      rtn[[column_names[i]]] <- reactable::colDef(
        headerStyle = list(background = col_color),
        style = function(value) {
          # we will need to use something like this to color cell for rows
          if (grepl("->", value)) {
            return(list(background = col_color))
          }
        }
      )
    } else {
      rtn[[column_names[i]]] <- reactable::colDef(
        headerStyle = list(background = col_color),
        cell = function(value, index) {
          # turn NULL into empty text
          if (identical(value, "NULL")) {
            return(htmltools::p(""))
          }
          return(value)
        },
        style = function(value) {
          return(list(background = col_color))
        }
      )
    }
  }
  return(rtn)
}

#' Takes diff information to format and return a reactable table as knit_print.
#'
#' @param diff
#'
#' @return knit_print
#' @export
#'
#' @examples
format_table <- function(diff_data, reactable_output = FALSE) {
  # TODO: you might consider a more general function that would take
  # user supplied styling per columns (for e.g. when you want to do callouts)
  # unpack data about diff
  inserted_rows <- diff_data$inserted_rows
  inserted_cols <- diff_data$inserted_cols
  deleted_rows <- diff_data$deleted_rows
  deleted_cols <- diff_data$deleted_cols
  modified_rows <- diff_data$modified_rows
  data <- diff_data$data
  col_vectors <- 1:length(colnames(data))

  # collect column definitions for reactable for styling
  inserted_column_defs <- format_columns(data[, inserted_cols, drop = FALSE], "inserted")
  deleted_column_defs <- format_columns(data[, deleted_cols, drop = FALSE], "deleted")
  modified_cols <- col_vectors[!(col_vectors %in% c(inserted_cols, deleted_cols))]
  modified_column_defs <- format_columns(data[, modified_cols, drop = FALSE], "modified")

  # construct reactable
  reactable_table <-
    reactable::reactable(data,
      theme = reactable::reactableTheme(
        borderWidth = "2px",
      ),
      height = 300,
      compact = TRUE,
      pagination = FALSE,
      rownames = TRUE,
      bordered = TRUE,
      resizable = TRUE,
      rowStyle = function(index) {
        if (index %in% inserted_rows) {
          list(background = inserted_color)
        } else if (index %in% deleted_rows) {
          list(background = deleted_color)
        }
      },
      defaultColDef = reactable::colDef(
        headerStyle = list(background = modified_color)
      ),
      columns = reappend(
        list(.rownames = reactable::colDef(style = list(textAlign = "left"))),
        modified_column_defs,
        inserted_column_defs,
        deleted_column_defs
      )
    )
  if (reactable_output) {
    return(reactable_table)
  }
  # return knit_print for rendering
  htmltools::knit_print.shiny.tag.list(reactable_table)
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
daff_html <- function(data_ref, data, include_summary = FALSE, include_diff_table = FALSE, reactable_output = FALSE) {

  diff <- daff::diff_data(data_ref = data_ref, data = data)
  s <- attr(diff, "summary")

  # Render the data diff summary table
  summary_table_html <- ""
  if (include_summary) {
    summary_table_html <- diff_summary_html(s)
  }

  # Now render the data diff table
  diff_table_html <- ""
  if (include_diff_table) {

    # Construct diff information for rows/columns
    diff_data <- construct_diff_data(diff)

    # Format table
    diff_table_html <- format_table(diff_data, reactable_output)
  }
  if (reactable_output) {
    return(diff_table_html)
  }
  return(c(summary_table_html, "<br>", diff_table_html))
}
