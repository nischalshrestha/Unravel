
get_unsupported_vars <- function(summary) {
  Filter(
    function(s) {
      identical(names(s), "DataClassNotSupported")
    },
    summary
  )
}

#' Return basic descriptive stats about the columns of a \code{data.frame} or \code{tibble}
#'
#' Currently, we return type of column, # unique elements, # missing values.
#'
#' @param dat \code{data.frame} or \code{tibble}
#'
#' @return \code{tibble}
#' @export
get_summary <- function(dat) {
  # suppress the dataReporter warnings for things like unsupported types
  suppressWarnings(
    summary <- dataReporter::summarize(dat)
  )

  variables <- names(summary)

  # solution 1: let's keep track of the vars that don't have support
  not_supported_vars <- get_unsupported_vars(summary)

  # extract the type
  # NOTE: we use `typeof` instead of dataReporter's `variableType` because
  # the result of unsupported variables are not useful
  var_types <- unlist(unname(
    lapply(
      variables,
      function(v) {
        unlist(strsplit(vctrs::vec_ptype_full(dat[[v]]), "<"))[[1]]
      }
    )
  ))

  # extract unique value count
  unique_elements <- unlist(unname(
    lapply(
      summary,
      function(variable) {
        variable$unique$result
      }
    )
  ))

  # extract missing count
  missing_counts <- unlist(unname(
    lapply(
      summary,
      function(variable) {
        variable$countMissing$value
      }
    )
  ))

  # extract non-na counts
  non_na_counts <- unlist(unname(
    lapply(
      variables,
      function(v) {
        sum(!is.na(dat[[v]]))
      }
    )
  ))

  # unsupported variables will just have NAs for now
  if (length(not_supported_vars) > 0) {
    unique_elements <- append(unique_elements, rep(NA, length(not_supported_vars)))
    missing_counts <- append(missing_counts, rep(NA, length(not_supported_vars)))
  }

  # try producing the final summary tibble
  # this can fail if there are discrepancies in the column lengths caused by
  # unsupported data types for `dataReporter` to produce summaries (e.g list columns)
  # NOTE: the `details` column is to show the expand for details button on the reactable
  tryCatch(
    dplyr::tibble(
      variable = variables,
      type = var_types,
      non_na = non_na_counts,
      unique = unique_elements,
      missing = missing_counts,
      details = NA
    ),
    error = function(e) {
      stop("There were some unsupported types that prevented me from producing diagnostic summaries.")
    }
  )
}

#' Get a diagnosis \code{reactable} given a data.frame/tibble.
#'
#' @param dat \code{data.frame} or \code{tibble}
#'
#' @importFrom sparkline sparkline
#'
#' @return \code{reactable}
#' @export
get_diagnosis <- function(dat) {

  # first grab the summary tibble and
  # suppress the dataReporter warnings for things like unsupported types
  suppressWarnings(
    summary <- dataReporter::summarize(dat)
  )

  dat_summary <- get_summary(dat)
  dat_summary <-  dat_summary %>%
    mutate(boxplot = NA, distribution = NA) %>%
    select(variable, type, unique, non_na, missing, distribution, boxplot, details)

  ### Curated summary
  # list of the variable descriptive stat tables
  # but exclude variableType, countMissing (these will be in the outer table)
  stables <- lapply(
    summary,
    function(variable) {
      features <- names(variable)
      exclude <- c("variableType", "countMissing", "uniqueValues")
      features <- features[!features %in% exclude]
      is_num <- any(variable[['variableType']] %in% c("integer", "numeric"))
      # get the human-readable versions of the features
      readable_names <- list(
        "centralValue" = ifelse(is_num, "Median", "Mode"),
        "refCat" = "Reference category",
        "quartiles" = "1st and 3rd quartiles",
        "minMax" = "Min. and max."
      )
      feature_names <- unname(readable_names[features])
      variables <- Filter(function(v) !v %in% exclude, variable)
      tidyr::unnest(dplyr::tibble(
        Statistic = unlist(feature_names),
        Result = lapply(
          features,
          function(fkey) {
            paste0(variable[[fkey]]$result)
          }
        )
      ), cols = c(Result))
    }
  )

  ### Checks
  not_supported_vars <- get_unsupported_vars(summary)
  dat_checks <- dataReporter::check(
    dat[!names(dat) %in% names(not_supported_vars)],
    checks = dataReporter::setChecks(
      numeric = dataReporter::defaultNumericChecks(
        remove = "identifyOutliers", add = "identifyOutliersTBStyle"
      ),
      integer = dataReporter::defaultIntegerChecks(
        remove = "identifyOutliers", add = "identifyOutliersTBStyle"
      )
    )
  )

  # final diagnostic table
  reactable::reactable(
    dat_summary,
    pagination = FALSE,
    resizable = TRUE,
    compact = TRUE,
    bordered = TRUE,
    columns = list(
      unique = colDef(
        maxWidth = 80
      ),
      non_na = colDef(
        name = "not missing"
      ),
      missing = colDef(
        # use a small bar graph to display this
        cell = htmlwidgets::JS(glue::glue("function(cellInfo) {
          // Format as percentage for the bar shading
          let pct = ((cellInfo.value / {{nrow(dat)}}) * 100).toFixed(2) + '%';
          // if undefined, dont shade bar red
          if (cellInfo.value === undefined) {
            pct = '0%';
          }
          // Use the value as is for the count
          let pct_string = cellInfo.value;
          // Render bar chart
          return (
            '<div class=\"bar-cell\">' +
              '<span>' + pct_string + '</span>' +
              '<div class=\"bar-chart\" style=\"background-color: #e1e1e1\">' +
                '<div class=\"bar\" style=\"width: ' + pct + '; background-color: #fb8072\"></div>' +
              '</div>' +
            '</div>'
          )
          }", .open = "{{", .close = "}}")
        ),
        html = TRUE
      ),
      # display the distribution and the boxplot (will work for numeric, and won't crash for factors)
      distribution = colDef(
        cell = function(value, index) {
          if (index > length(dat)) return("")
          col_dat <- dat[[index]]
          # we have to get counts to show histogram
          if (is.factor(col_dat) || is.character(col_dat)) {
            unique_counts <- unique(col_dat)
            if (length(unique_counts) == 1) {
              return(sparkline(
                col_dat, type = "bar", height = 25, width = 100, barWidth = 8, nullColor = "#fb8072"
              ))
            }
            return(sparkline(
              dplyr::count(dat, across(names(dat)[[index]]))[['n']],
              type = "bar", height = 25, width = 100, barWidth = 8,
              nullColor = "#fb8072",
              tooltipValueLookups = list("10" = "foo")
            ))
          }
          sparkline(col_dat, type = "bar", height = 25, width = 100, barWidth = 8, nullColor = "#fb8072")
        }
      ),
      boxplot = colDef(
        cell = function(value, index) {
          if (index > length(dat)) return("")
          col_dat <- dat[[index]]
          if (is.factor(col_dat) || is.character(col_dat)) return("not applicable")
          sparkline(col_dat, type = "box", height = 25, width = 100)
        }
      ),
      # create a column for the show details button
      details = colDef(
        name = "",
        maxWidth = 100,
        sortable = FALSE,
        cell = function() shiny::tags$button("Show details", class = "btn-sm")
      )
    ),
    details = function(index) {
      var_checks <- dat_checks[index][[1]]
      if (length(var_checks) == 0) return(paste0("Unsupported variable type"))
      # only extract the problems
      problems <- Filter(function(c) c$problem, var_checks)
      # gather the problematic messages
      problems <- unname(lapply(problems, function(c) c$message))
      # then, create list items so they can be displayed as bullet points
      problems <- lapply(unname(problems), function(p) shiny::tags$li(gsub("\\\\", "", p)))
      # construct the html for the table of stats and the potential issues
      var_type <- unlist(strsplit(vctrs::vec_ptype_full(dat[[index]]), "<"))[[1]]
      taglist <- list()
      # if it's a ordinal/categorical variable, provide a count stat and exclude stats table
      if (var_type %in% c('ordered', 'character', 'factor')) {
        taglist <-
          list(
            shiny::div(
              reactable::reactable(
                as.data.frame(dplyr::count(dat, across(names(dat)[[index]]))),
                defaultColDef = colDef(
                  na = "NA"
                )
              )
            )
          )
      } else {
        # otherwise start with stats table
        taglist <- list(
          shiny::div(
            style = "padding: 0.5em;",
            reactable::reactable(
              stables[[index]],
              compact = TRUE,
              columns = list(
                Result = colDef(
                  name = ""
                )
              )
            )
          )
        )
      }
      # include problems html if they exist
      if (length(problems) > 0) {
        taglist <- append(
          taglist,
          list(
            shiny::div(
              style = "padding: 0.5em;",
              shiny::h5("Potential issues:"),
              shiny::tags$p(
                shiny::tags$ul(problems)
              )
            )
          )
        )
      }
      class(taglist) <- "shiny.tag.list"
      taglist
    },
    onClick = "expand",
    # Give rows a pointer cursor to indicate that they're clickable
    rowStyle = list(cursor = "pointer")
  )
}
