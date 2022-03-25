
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
  # NOTE: we only call the summarize() to know which variables we support
  suppressWarnings(
    summary <- dataReporter::summarize(dat)
  )

  variables <- names(summary)

  # let's keep track of the vars that don't have support
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
    list(
      dplyr::tibble(
        Variable = variables,
        Type = var_types,
        Unique = unique_elements,
        `Missing / Not Missing` = missing_counts
      ),
      not_supported_vars
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

  summaries <- get_summary(dat)
  dat_summary <- summaries[[1]]
  dat_summary <-  dat_summary %>%
    mutate(Details = NA, Distribution = NA, `Potential Problems` = NA) %>%
    select(Details, Variable, Type, Unique, `Missing / Not Missing`, Distribution, `Potential Problems`)

  ### Curated summary
  # list of the variable descriptive stat tables
  stables <- lapply(
    dat,
    function(variable) {
      if (is.list(variable)) {
        return(tibble())
      }
      var_summary <- as_tibble(as.list(summary(variable)))
      if (is.numeric(variable)) {
        return(
          rename(var_summary, `1st Quartile` = `1st Qu.`, `3rd Quartile` = `3rd Qu.`)
        )
      }
      var_summary
    }
  )

  ### Checks
  not_supported_vars <- summaries[[2]]
  dat_checks <- dataReporter::check(
    dat[!names(dat) %in% names(not_supported_vars)],
    checks = dataReporter::setChecks(
      character = dataReporter::defaultCharacterChecks(remove = "isCPR"),
      Date = dataReporter::defaultDateChecks(remove = "isCPR"),
      factor = dataReporter::defaultFactorChecks(remove = "isCPR"),
      labelled = dataReporter::defaultLabelledChecks(remove = "isCPR"),
      haven_labelled = dataReporter::defaultHavenlabelledChecks(remove = "isCPR"),
      logical = dataReporter::defaultLogicalChecks(remove = "isCPR"),
      numeric = dataReporter::defaultNumericChecks(
        remove = c("identifyOutliers", "isCPR"), add = "identifyOutliersTBStyle"
      ),
      integer = dataReporter::defaultIntegerChecks(
        remove = c("identifyOutliers", "isCPR"), add = "identifyOutliersTBStyle"
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
    showSortable = TRUE,
    defaultColDef = colDef(
      align = "left"
    ),
    columns = list(
      Type = colDef(maxWidth = 80),
      Unique = colDef(maxWidth = 80),
      `Missing / Not Missing` = colDef(
        maxWidth = 150,
        # use a small bar graph to display this
        cell = htmlwidgets::JS(glue::glue("function(cellInfo) {
          // Format as percentage for the bar shading
          let dat_row_count = {{nrow(dat)}}
          let real_values = dat_row_count - cellInfo.value
          let pct = ((cellInfo.value / dat_row_count) * 100).toFixed(2) + '%'
          // if undefined, dont shade bar red
          if (cellInfo.value === undefined) {
            pct = '0%'
          }
          // Use the value as is for the count
          let missing_count = cellInfo.value
          // Render bar chart
          return (
            '<div class=\"bar-cell\">' +
              '<span>' + missing_count + '</span>' +
              '<div class=\"bar-chart\" style=\"background-color: #e1e1e1\">' +
                '<div class=\"bar\" style=\"width: ' + pct + '; background-color: #fb8072\"></div>' +
              '</div>' +
              '<span>' + real_values + '</span>' +
            '</div>'
          )
          }", .open = "{{", .close = "}}")
        ),
        html = TRUE
      ),
      # display the distribution and the boxplot (will work for numeric, and won't crash for factors)
      Distribution = colDef(
        maxWidth = 300,
        sortable = FALSE,
        cell = function(value, index) {
          if (index > length(dat)) return("")
          col_dat <- dat[[index]]
          # we have to get counts to show histogram for categoricals
          unique_counts <- unique(col_dat)
          if (is.factor(col_dat) || is.character(col_dat) || length(unique_counts) <= 10) {
            # if it's just one single category, just pass data as is so we can hover over and see label
            if (length(unique_counts) == 1) {
              return(sparkline(
                col_dat, type = "bar", height = 25, width = 200, barWidth = 8, nullColor = "#fb8072"
              ))
            }
            # else, display the distribution (where we can't see category on hover but could be seen in details)
            return(sparkline(
              c(dplyr::count(dat, across(names(dat)[[index]]))[['n']], 0),
              type = "bar", height = 25, width = 200, barWidth = 8,
              nullColor = "#fb8072",
              zeroColor = "#fff",
              tooltipValueLookups = list("10" = "foo")
            ))
          }
          # else, if it's not categorical in nature and it's not list columns, create plots
          # that could either be a histogram or count
          if (!is.list(col_dat)) {
            # if we have a lot of unique values, it's better to get the histogram
            if (length(unique_counts) > 50) {
              sparkline(
                hist(col_dat, plot = FALSE, breaks = length(unique_counts) %/% 2)$counts,
                type = "bar", height = 25, width = 200, barWidth = 8, nullColor = "#fb8072"
              )
            } else {
              # otherwise, if there aren't a lot of unique values, it's more beneficial
              # to display the counts of each
              sparkline(
                dplyr::count(dat, across(names(dat)[[index]]))[['n']],
                type = "bar", height = 25, width = 200, barWidth = 8, nullColor = "#fb8072"
              )
            }
          }
        }
      ),
      `Potential Problems` = colDef(
        maxWidth = 100,
        html = TRUE,
        align = 'center',
        style = function(value) {
          list(fontWeight = "bold")
        },
        cell = function(value, index) {
          var_checks <- dat_checks[index][[1]]
          problems <- Filter(function(c) c$problem, var_checks)
          if (length(problems) == 0) {
            return("")
          } else {
            return(length(problems))
          }
        }
      ),
      # create a column for the show details button
      Details = colDef(
        width = 60,
        sortable = FALSE,
        details = function(index) {
          var_checks <- dat_checks[index][[1]]
          if (length(var_checks) == 0) return(paste0("Unsupported variable type"))
          var_name <- names(dat)[[index]]
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
            suppressWarnings(
              taglist <-
                list(
                  shiny::div(
                    reactable::reactable(
                      as.data.frame(dplyr::count(dat, across(var_name))),
                      defaultColDef = colDef(na = "NA")
                    )
                  )
                )
            )
          } else {
            # otherwise start with stats table
            if (is.numeric(dat[[var_name]])) {
              taglist <- list(
                shiny::div(
                  style = "padding: 0.5em;",
                  reactable::reactable(
                    stables[[var_name]],
                    compact = TRUE,
                    defaultColDef = colDef(format = reactable::colFormat(digits = 2))
                  )
                )
              )
            } else {
              taglist <- list(
                shiny::div(
                  style = "padding: 0.5em;",
                  reactable::reactable(stables[[var_name]], compact = TRUE)
                )
              )
            }
          }
          # include problems html if they exist
          if (length(problems) > 0) {
            taglist <- append(
              taglist,
              list(
                shiny::div(
                  style = "padding: 0.5em;",
                  shiny::h5("Potential issues:"),
                  shiny::tags$p(shiny::tags$ul(problems))
                )
              )
            )
          }
          class(taglist) <- "shiny.tag.list"
          taglist
        }
      )
    )
  )
}
