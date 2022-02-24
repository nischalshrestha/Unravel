
#' Return basic descriptive stats about the columns of a \code{data.frame} or \code{tibble}
#'
#' Currently, we return type of column, # unique elements, # missing values.
#'
#' @param dat \code{data.frame} or \code{tibble}
#'
#' @return \code{tibble}
#' @export
get_summary <- function(dat) {
  summary <- dataReporter::summarize(dat)
  variables <- names(summary)

  # extract the type
  var_types <- unlist(unname(
    lapply(
      summary,
      function(variable) {
        variable$variableType$result
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

  # extract missing value count
  pct_missing <- unlist(unname(
    lapply(
      summary,
      function(variable) {
        variable$countMissing$value
      }
    )
  ))

  # final summary
  # NOTE: the `details` column is to show the expand for details button
  # on the reactable
  dplyr::tibble(
    columns = variables,
    type = var_types,
    unique = unique_elements,
    missing = pct_missing,
    details = NA
  )
}

#' Get a diagnosis \code{reactable} given a data.frame/tibble.
#'
#' @param dat \code{data.frame} or \code{tibble}
#'
#' @return \code{reactable}
#' @export
get_diagnosis <- function(dat) {
  # first grab the summary tibble and
  summary <- dataReporter::summarize(dat)
  dat_summary <- get_summary(dat)

  ### Curated summary
  # list of the variable descriptive stat tables
  # but exclude variableType, countMissing (these will be in the outer table)
  stables <- lapply(
    summary,
    function(variable) {
      features <- names(variable)
      exclude <- c("variableType", "countMissing", "uniqueValues")
      features <- features[!features %in% exclude]
      readable_names <- list(
        "centralValue" = "Median",
        "quartiles" = "1st and 3rd quartiles",
        "minMax" = "Min. and max."
      )
      feature_names <- unname(readable_names[features])
      variables <- Filter(function(v) !v %in% exclude, variable)
      tidyr::unnest(dplyr::tibble(
        # TODO get the human-readable versions of the features
        # dataReporter does this when reporting issues, just need to
        # somehow dig that part out or do the renaming ourselves (with key-value list)
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
  dat_checks <- dataReporter::check(
    dat, checks = setChecks(
      numeric = defaultNumericChecks(
        remove = "identifyOutliers", add = "identifyOutliersTBStyle"
      ),
      integer = defaultIntegerChecks(
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
      missing = colDef(
        # style text red if there are missing values
        style = function(value) {
          if (value > 0) {
            color <- "#ff4a40"
            fontWeight <- "bold"
          } else {
            color <- "#000"
            fontWeight <- "normal"
          }
          list(color = color, fontWeight = fontWeight)
        },
        cell = function(value) {
          # Render as count (%)
          paste0(value, " (", round(value / nrow(dat), 2), "%)")
        }
      ),
      # TODO maybe we can style this button better or use some kinda
      # bootstrap button to make it look less bland
      details = colDef(
        name = "",
        sortable = FALSE,
        cell = function() shiny::tags$button("Show details")
      )
    ),
    details = function(index) {
      # paste("Details for row:", index)
      var_checks <- dat_checks[index][[1]]
      # only extract the problems
      problems <- Filter(function(c) c$problem, var_checks)
      # gather the problematic messages
      problems <- unname(lapply(problems, function(c) c$message))
      # then, create list items so they can be displayed as bullet points
      problems <- lapply(unname(problems), function(p) shiny::tags$li(gsub("\\\\", "", p)))
      # display the table of stats and the potential issues
      shiny::div(
        shiny::div(
          style = "padding: 0.5em;",
          reactable::reactable(
            stables[[index]],
            compact = TRUE,
            fullWidth = F,
            defaultColDef = colDef(
              minWidth = 150
            ),
            columns = list(
              Result = colDef(
                name = ""
              )
            )
          )
        ),
        shiny::div(
          style = "padding: 0.5em;",
          shiny::h5("Potential issues:"),
          shiny::tags$p(
            shiny::tags$ul(problems)
          )
        )
      )
    },
    onClick = "expand",
    # Give rows a pointer cursor to indicate that they're clickable
    rowStyle = list(cursor = "pointer")
  )
}
