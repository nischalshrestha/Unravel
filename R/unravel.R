
invoke_unravel <- function(code, viewer = T) {
  library(shiny)
  suppressMessages(library(tidylog))

  ui <- fluidPage(
    unravelUI("unravel")
  )

  server <- function(input, output, session) {
    unravelServer("unravel", code)
  }

  # by default run on Viewer pane, else on browser
  if (viewer) {
    on.exit(options(shiny.launch.browser = .rs.invokeShinyPaneViewer, add = TRUE))
  }

  shinyApp(
    ui,
    server,
    onStart = function() {
      onStop(function() {
        # detach tidylog so the outputs no longer appear
        detach('package:tidylog')
      })
    },
    options = list(quiet = TRUE)
  )
}

#' Unravel dplyr expression for exploration. You can either wrap your dplyr/tidyr code in
#' a call or pipe the code to unravel.
#'
#' @param code a language (dplyr/tidyr code)
#' @param viewer a boolean on whether to display unravel in viewer pane
#'   This is `TRUE` by default so it's fluid going between editor and viewer rather than window.
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' unravel(
#'   mtcars %>%
#'     head(20) %>%
#'     select(mpg)
#' )
#' mtcars %>%
#'   head(20) %>%
#'   select(mpg) %>%
#'   unravel()
unravel <- function(code, viewer = T) {
  # don't evaluate code yet
  code <- substitute(code)
  # check if we are at the last pipe function
  if (identical(code, quote(`.`))) {
    # if so, get the piped code
    code <- sys.call(1)[[2]]
  }

  # deparsing the code does not always produce well formatted text so we format it here
  if (!is.null(code)) {
    code <- gsub("%>% ", "%>%\n\t", paste0(trimws(rlang::expr_deparse(code)), collapse = ""))
  } else {
    code <- ""
  }

  invoke_unravel(code, viewer)
}

#' A variant of `unravel` to support accepting the code character instead.
#'
#' @param code A character
#' @param viewer a boolean on whether to display unravel in viewer pane
#'   This is `TRUE` by default so it's fluid going between editor and viewer rather than window.
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' unravel_code("mtcars %>% select(cyl, mpg)")
unravel_code <- function(code = "", viewer = T) {
  invoke_unravel(code, viewer)
}

#' The binding function for unraveling code using the add-in.
#'
#' @return A shiny app
#' @export
unravel_addin <- function() {
  ec <- rstudioapi::getSourceEditorContext()
  selected <- ec$selection[[1]]$text
  invoke_unravel(code = selected)
}

