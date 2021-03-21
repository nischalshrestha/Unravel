
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
unravel <- function(code = NULL, viewer = T) {
  require(shiny)
  # don't evaluate code yet
  code <- substitute(code)
  # are we at the last pipe function
  if (identical(code, quote(`.`))) {
    # if so, get the piped code
    code <- sys.call(1)[[2]]
  }

  # by default run on Viewer pane, else on browser
  if (viewer) {
    on.exit(options(shiny.launch.browser = .rs.invokeShinyPaneViewer, add = TRUE))
  }

  if (!is.null(code)) {
    code <- gsub("%>% ", "%>%\n\t", paste0(rlang::expr_deparse(code), collapse = ""))
  } else {
    code <- ""
  }

  ui <- fluidPage(
    datawatsUI("datawat")
  )

  server <- function(input, output, session) {
    datawatsServer("datawat", code)
  }

  shinyApp(ui, server)
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
  require(shiny)

  # by default run on Viewer pane, else on browser
  if (viewer) {
    on.exit(options(shiny.launch.browser = .rs.invokeShinyPaneViewer, add = TRUE))
  }

  ui <- fluidPage(
    datawatsUI("datawat")
  )

  server <- function(input, output, session) {
    datawatsServer("datawat", code)
  }

  shinyApp(ui, server)
}
