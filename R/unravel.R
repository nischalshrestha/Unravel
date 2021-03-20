
#' Unravel dplyr expression for exploration
#'
#' @param code a language
#' @param viewer a boolean on whether to display unravel in viewer pane
#'   This is `TRUE` by default so it's fluid going between editor and viewer rather than window.
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' unravel(
#' mtcars %>%
#'   head(20) %>%
#'   select(mpg)
#' )
unravel <- function(code = NULL, viewer = T) {
  require(shiny)
  # by default run on Viewer pane, else on browser
  if (viewer) {
    on.exit(options(shiny.launch.browser = .rs.invokeShinyPaneViewer, add = TRUE))
  }

  expr <- rlang::get_expr(rlang::enquo(code))
  if (!is.null(expr)) {
    code <- gsub("%>% ", "%>%\n\t", paste0(rlang::expr_deparse(expr), collapse = ""))
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
