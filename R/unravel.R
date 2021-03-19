
#' Unravel dplyr expression for exploration
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' unravel(
#' mtcars %>%
#'   head(20) %>%
#'   select(mpg)
#' )
unravel <- function(code, viewer = T) {
  require(shiny)
  # by default run on Viewer pane, else on browser
  if (viewer) {
    on.exit(options(shiny.launch.browser = .rs.invokeShinyPaneViewer, add = TRUE))
  }

  expr <- rlang::get_expr(rlang::enquo(code))
  code <- gsub("%>% ", "%>%\n\t", paste0(rlang::expr_deparse(expr), collapse = ""))

  ui <- fluidPage(
    datawatsUI("datawat")
  )

  server <- function(input, output, session) {
    datawatsServer("datawat", code)
  }

  shinyApp(ui, server)
}


#' Unravels dplyr code for exploration.
#'
#' @param code A character
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
