
#' Unravels dplyr code for exploration.
#'
#' @param code A character
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' unravel("mtcars %>% select(cyl, mpg)")
unravel <- function(code) {

  ui <- fluidPage(
    datawatsUI("datawat", code)
  )

  server <- function(input, output, session) {
    datawatsServer("datawat")
  }

  shinyApp(ui, server)
}
