
#' Unravels dplyr code for exploration.
#'
#' @param code A character
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' unravel("mtcars %>% select(cyl, mpg))")
unravel <- function(code) {

  # TODO this is a dummy pipeline just so we have an example to run when app starts
  # "diamonds %>%
  # select(carat, cut, color, clarity, price) %>%
  # group_by(color) %>%
  # summarise(n = n(), price = mean(price)) %>%
  # arrange(desc(color))" -> code

  ui <- fluidPage(
    datawatsUI("datawat", code)
  )

  server <- function(input, output, session) {
    datawatsServer("datawat")
  }

  shinyApp(ui, server)
}
