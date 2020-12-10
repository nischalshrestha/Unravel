
#' Takes in code string and returns a shiny::tagList with a Prism.highlightAll()
#' script to syntax highlight it.
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
prismCodeBlock <- function(code_text) {
  # print(shiny::code(code_text, class='language-python'))
  shiny::tagList(
    shiny::pre(shiny::code(code_text, class='language-python')),
    # shiny::tags$script(glue::glue("Prism.highlight(\"{code_text}\", Prism.languages.python, 'python');"))
    shiny::tags$script("Prism.highlightAll()")
  )
}
