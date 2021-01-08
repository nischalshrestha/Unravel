library(shiny)

# linked hover

# basic template
# class = callout_<class> (code side)
# class = callout_text, id = foo (summary side)

# html looks like this
# <div id="code">
#   <p>pd.<span class = "callout_DataFrame">DataFrame</span>(<span class = "callout_dict">{'a':[1, 2, 3]}</span>)</p>
# </div>
# <hr>
# <div id="summary">
#   <p>We create a <span class="callout_text" id = "A">DataFrame</span> by supplying a <span class="callout_text" id = "B">dict</span>.</p>
# </div>

# TODO put in external js
"$(document).ready(function() {
  $('.callout_text#A').hover(function(event) {
    $('.callout_DataFrame').toggleClass('hover_yellow');
    console.log('callout_DataFrame div was hovered');
  });
  $('.callout_text#B').hover(function(event) {
    $('.callout_dict').toggleClass('hover_yellow');
    console.log('callout_dict div was hovered');
  });
});" -> hover_linked_js


# TODO put in external css
".hover_yellow {
  background-color: #ffff7f;
}

.hover_green {
  background-color: #caf8bb;
}

.code {
  background-color: lightgrey;
  border-radius: 5px;
}

.callout_text {
  background-color: lightgrey;
  /* adds a background colour to the button */
    color: black;
  /* changes the text colour */
    cursor: pointer;
  /* changes the mouse on hover */
    border-radius: 5px;
  border: 1px dotted black;
  padding: 2px;
}

.callout_text:hover {
  background-color: #ffff7f;
    /* adds a background hover colour to the button */
    color: #333;
    border: 1px solid black;
  /* changes the text colour on hover */
    border-radius: 5px;
}

div {
  padding: 10px;
}" -> hover_linked_css

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(HTML(hover_linked_css)),
  ),
  shiny::htmlOutput("code"),
  shiny::hr(),
  shiny::htmlOutput("summary")
)

server <- function(input, output) {
  # your code snippet text area
  output$code <- shiny::renderUI({
    shiny::div(
      # pd.DataFrame({'a': [1, 2, 3]})
      shiny::p(
        "pd.",
        shiny::span("DataFrame", class = "callout_DataFrame", .noWS = "outside"),
        "(",
        shiny::span("{'a': [1, 2, 3]}", class = "callout_dict", .noWS = "outside"),
        ")"

      ),
      .noWS = "outside"
    )
  })

  # your text explanation area, where you need to also load the jquery for
  # allowing the hover effect to take place
  # TODO: look to see if we can also just create an external JS/CSS for this
  output$summary <- shiny::renderUI({
    shiny::tagList(
      shiny::div(
        # We create a DataFrame by supplying a dict.
        shiny::p(
          "We create a ",
          shiny::span("DataFrame", class = "callout_text", id = "A", .noWS = "outside"),
          " by supplying a ",
          shiny::span("dict", class = "callout_text", id = "B", .noWS = "outside"),
          "."
        ),
        .noWS = "outside"
      ),
      shiny::tags$script(
        shiny::HTML(hover_linked_js)
      )
    )
  })
}

# shiny::shinyApp(ui, server)
