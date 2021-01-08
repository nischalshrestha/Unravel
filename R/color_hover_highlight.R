library(shiny)

# html
# <div>
#   <p class="yellow_receiver">I am yellow when you hover over Target A</p>
# </div>
# <div>
#   <p class="green_receiver">I am green when you hover over Target B</p>
# </div>
# <p class="yellow">Target A</p>
# <p class="green">Target B</p>

hover_html <- shiny::tagList(
  shiny::div(shiny::p("I am yellow when you hover over Target A", class = "yellow_receiver")),
  shiny::div(shiny::p("I am green when you hover over Target B", class = "green_receiver")),
  shiny::p("Target A", class = "yellow"),
  shiny::p("Target B", class = "green")
)

"
$(document).ready(function(){
  $('.yellow').hover(function(){
     $('.yellow_receiver').toggleClass('hover_yellow');
     console.log('yellow div was hovered');
  });

  $('.green').hover(function(){
     $('.green_receiver').toggleClass('hover_green');
     console.log('green div was hovered');
  });
});
" -> hover_js

".hover_yellow
{
  background-color: #ffff7f;
}

.hover_green
{
  background-color: #caf8bb;
}" -> hover_css

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(HTML(hover_css)),
  ),
  shiny::htmlOutput("code"),
  shiny::htmlOutput("summary")
)

server <- function(input, output) {
  # your code snippet text area
  output$code <- shiny::renderUI({
    shiny::tagList(
      shiny::div(shiny::p("I am yellow when you hover over Target A", class = "yellow_receiver")),
      shiny::div(shiny::p("I am green when you hover over Target B", class = "green_receiver"))
    )
  })
  # your text explanation area, where you need to also load the jquery for
  # allowing the hover effect to take place
  # TODO: look to see if we can also just create an external JS/CSS for this
  output$summary <- shiny::renderUI({
    shiny::tagList(
      shiny::p("Target A", class = "yellow"),
      shiny::p("Target B", class = "green"),
      shiny::tags$script(
        shiny::HTML(hover_js)
      )
    )
  })
}

# shinyApp(ui, server)
