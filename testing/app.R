library(shiny)

# prismCodeBlock <- function(code) {
#     tagList(
#         HTML(code),
#         tags$script("Prism.highlightAll()")
#     )
# }
#
# prismDependencies <- tags$head(
#     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
#     tags$link(rel = "stylesheet", type = "text/css",
#               href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
# )
#
# prismSqlDependency <- tags$head(
#     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-python.min.js")
# )

#
# <!DOCTYPE html>
# <html>
# <head>
# <title>Page Title</title>
# <link rel="stylesheet" href="a11y-light.css">
# <script src="highlight.pack.js"></script>
# <script>hljs.initHighlightingOnLoad();</script>
# </head>
# <body>
# <!-- FLAIRED CODE -->
# <pre><code class="python">{{ codetext }}</code></pre>
# </body>
# </html>

prismCodeBlock <- function(code) {
    tagList(
        HTML(code),
        tags$script("Prism.highlightAll()")
    )
}

prismDependencies <- tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

prismSqlDependency <- tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-python.min.js")
)

ui <- fluidPage(
    prismDependencies,
    prismSqlDependency,

    HTML("<pre><code class='language-python'>lambda x: x + 1
       -- this chunk should be syntax highlighted and it is
       </code></pre>"),

    htmlOutput("python")
)

server <- function(input, output) {
    txt <- "lambda x: x + 2"

    output$sql <- renderUI({
        prismCodeBlock(txt)
    })
}

shinyApp(ui, server)
