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

# prismCodeBlock <- function(code) {
#     tagList(
#         HTML(code),
#         tags$script("Prism.highlightAll()")
#     )
# }
# <link rel="stylesheet"
# href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/styles/default.min.css">
#     <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/highlight.min.js"></script>
#     <!-- and it's easy to individually load additional languages -->
# <script charset="UTF-8"
#  src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/languages/go.min.js"></script>"


# src: https://stackoverflow.com/questions/47445260/how-to-enable-syntax-highlighting-in-r-shiny-app-with-htmloutput

hlCodeBlock <- function(code) {
    tagList(
        HTML(code),
        tags$script("hljs.initHighlightingOnLoad()")
    )
}

hljsDependencies <- tags$head(
    # tags$script(src = "highlight.pack.js"),
    tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/highlight.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/styles/a11y-light.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/languages/python.min.js"),
    tags$script("hljs.initHighlightingOnLoad()")
)

# prismSqlDependency <- tags$head(
#     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-python.min.js")
# )

ui <- fluidPage(
    # prismDependencies,
    # prismSqlDependency,
    #
    # HTML("<pre><code class='language-python'>lambda x: x + 1
    #    -- this chunk should be syntax highlighted and it is
    #    </code></pre>"),
    #
    # htmlOutput("python")

    hljsDependencies,

    HTML("<pre><code class=\"python\">nba = (nba.rename(columns=column_names)\n    <span style='background-color:#ffff7f'>.dropna(thresh=4)</span>\n    [['date', 'away_team', 'away_points', 'home_team', 'home_points']]\n    .assign(date=lambda x: pd.to_datetime(x['date'], format='%a, %b %d, %Y'))\n    .set_index('date', append=True)\n    .rename_axis([\"game_id\", \"date\"])\n    .sort_index()\n  )</code></pre>"),

    # htmlOutput("pythoncode"),
    # shiny::includeHTML("code_template.html")

)

stepper_path <- here::here("inst/tutorials/stepper/")

server <- function(input, output) {
    txt <- shiny::code("lambda x: x + 1", class="python")
    output$pythoncode <- renderUI({
        # fluidPage(
        #     # prismDependencies,
        #     # prismSqlDependency,
        #     #
        #     # HTML("<pre><code class='language-python'>lambda x: x + 1
        #     #    -- this chunk should be syntax highlighted and it is
        #     #    </code></pre>"),
        #     #
        #     # htmlOutput("python")
        #
        #     hljsDependencies,
        #
        #     HTML("<pre><code class=\"python\">nba = (nba.rename(columns=column_names)\n    <span style='background-color:#ffff7f'>.dropna(thresh=4)</span>\n    [['date', 'away_team', 'away_points', 'home_team', 'home_points']]\n    .assign(date=lambda x: pd.to_datetime(x['date'], format='%a, %b %d, %Y'))\n    .set_index('date', append=True)\n    .rename_axis([\"game_id\", \"date\"])\n    .sort_index()\n  )</code></pre>")
        #
        #
        # )
        # shiny::htmlTemplate(
        #     paste0(stepper_path, "code_template.html"),
        #     codetext = txt,
        #     document_ = TRUE
        # )


        # HTML("<pre><code class=\"python\">nba = (nba.rename(columns=column_names)\n    <span style='background-color:#ffff7f'>.dropna(thresh=4)</span>\n    [['date', 'away_team', 'away_points', 'home_team', 'home_points']]\n    .assign(date=lambda x: pd.to_datetime(x['date'], format='%a, %b %d, %Y'))\n    .set_index('date', append=True)\n    .rename_axis([\"game_id\", \"date\"])\n    .sort_index()\n  )</code></pre>")
        # hlCodeBlock("<pre><code class=\"python\">lambda x: x + 1</code></pre>")
        # hlCodeBlock(as.character(shiny::code("lambda x: x + 1", class="python")))
    })

    # output$python <- renderUI({
    #     HTML("<pre><code class=\"python\">lambda x: x + 1</code></pre>")
    # })
}

shinyApp(ui, server)


