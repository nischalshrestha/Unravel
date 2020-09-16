
install_knitr_hooks <- function() {
  # opts_template for dsviz
  dopts <- list(echo=TRUE, include=TRUE, fig.width=8, fig.align="center")
  diopts <- list(echo=FALSE, include=TRUE, fig.align="center")
  knitr::opts_template$set(dsviz = dopts)
  knitr::opts_template$set(dsvizi = diopts)
  # custom engine that takes python code and outputs data structure viz via `lolviz`
  knitr::knit_engines$set(dsviz = function(options) {
      # TODO: use an opts template for fitting images nicely
      # while also allowing overrides of passed in chunk options
      # use python engine and check it's loadable
      if (isFALSE(options$python.reticulate)) {
        eng_interpreted(options)
      } else {
        if (!loadable("reticulate"))
          warning2("The 'python' engine in knitr requires the reticulate package. ",
                   "If you do not want to use the reticulate package, set the chunk option ",
                   "python.reticulate = FALSE.")
        # setup python source code
        code <- glue::glue(
          "# import whatever libraries you'd need for evaluation in python
              from lolviz import objviz
              import pandas as pd
      
              objviz(eval(\"{options$code}\")).render(filename=\"images/{options$label}.gv\", format='svg', view = False)
              "
        )
        # run and save dot
        reticulate::py_run_string(code)
        # override knitr options
        # we want the output as verbatim markdown
        options$results <- "asis"
        options$opts.label <- "dsviz"
        code <- ""
        if (options$echo) {
          code <- options$code
        }
        return(
          knitr::engine_output(options, code, glue::glue("![](./images/{options$label}.gv.svg)"))
        )
      }
    }
  )
}