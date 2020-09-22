
install_knitr_hooks <- function() {
  library(xfun)
  # links to check out for custom engine:
  # https://github.com/r-lib/asciicast/blob/03667c031f4e6f0ad716cf4ec55eb1395d8f5344/R/knitr.R#L100
  # https://github.com/carpentries/dovetail/blob/master/R/engine.R
  # TODO: check out knit_print + engine_output to allow html_widget
  # https://cran.r-project.org/web/packages/knitr/vignettes/knit_print.html
  # run python engine
  # run R engine
  # output in the correct form.
  
  # TODO figure out a way to clean up knitr hooks when exiting (learnr does this)
  # opts_template for dsviz
  dopts <- list(echo = TRUE, include = TRUE, fig.width = 8, fig.align = "center")
  diopts <- list(echo = FALSE, include = TRUE, fig.align = "center")
  knitr::opts_template$set(dsviz = dopts)
  knitr::opts_template$set(dsvizi = diopts)
  # custom engine that takes python code and outputs data structure viz via `lolviz`
  knitr::knit_engines$set(dsviz = function(options) {
    # use python engine and check it's loadable
    if (isFALSE(options$python.reticulate)) {
      eng_interpreted(options)
    } else {
      if (!loadable("reticulate")) {
        warning2(
          "The 'python' engine in knitr requires the reticulate package. ",
          "If you do not want to use the reticulate package, set the chunk option ",
          "python.reticulate = FALSE."
        )
      }
    }
    # setup python source code
    code <- glue::glue(
      "# import whatever libraries you'd need for evaluation in python
          from lolviz import objviz
          import pandas as pd
    
          objviz(eval(\"{options$code}\")).render(filename=\"images/{options$label}.gv\", format='svg', view = False)
          "
    )
    # run and save dot diagram
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
  })
  
  
  # prettier printing and dataframe formatting for Python
  
  # helper function to get the last line of the code in an exercise
  get_last_line <- function(options) {
    non_empty <- options$code[options$code != ""]
    tail(non_empty, n = 1)
  }
  # helper template for pprint knitr option
  knitr::opts_template$set(pprint = list(pprint = TRUE, results = "hide"))
  
  # this hook is to pretty print dataframes when it's the last line of python code
  knitr::knit_hooks$set(pprint = function(before, options, envir) {
      
      if (!before && options$pprint) {
        library(magrittr)
        if (length(options$code) > 0) {
          last_line <- get_last_line(options)
          if (length(last_line) > 0) {
            # first run the entire code to introduce variables to environment
            reticulate::py_run_string(paste0(options$code, collapse="\n"))
            # then, evaluate last line to detect whether it's a dataframe
            raw_result <-  reticulate::py_eval(last_line, convert = FALSE)
            converted_result <- reticulate::py_to_r(raw_result)
            # for data.frame, pretty print the output
            if(identical("data.frame", class(converted_result))) {
              options$results <- "asis"
              options$code <- ""
              # wizard of oz pandas dataframe by changing index as well
              rownames(converted_result) <- vapply(rownames(converted_result), function(x) as.character(as.numeric(x) - 1), FUN.VALUE = character(1))
              # kableExtra for now
              out <- kableExtra::kbl(converted_result) %>%
                kableExtra::kable_styling(bootstrap_options = c("condensed", "responsive")) %>%
                kableExtra::scroll_box(width = "100%", height = "250px")
              return(knitr::engine_output(options, "", out))
            } else {
              # for everything else, use python engine as expected
              options$results <- "markdown"
              return(knitr::engine_output(options, options$code, raw_result))
            }
          }
        }
      } else if(!before) {
        # for everything else, use python engine as expected
        last_line <- get_last_line(options)
        options$results <- "markdown"
        raw_result <- reticulate::py_eval(last_line, convert = FALSE)
        return(knitr::engine_output(options, "", raw_result))
      }
  })
  
}
