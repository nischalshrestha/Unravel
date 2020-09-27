
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

  # helper template for pprint knitr option
  knitr::opts_template$set(pprint = list(pprint = TRUE, results = "hide"))
  
  # prettier printing and dataframe formatting for Python
  pprinter <- function(before, options, envir) {
    if (!before) {
      oldcode <- options$code
      cached_diff <- get_delta_cache(paste0(oldcode, collapse="\n"))
      # TODO: same table format for both Python/R so that it's not confusing?
      # that would require checking the engine and handling R more directly with kableExtra
      if (options$pprint && is.null(cached_diff)) {
        library(magrittr)
        if (length(options$code) > 0) {
          last_line <- get_last_line(options)
          if (length(last_line) > 0) {
            # get all of the setup code and the code itself
            exercise_cache <- learnr:::get_exercise_cache(options$label)
            all_chunks <- exercise_cache$chunks
            all_setup_code <- paste0(
              vapply(all_chunks[-length(all_chunks)], function(x) x$code, character(1)),
              collapse = "\n"
            )
            # execute code if not empty
            if (!identical(all_setup_code, "")) {
              # first run the entire code to introduce variables to environment
              reticulate::py_run_string(all_setup_code)
              # then, evaluate last line to detect whether it's a dataframe
              raw_result <- reticulate::py_eval(last_line, convert=FALSE)
              converted_result <- reticulate::py_to_r(raw_result)
              # for data.frame, pretty print the output
              if(identical("data.frame", class(converted_result))) {
                options$results <- "asis"
                options$code <- ""
                # wizard of oz pandas dataframe by changing index as well
                converted_result <- python_df(raw_result)
                # kableExtra for now
                out <- kableExtra::kbl(converted_result) %>%
                  kableExtra::kable_styling(bootstrap_options = c("condensed", "responsive")) %>%
                  kableExtra::scroll_box(width = "100%", height = "250px")
                # TODO: if data_diff is requested, append the output with it
                if (options$data_diff) {
                  diff_out <- data_diff(include_summary = TRUE, include_diff_table = TRUE)
                  # TODO to build this summary programmatically
                  summary_statement <- 
                    "All **9** income variables have been deleted (red) and are now under 
                    `variable` (green), which added **162** rows. In other words, we changed 
                    the data from a wide format to a long format. We weren't modifying any 
                    existing values so there are no modified/reordered rows/columns."
                  out <- c(out, "<br>", "**Data Diff Summary:**\n", summary_statement, "<br>", diff_out)
                }
                store_delta_cache(paste0(oldcode, collapse="\n"), out)
                return(knitr::engine_output(options, "", out))
              } else {
                # for everything else, use python engine as expected
                options$results <- "markdown"
                return(knitr::engine_output(options, options$code, raw_result))
              }
            }
          }
        }
      } else if(options$pprint && !is.null(cached_diff)) {
        return(cached_diff)
      } else if(!options$pprint) {
        # for everything else, use python engine as expected
        last_line <- get_last_line(options)
        options$results <- "markdown"
        raw_result <- reticulate::py_eval(last_line, convert = FALSE)
        return(knitr::engine_output(options, "", raw_result))
      }
    }
  }
  
  # this hook is to pretty print dataframes when it's the last line of python code
  knitr::knit_hooks$set(pprint = pprinter)
  
}
