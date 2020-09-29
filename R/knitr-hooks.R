
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
  pprintopts <- list(pprint = TRUE, results = "hide")
  knitr::opts_template$set(dsviz = dopts)
  knitr::opts_template$set(dsvizi = diopts)
  # helper template for pprint knitr option
  knitr::opts_template$set(pprinter = pprintopts)
  # templates for various daff presentations
  knitr::opts_template$set(diffbasic = append(pprintopts, list(data_diff = TRUE, summary = TRUE, diff_table = FALSE)))
  knitr::opts_template$set(difftable = append(pprintopts, list(data_diff = TRUE, summary = FALSE, diff_table = TRUE)))
  knitr::opts_template$set(diffadvanced = append(pprintopts, list(data_diff = TRUE, summary = TRUE, diff_table = TRUE)))

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
  pprinter_func <- function(before, options, envir) {
    # TODO: handle both Python/R
    if (!before) {
      oldcode <- options$code
      last_line <- get_last_line(options)
      # cached_diff <- get_delta_cache(paste0(oldcode, collapse="\n"))
      # that would require checking the engine and handling R more directly with kableExtra
      if (options$pprint) {
        library(magrittr)
        if (length(options$code) > 0 && length(last_line) > 0) {
          # get all of the setup code and the code itself
          exercise_cache <- learnr:::get_exercise_cache(options$label)
          all_code <- get_exercise_code(exercise_cache)
          all_setup_code <- get_exercise_code(exercise_cache, setup = TRUE)
          # execute code if not empty
          if (!identical(all_code, "")) {
            debug_print(options, 'running python code')
            # first run the entire code to introduce variables to environment
            reticulate::py_run_string(all_setup_code)
            # then, evaluate last line to detect whether it's a dataframe
            raw_result <- reticulate::py_eval(last_line, convert=FALSE)
            converted_result <- reticulate::py_to_r(raw_result)
            # for data.frame, pretty print the output
            if(identical("data.frame", class(converted_result))) {
              debug_print(options, 'prepping a dataframe')
              options$results <- "asis"
              # options$code <- ""
              # wizard of oz pandas dataframe by changing index as well
              converted_result <- python_df(raw_result)
              out <- htmltools::knit_print.shiny.tag.list(
                reactable::reactable(
                  converted_result,
                  pagination = FALSE,
                  height = 300,
                  bordered = TRUE,
                  # rownames = TRUE,
                  resizable = TRUE
                )
              )
              debug_print(options, 'got the reactable')
              # if data_diff is requested, append the output with it
              if (length(options$data_diff)) {
                # debug_print(options, 'got the reactable')
                out <- c(out, get_daff_output(options))
              }
              # print('returning dataframe')
              debug_print(options, out)
              # store_delta_cache(paste0(oldcode, collapse="\n"), out)
              return(out)
            } else {
              debug_print(options, 'returning a non-dataframe will not need be pretty printed')
              # for everything else, use python engine as expected
              options$results <- "markdown"
              raw_result <- reticulate::py_eval(last_line, convert = FALSE)
              return(knitr::engine_output(options, oldcode, raw_result))
            }
          }
        }
      } else if(!options$pprint) {
        debug_print(options, 'returning something that does not need pretty printing')
        # for everything else, use python engine as expected
        options$results <- "markdown"
        raw_result <- reticulate::py_eval(last_line, convert = FALSE)
        return(knitr::engine_output(options, "", raw_result))
      }
    }
  }
  # this hook is to pretty print dataframes when it's the last line of python code
  knitr::knit_hooks$set(pprint = pprinter_func)

}
