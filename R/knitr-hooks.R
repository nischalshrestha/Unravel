
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
  
  pprinter <- function(before, options, envir) {
    # TODO: same table format for both Python/R so that it's not confusing?
    # that would require checking the engine and handling R more directly with kableExtra
    if (!before && options$pprint) {
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
            raw_result <-  reticulate::py_eval(last_line, convert=FALSE)
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
              # TODO: if data_diff is requested, append the output with it
              if (options$data_diff) {
                diff_out <- data_diff(options)
                summary_statement <- 
                  "All **9** income variables have been deleted (red) and are now under 
                  `variable` (green), which added **162** rows. In other words, we changed 
                  the data from a wide format to a long format. We weren't modifying any 
                  existing values so there are no modified/reordered rows/columns."
                out <- c(out, "<br>", "**Data Diff Summary:**\n", summary_statement, diff_out)
              }
              return(knitr::engine_output(options, "", out))
            } else {
              # for everything else, use python engine as expected
              options$results <- "markdown"
              return(knitr::engine_output(options, options$code, raw_result))
            }
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
  }
  
  # this hook is to pretty print dataframes when it's the last line of python code
  knitr::knit_hooks$set(pprint = pprinter)
  
  data_diff <- function(options, include_summary = FALSE, include_diff_table = TRUE) {
    # colors for data diffing
    modified <- "#a0a0ff"
    green <- "#74ff74"
    red <- "#ff7374"
    
    old_pew <- as.data.frame(py$pew)
    new_pew <- as.data.frame(py$pew_long)
    diff <- daff::diff_data(old_pew, new_pew)
    
    summary_table_html <- ""
    
    if (include_summary) {
      #### Render the data diff summary table
      s <- daff:::summary.data_diff(diff)
      
      # Note: there are all the summaries that are available to us:
      # > ls(s)
      #  [1] "col_count_change_text"         "col_count_final"               "col_count_initial"
      #  [4] "col_deletes"                   "col_inserts"                   "col_renames"
      #  [7] "col_reorders"                  "col_updates"                   "data"
      # [10] "row_count_change_text"         "row_count_final"               "row_count_final_with_header"
      # [13] "row_count_initial"             "row_count_initial_with_header" "row_deletes"
      # [16] "row_inserts"                   "row_reorders"                  "row_updates"
      # [19] "source_name"                   "target_name"
      # These are the main ones we care about
      summary_tbl <- tibble::tribble(
        ~ "", ~ "#", ~Modified, ~Reordered, ~Deleted, ~Added,
        "Rows", s$row_count_change_text, s$row_updates, s$row_reorders, s$row_deletes, s$row_inserts,
        "Cols", s$col_count_change_text, s$col_updates, s$col_reorders, s$col_deletes, s$col_inserts,
      )
      
      # summary table
      summary_table_html <-
        kableExtra::kbl(summary_tbl) %>%
        kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "responsive")) %>%
        kableExtra::column_spec(3:4, background = modified) %>%
        kableExtra::column_spec(5, background = red) %>%
        kableExtra::column_spec(6, background = green)
    }
     
    diff_table_html <- ""
    if (include_diff_table) {
      
    #### Now render the data diff table
    library(dplyr)
    
    # TODO: change the file location (rn it's under project dir)
    daff::write_diff(diff, "patch.csv")
    diff_data <- readr::read_csv("patch.csv")
    diff_data <- as.data.frame(diff_data)
    
    # TODO: incorporate modified as well
    
    # get inserted rows
    row_vectors <- 1:length(rownames(diff_data))
    inserted_rows <- row_vectors[diff_data$`!` == "+++"]
    
    # get deleted cols
    deleted_col_names <- diff_data %>%
      select(starts_with("---")) %>%
      colnames()
    col_vectors <- 1:length(colnames(diff_data))
    deleted_cols <- col_vectors[colnames(diff_data) %in% deleted_col_names]
    
    # get inserted cols
    inserted_col_names <- diff_data %>%
      select(starts_with("+++")) %>%
      colnames()
    inserted_cols <- col_vectors[colnames(diff_data) %in% inserted_col_names]
    
    # get rid of daff-related schema row
    cols_to_add <- diff_data[1, 2:length(colnames(diff_data))]
    
    colnames(diff_data) <- diff_data[1, ]
    # get rid of daff-related ! column
    diff_data$`@@` <- NULL
    diff_data <- diff_data[-1, ]
    # wizard of oz python indexing
    rownames(diff_data) <- vapply(
      rownames(diff_data), 
      function(x) as.character(as.numeric(x) - 2), 
      character(1)
    )
    
    # TODO: see if you can also highlight column parts
    
    # TODO: instead of NULL for the columns that are deleted while overlapped with inserted rows, 
    # maybe we could replace with empty string
    # render a pretty printed table with highlighted modified, inserted, deleted rows/columns
    diff_table_html <- kableExtra::kbl(diff_data) %>%
      kableExtra::kable_styling(bootstrap_options = c("condensed", "responsive")) %>%
      kableExtra::row_spec(inserted_rows - 1, background = green) %>%
      kableExtra::column_spec(inserted_cols, background = green) %>%
      kableExtra::column_spec(deleted_cols, background = red) %>%
      kableExtra::scroll_box(width = "100%", height = "400px")
    }

    return(c(summary_table_html, diff_table_html))
  }
  
}
