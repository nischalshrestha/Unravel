.onLoad <- function(libname, pkgname) {
  # since we need to mask some package functions, let's temporarily detach them
  detach_list <- c("package:dplyr", "package:tidyr")
  attach_list <- search()
  lapply(attach_list, function(x) {
    if (x %in% detach_list) {
      detach(x, character.only = TRUE)
    }
  })
  suppressMessages({
    library(dplyr)
    library(tidyr)
    library(tidylog)
  })
  # set tidylog messages to re-route to our tidylog_cache environment so we can access it
  # this is done after all the utility functions for getting/setting summaries are defined above
  options(
    "tidylog.display" = list(DataTutor:::store_verb_summary),
    "tidylog.callouts" = DataTutor:::store_line_callouts
  )
}
