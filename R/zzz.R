.onLoad <- function(libname, pkgname) {
  suppressMessages(library(tidylog))
  # set tidylog messages to re-route to our tidylog_cache environment so we can access it
  # this is done after all the utility functions for getting/setting summaries are defined above
  options(
    "tidylog.display" = list(DataTutor:::store_verb_summary),
    "tidylog.callouts" = DataTutor:::store_line_callouts
  )
}
