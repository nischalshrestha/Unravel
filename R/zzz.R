.onLoad <- function(libname, pkgname) {
  # set tidylog messages to re-route to our tidylog_cache environment so we can access it
  require(tidylog)
  # this is done after all the utility functions for getting/setting summaries are defined above
  options(
    "tidylog.display" = list(DataTutor:::store_verb_summary),
    "tidylog.callouts" = DataTutor:::store_line_callouts
  )
}
