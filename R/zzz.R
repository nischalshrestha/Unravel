
.onLoad <- function(libname, pkgname) {
  # the tidylog messages re-routes to our `tidylog_cache` environment (see utils.R) so we can access it
  # this is done after all the utility functions for getting/setting summaries are defined above
  options(
    "tidylog.display" = list(store_verb_summary),
    "tidylog.callouts" = store_line_callouts,
    "tidylog.fns_help" = store_fns_help
  )
}
