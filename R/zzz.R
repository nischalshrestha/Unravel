# import the `pythontools` module,
# .onLoad <- function(libname, pkgname) {
#   # environment where the `pygradethis` objects will be stored
#   pkg_ns_env <- parent.env(environment())
#   #
#   # python_module <- reticulate::import_from_path(
#   #   module = "pythontools",
#   #   path = system.file("python", package = packageName())
#   # )
#   reticulate::source_python(
#     system.file("python/pythontools.py", package="DataTutor"),
#     envir=pkg_ns_env
#   )
#
# }
