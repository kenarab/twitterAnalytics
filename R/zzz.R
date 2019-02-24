#' Executes code while loading the package.
#'
#' @param libname The library name
#' @param pkgname The package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  home.dir <<- "~/.R/twitterAnalytics"

}
