#' Executes code while loading the package.
#'
#' @param libname The library name
#' @param pkgname The package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  home.dir <<- "~/.R/twitterAnalytics"
  twitter.analytics.process.manager <<- experimentR::ABProcessRecord.class$new(record.name="darknet-iq")

}
