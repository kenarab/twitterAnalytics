library(testthat)
library(twitterAnalytics)
library(tibble)
library(readr)
library(dplyr)
library(reshape2)
library(futile.logger)


#Change threshold to ERROR. Comment out/change if verbosity required for development
futile.logger::flog.threshold(futile.logger::ERROR)

#' getDataDirMockedTest mocked function for a temp dest folder for testing proposes



testthat::test_check("twitterAnalytics")
