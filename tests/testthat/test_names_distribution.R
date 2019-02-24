
context("NamesDistribution")
testthat::test_that("Generate names distribution with argentina's names", {

  argentina.names.retriever <- ArgentinaNamesRetriever.class$new()
  years.80s <- 1980:1990
  dataset.argentina.80s <- argentina.names.retriever$readTestData("argentina-80s")

  # Simulation of possible distribution for a name profile
  names.sample <- read.csv(file = "inst/extdata/names.to.profile.csv")


  sim.1 <- names.distribution.processor$simulateDistribution(names.count = names.sample,
                                                             years = years.80s,
                                                             seed = 12345)
  sim.1b <- names.distribution.processor$simulateDistribution(names.count = names.sample,
                                                              years = years.80s,
                                                              seed = 12345)

  rows.diff <- sim.1$compareRows(sim.1b)
  testthat::expect_condition(is.null(rows.diff))
  # TODO tests
  #
  })
