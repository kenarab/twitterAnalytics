
context("NamesDistribution")
testthat::test_that("Generate names distribution with Argentina's names", {
  futile.logger::flog.info(paste("home.dir is ", home.dir))
  argentina.names.retriever <- ArgentinaNamesRetriever.class$new()
  years.80s <- 1980:1990
  dataset.argentina.80s <-
      argentina.names.retriever$readTestData("argentina-80s")

  # Simulation of possible distribution for a name profile
  names.sample <-
    read.csv(file = file.path(getPackageDir(), "names.to.profile.csv"))

  #creating the processor object
  names.distribution.processor <-
    NamesDistribution.class$new(dataset.argentina.80s)

  sim.1 <-
    names.distribution.processor$simulateDistribution(
           names.count = names.sample,
           years = years.80s,
           seed = 12345)

  sim.1b <-
    names.distribution.processor$simulateDistribution(
            names.count = names.sample,
            years = years.80s,
            seed = 12345)

  rows.diff <- sim.1$compareRows(sim.1b)
  testthat::expect_true(is.null(rows.diff))
  # TODO tests
  #
  })
