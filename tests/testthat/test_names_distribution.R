
context("NamesDistribution")
testthat::test_that("Generate names distribution with Argentina's names", {
  futile.logger::flog.info(paste("home.dir is ", home.dir))
  argentina.names.retriever <- ArgentinaNamesRetriever.class$new()
  years.80s <- 1980:1990
  dataset.argentina.80s <-
      argentina.names.retriever$readTestData("argentina-80s")
  argentina.names.retriever$loadData()
  argentina.names.retriever$name.year.count
  # Simulation of possible distribution for a name profile
  names.sample <-
    read.csv(file = file.path(getPackageDir(), "names.to.profile.csv"))

  #creating the processor object
  names.distribution.processor <-
    NamesDistribution.class$new(argentina.names.retriever  = argentina.names.retriever)
  names.distribution.processor$initDefaultValues()
  names.distribution.processor$setUpDistribution(names.count = names.sample,
                                                 years = years.80s)

  self$names.distribution  <- self$getNamesDistribution(years = years.80s,
                                                        names = names.sample$name,
                                                        relative = TRUE)

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
