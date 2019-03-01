

#' Names distributions
#'
#' Class for analysing time-distribution of names from a names database with fields: name, quantity, year
#'
#' @section Methods:
#' \describe{
#'   \item{\code{process()}}{}
#' }
#' @field
#' @format \code{\link{R6Class}} object.
#' @docType class
#' @importFrom R6 R6Class
#' @import dplyr
#' @importFrom reshape2 dcast
#' @export
NamesDistribution.class <- R6::R6Class("NamesDistribution",
  public = list(
    name.year.count = NA,
  initialize = function(name.year.count) {
    self$name.year.count <- name.year.count
    self
  },
  getFilteredNameYearCount = function(
          names = NULL,
          years = sort(unique(self$name.year.count$year))){
    filtered.names <- self$name.year.count
    filtered.names <- filtered.names %>%
      filter (year %in% years)
    if (!is.null(names)){
      filtered.names$name <- normalizeString(filtered.names$name)
      names <- normalizeString(names)
      filtered.names <- filtered.names %>%
        filter (name %in% names)
    }
    filtered.names
  },
  getNamesRanking = function(n = 20,
             names = NULL,
             years = sort(unique(self$name.year.count$year))){
    filtered.names <- self$getFilteredNameYearCount(names = names,
                                                    years = years)
    filtered.names %>%
      group_by(name) %>%
      summarize( count = sum(count)) %>%
      arrange(desc(count)) %>%
      top_n(n = n)
  },
  getNamesDistribution = function(names = NULL,
                                  years, relative = FALSE,
                                  decimals = 5){
    filtered.names <- self$getFilteredNameYearCount(names = names,
                                                    years = years)
    ret <- dcast(filtered.names,
            formula = name ~ year,
            value.var = "count",
            fun.aggregate = sum,
            fill = 0)
    data.cols <- 2:ncol(ret)
    total <- apply(ret[, data.cols], MARGIN = 1, FUN = sum)
    if (relative){
      for (i in seq_len(nrow(ret))){
        ret[i, data.cols] <- round(ret[i, data.cols] / total[i],
                                   digits = decimals)
      }
      #check
      #print(abs(apply(ret[,data.cols], MARGIN = 1, FUN = sum)-1) <
      #10^-(decimals-1))
    }
    ret <- ret[order(-total), ]

    ret
  },
  runSimulations = function(names.count,
                            years,
                            seed,
                            runs = 1){
    seeds <- round(runif(runs, 0, 10 ^ 8))
    futile.logger::flog.info(paste("Running", runs, "simulations using seeds",
                        paste(seeds, collapse = ",")))
    ret <- NamesDistributionSimulationMultipleRuns.class$new()

    #debug
    ret <<- ret
    for (i in seq_len(runs)){
      run <- self$simulateDistribution(names.count, years,
                                seed = seeds[i])
      ret$addSimulation(run)
    }
    ret
  },
  simulateDistribution = function(names.count,
                    years = sort(unique(self$name.year.count$year)),
                    seed){

    #Validate and prepare data
    mandatory.fields <- c("name", "count")
    valid.data <- c("name", "count") %in% names(names.count)
    if (min(valid.data) == 0){
      #If there is missing row
      stop(paste("Missing fields in names.count:",
                 "name and count must be present but there are:",
                 paste(names(names.count), collapse = ",")))
    }
    names.count <- names.count %>%
      group_by(name) %>% summarize( count = sum(count))

    names.count$name <- normalizeString(names.count$name)

    names.distribution  <- self$getNamesDistribution(years = years,
                                                     names = names.count$name,
                                                     relative = TRUE)

    futile.logger::flog.info(paste("Running simulation with seed", seed,
                     "for", nrow(names.count), "names using",
                     nrow(names.distribution), "names distributions"))

    set.seed(seed)
    ret <- names.distribution[0, ]
    i <- 1
    names.not.processed <- NULL
    for (current.name in names.count$name){
      current.name.count <- names.count %>% filter(name == current.name)
      current.name.distribution <-
              names.distribution %>%
                filter(name == current.name)
      if (nrow(current.name.distribution) > 0){
        cols.data <- 2:ncol(current.name.distribution)
        current.name.distribution <- current.name.distribution[, cols.data]
        current.name.sample <- sample(years, size = current.name.count$count,
                                      replace = TRUE,
                                      prob = current.name.distribution)
        sample.distribution <- data.frame(year = years) %>%
          left_join(data.frame(year = current.name.sample, count = 1),
                    by = "year") %>%
          mutate_each(funs(replace(., which(is.na(.)), 0))) %>%
          group_by(year) %>%
          summarize(count = sum(count))
        sample.distribution.tab <- t(sample.distribution)[2, ]
        names(sample.distribution.tab) <- sample.distribution$year
        sample.distribution.tab <- as.data.frame(t(sample.distribution.tab))
        sample.distribution.tab$name <- current.name
        ret[i, ] <- sample.distribution.tab[, names(ret)]
        i <- i + 1
      }
      else{
        names.not.processed <- c(names.not.processed, current.name)
      }
    }
    futile.logger::flog.info(paste(i, "names processed and",
                   length(names.not.processed),
                   "names not processed as not present in names distribution"))
    futile.logger::flog.debug(paste(names.not.processed, collapse = ","))

    total <- as.data.frame(t(apply(ret[, cols.data], MARGIN = 2, sum)))
    total$name <- "total"
    ret[i, ] <- total[, names(ret)]
    ret <- NamesDistributionSimulationRun.class$new(distribution.matrix = ret)
    ret
  }
))
