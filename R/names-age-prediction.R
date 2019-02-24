

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
    historic.names = NA,
  initialize = function(names.dt) {
    self$historic.names <- historic.names
    self
  },
  getNamesRanking = function(n = 20,
                             names = NULL,
                             years = sort(unique(self$historic.names$year))){
    filtered.names <- self$historic.names
    filtered.names %>%
      filter (year %in% years)
    if (!is.null(names)){
      filtered.names <- filtered.names %>%
        filter (name %in% names)
    }
    filtered.names %>%
      group_by(name) %>%
      summarize( count = sum(count)) %>%
      arrange(desc(count)) %>%
      top_n(n = n)
  },
  getNamesDistribution = function(years, names = NULL,
                                  relative = FALSE,
                                  decimals = 5){
    filtered.names <- self$historic.names
    filtered.names <- filtered.names %>% filter(year %in% years)
    if (!is.null(names)){
      filtered.names$name <- normalizeString(filtered.names$name)
      names <- normalizeString(names)
      filtered.names <- filtered.names %>%
        filter (name %in% names)
    }
    ret <- dcast(filtered.names,
            formula = name ~ year,
            value.var = "count",
            fun.aggregate = sum,
            fill = 0)
    data.cols <- 2:ncol(ret)
    total <- apply(ret[,data.cols], MARGIN = 1, FUN = sum)
    if (relative){
      for (i in seq_len(nrow(ret))){
        ret[i, data.cols] <- round(ret[i,data.cols]/total[i],
                                   digits = decimals)
      }
      #check
      #print(abs(apply(ret[,data.cols], MARGIN = 1, FUN = sum)-1)<10^-(decimals-1))
    }
    ret <- ret[order(-total),]

    ret
  },
  simulateDistribution = function(names.count,
                                  years = sort(unique(self$historic.names$year)),
                                  seed){
    #Validate and prepare data
    mandatory.fields <- c("name", "count")
    valid.data <- c("name", "count") %in% names(names.count)
    if (min(valid.data)==0){
      #If there is missing row
      stop(paste("Missing fields in names.count: name and count must be present but there are:",
                 paste(names(names.count), collapse= ",")))
    }
    names.count <- names.count %>%
      group_by(name) %>% summarize( count = sum(count))

    names.count$name <- normalizeString(names.count$name)

    names.distribution  <- self$getNamesDistribution(years = years,
                                                     names = names.count$name,
                                                     relative = TRUE)
    set.seed(seed)
    ret <- names.distribution[0,]
    i <- 1
    for (current.name in names.count$name){
      current.name.count <- names.count %>% filter(name == current.name)
      current.name.distribution <- names.distribution %>% filter(name == current.name)
      cols.data <- 2:ncol(current.name.distribution)
      current.name.distribution <- current.name.distribution[,cols.data]
      current.name.sample <- sample(years, size = current.name.count$count,
                                    replace = TRUE, prob = current.name.distribution)
      sample.distribution <- data.frame(year = years) %>%
                                left_join(data.frame(year = current.name.sample, count = 1),
                                          by = "year") %>%
                                mutate_each(funs(replace(., which(is.na(.)), 0))) %>%
                                group_by(year) %>%
                                summarize(count = sum(count))
      sample.distribution.tab <- t(sample.distribution)[2,]
      names(sample.distribution.tab) <- sample.distribution$year
      sample.distribution.tab <- as.data.frame(t(sample.distribution.tab))
      sample.distribution.tab$name <- current.name
      ret[i,] <- sample.distribution.tab[,names(ret)]
      i <- i +1
    }
    total <- as.data.frame(t(apply(ret[,cols.data], MARGIN = 2, sum)))
    total$name <- "total"
    ret[i,] <- total
    ret <- NamesDistributionSimulation.class$new(distribution.matrix = ret)
    ret
  }

))


NamesDistributionSimulation.class <- R6::R6Class("NamesDistributionSimulation",
   public = list(
     distribution.matrix = NA,
     initialize = function(distribution.matrix) {
       self$distribution.matrix <- distribution.matrix
       self
     },
     compareTo = function(names.distribution.simulation){
       a <- self$distribution.matrix
       b <- names.distribution.simulation$distribution.matrix
     }
   ))
