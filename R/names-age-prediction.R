

#' Names distributions
#'
#' Class for analysing time-distribution of names from a names database with fields: name, quantity, year
#'
#' @section Methods:
#' \describe{
#'   \item{\code{process()}}{}
#' }
#' @field file.id polyhedron file id
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
  },
  getNamesRanking = function(n = 20,
                             names = NULL,
                             years = sort(unique(self$historic.names$year))){
    filtered.names <- self$historic.names
    if (!is.null(names)){
      filtered.names <- filtered.names %>%
        filter (name %in% names)
    }
    filtered.names %>%
      filter (year %in% years) %>%
      group_by(name) %>%
      summarize( count = sum(count)) %>%
      arrange(desc(count)) %>%
      top_n(n = n)
  },
  getNamesDistribution = function(years, names = NULL,
                                  relative = FALSE,
                                  decimals = 5){
    filtered.names <- self$historic.names
    if (!is.null(names)){
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
    ret <- ret[order(-total),]
    if (relative){
      for (i in seq_len(nrow(ret))){
        ret[i, data.cols] <- round(ret[i,data.cols]/total[i],
                                   digits = decimals)
      }
    }
    ret
  },
  simulateDistribution = function(distribution,
                                  names.count,
                                  seed){
    stop("Not implemented yet")

  }

))

