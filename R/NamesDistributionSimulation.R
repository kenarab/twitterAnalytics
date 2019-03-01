#' Names Distribution Simulation abstract class
#'
NamesDistributionSimulation.class <- R6::R6Class("NamesDistributionSimulation",
   public = list(
     distribution.matrix = NA,
     initialize = function(distribution.matrix) {
       self$distribution.matrix <- distribution.matrix
       self
     },
     compareRows = function(sim.2){
       rows.with.differences <- NULL
       a <- self$distribution.matrix
       b <- sim.2$distribution.matrix
       for (i in seq_len(nrow(a))){
         current.name.a <- a[i, ]
         current.name.b <- b %>% filter(name == current.name.a$name)
         if (nrow(current.name.b) == 1){
           row.diff <- current.name.a == current.name.b
           if (min(row.diff) == 0){

           }
         }
         else{
           rows.with.differences <- c(rows.with.differences, i)
         }
       }
     },
     compareTo = function(sim.2){
       self$compareRows(sim.2)
       length(rows.with.differences) == 0
     }
   ))




#' Names Distribution Simulation Run is a container class of a unique simulation
#' @export
NamesDistributionSimulationRun.class <-
  R6::R6Class("NamesDistributionSimulationRun",
  inherit = NamesDistributionSimulation.class,
  public = list(
    initialize = function(distribution.matrix) {
      super$initialize(distribution.matrix)
      self
    }
  ))


#' Names Distribution Simulation Multiple Runs is a container class of multiple simulations
#' @export
NamesDistributionSimulationMultipleRuns.class <-
  R6::R6Class("NamesDistributionSimulationMultipleRuns",
  inherit = NamesDistributionSimulation.class,
  public = list(
    #state
    runs = NULL,
    runs.df = NULL,
    statistics = NULL,
    initialize = function() {
      super$initialize(NULL)
      self$runs <- list()
      self
    },
    addSimulation = function(simulation.output){
      new.simulation <- simulation.output$distribution.matrix
      new.simulation.total <- new.simulation  %>% filter(name == "total.relative")
      new.simulation.total <- new.simulation.total[,
                                           2:ncol(new.simulation.total)]

      self$runs[[as.character(length(self$run))]] <- new.simulation

      self$runs.df <- rbind(self$runs.df, new.simulation.total)
      self$statistics <- data.frame(year= as.numeric(names(self$runs.df)))
      self$statistics$mean <- apply(self$runs.df, MARGIN = 2, FUN = mean)
      self$statistics$sd   <- apply(self$runs.df, MARGIN = 2, FUN = sd)
      self$statistics
    },
    getResult = function(){
      ret <- NULL
      for (i in seq_len(nrow(self$runs.df))){
        for (j in 1:ncol(self$runs.df)){
          ret <- rbind(ret,
                       data.frame(run = i,
                                  year = as.numeric(names(self$runs.df)[j]),
                                  count = self$runs.df[i, j]))
        }
      }
      ret
    },
    testDistribution = function(names.year.sample, graph = TRUE){
      actual.distribution <-self$statistics
      #names(actual.distribution) <- self$statistics$year
      expected.distribution <- names.year.sample %>%
                                group_by(year) %>%
                                filter(year %in% self$statistics$year) %>%
                                summarize(count = sum(count))
      expected.distribution$proportion <- expected.distribution$count/sum(expected.distribution$count)
      #names(expected.distribution) <- year.sample$year
      #expected.distribution <- expected.distribution/sum(expected.distribution)
      if (graph){
        ggplot <- ggplot() +
        geom_line(data=actual.distribution, aes(x = year, y = mean, colour = "actual"))+
          geom_line(data=expected.distribution, aes(x = year, y = proportion, colour = "expected"))
        ggplot
      }
      chisq.test(actual.distribution, expected.distribution)
    }
  ))
