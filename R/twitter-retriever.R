


#' TwitterRetriever
#' @importFrom R6 R6Class
#' @import lgr
#' @import twitteR
#' @export
TwitterRetriever.class <- R6Class("TwitterRetriever",
  public = list(
    #parameters
    interesting.keywords = NA,
    twitter.result.type = "mixed",
    target.countries  = NA,
    interesting.accounts = NA,
    min.followers = 5000,
    # users
    users = NULL,
    #trends
    trend.locations = NULL,
    trends = NULL,
    timestamp.trends.begin = NULL,
    timestamp.trends.end = NULL,

    # tweets
    tweets.unprocessed = NULL,
    # logger
    logger = NA,
    initialize = function(target.countries = "Argentina",
                          interesting.keywords = ""){
      #self$loadLog(date.min = date.min, date.max = date.max)
      self$target.countries <- target.countries
      self$interesting.keywords  <- self$normalize(interesting.keywords)
      self$users  <- TwitterUsers.class$new()

      self$tweets.unprocessed <- list()
      self$logger <- genLogger(self)
      self
    },
    normalize = function(x){
      tolower(x)
    },
    loadTrendsLocations = function(){
      self$trend.locations <- twitteR::availableTrendLocations()
      self$trend.locations
    },
    getTrendsLocations = function(countries = self$target.countries){
      if (is.null(self$trends)){
        self$loadTrendsLocations()
      }
      trend.locations <- self$trend.locations %>% filter(country %in% self$target.countries)
      trend.locations
    },
    getAllTrends = function(){
      self$timestamp.trends.begin <- Sys.time()
      trend.locations <- self$getTrendsLocations()

      interesting.keywords.regexp <- paste(self$interesting.keywords, collapse = "|")
      for (i in seq_len(nrow(trend.locations))){
        current.location <- trend.locations[i,]
        location.trends <- getTrends(current.location$woeid)
        head(location.trends)
        names(location.trends)
        location.trends$location <- current.location$name
        location.trends$country <- current.location$country
        location.trends$order   <- 1:nrow(location.trends)
        location.trends$name.normalized <- self$normalize(location.trends$name)

        location.trends$interesting.topic <- FALSE
        location.trends[grep(interesting.keywords.regexp, location.trends$name.normalized),]$interesting.topic <- TRUE
        self$trends <- rbind(self$trends, location.trends)
      }
      self$timestamp.trends.end <- Sys.time()
      self$trends
    },
    getSearchData = function(n = 100){
      trends.agg <- self$trends %>%
        filter(interesting.topic) %>%
        group_by(name, interesting.topic) %>%
        summarize (n = n(),
                   order = mean(order),
                   locations = paste(location, collapse = ",")) %>%
        arrange(order, desc(n))

      for (i in seq_len(nrow(trends.agg))){
        current.trend <- trends.agg[i,]
        current.term <- current.trend[,"name"]
        getLogger(self)$info(paste("Retrieving term", current.term, "with n =",n))
        tweets <- searchTwitteR(searchString = current.trend[i,"name"],
                                n = n)
        self$tweets.unprocessed[[as.character(current.trend[i,"name"])]] <- tweets
      }
    },
    classifyTweets = function(){
      for (term in self$tweets.unprocessed){

      }
    }
    ))


#' TwitterUsers.class
#' @importFrom R6 R6Class
#' @import lgr
#' @import twitteR
#' @export
TwitterUsers.class <- R6Class("TwitterUsers",
  public = list(
    users.df = NA,
    initialize = function(){

    },
    addUser = function(username, followers, following, tweets, country){

    }
))
