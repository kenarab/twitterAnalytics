


#' TwitterRetriever
#' @importFrom R6 R6Class
#' @import lgr
#' @import twitteR\
#' @import digest
#' @export
TwitterRetriever.class <- R6Class("TwitterRetriever",
  public = list(
    #parameters
    interesting.keywords = NA,
    twitter.result.type = "mixed",
    target.countries  = NA,
    interesting.accounts = NA,
    # users
    users = NULL,
    #trends
    trend.locations = NULL,
    trends = NULL,
    timestamp.trends.begin = NULL,
    timestamp.trends.end = NULL,

    # tweets
    tweets.unprocessed = NULL,
    tweets.processed   = NULL,
    # logger
    errors.count = 0,
    logger = NA,
    initialize = function(target.countries = "Argentina",
                          interesting.keywords = "",
                          users = NULL,
                          min.followers = 500){
      #self$loadLog(date.min = date.min, date.max = date.max)
      self$target.countries <- target.countries
      self$interesting.keywords  <- self$normalize(interesting.keywords)
      if (is.null(users)){
        self$users  <- TwitterUsers.class$new(min.followers = min.followers)
      }
      else{
        self$users  <- users
        self$users$min.followers <- min.followers
      }
      self$tweets.unprocessed <- list()
      self$tweets.processed <- list()
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
        current.term <- as.character(current.trend[,"name"])
        getLogger(self)$info(paste("Retrieving term", current.term, "with n =",n))
        tweets <- searchTwitteR(searchString = current.term,
                                n = n)
        self$tweets.unprocessed[[current.term]] <- tweets
      }
    },
    classifyTweets = function(max.tweets = 0){
      logger <- getLogger(self)
      total.tweets <- lapply(self$tweets.unprocessed, FUN = length)

      logger$info(paste("Processing", sum(unlist(total.tweets)), "tweets for", length(total.tweets), "terms"))
      counter <- 0
      for (term in names(self$tweets.unprocessed)){
        term.tweets <- self$tweets.unprocessed[[term]]
        logger$info(paste("Processing", length(term.tweets), "tweets for", "term", term))
        for (i in seq_len(length(term.tweets))){
          current.tweet <- term.tweets[[1]]
          if (self$users$addUser(current.tweet =  current.tweet)){
            self$addProcessedTweet(current.tweet)
          }
          counter <- counter + 1
          term.tweets[[1]] <- NULL
          continue.processing <- (max.tweets ==0 | (counter+1) <= max.tweets)
          if (!continue.processing){
            break
          }
        }
        if (!continue.processing){
          break
        }
      }
      counter
    },
    addProcessedTweet = function(current.tweet){
      tweet.id <- current.tweet$id
      if (!tweet.id %in% names(self$tweets.processed)){
        self$tweets.processed[[tweet.id]] <- current.tweet
      }
      else{
        getLogger(self)$error(paste("Tweet", tweet.id, "already processed"))
        self$errors.count <- self$errors.count + 1
      }
    },
    storeTweets = function(){
      store_tweets_db(self$tweets.processed)

    },
    retrieveTweets = function(){

    }
    ))


#' TwitterUsers.class
#' @importFrom R6 R6Class
#' @import lgr
#' @import twitteR
#' @export
TwitterUsers.class <- R6Class("TwitterUsers",
  public = list(
    min.followers = NA,
    user.fields = NA,
    rejected.fields = NA,
    #state
    users.df = NULL,
    users.rejected.df = NULL,
    tweets.counter = NULL,
    # logger
    logger = NA,
    initialize = function(min.followers = 500){
      self$min.followers <- min.followers
      self$setupUserFields()
      self$tweets.counter <- list()
      self$logger <- genLogger(self)
      self
    },
    setupUserFields = function(){
      self$user.fields <- c("id", "screenName", "created", "followersCount", "friendsCount",
        "favoritesCount", "listedCount", "name", "verified", "profileImageUrl",
        "getLocation")
      self$rejected.fields <- c("id", "screenName", "created", "followersCount")
      self
    },
    addUser = function(current.tweet){
      screen.name <- current.tweet$screenName
      getLogger(self)$debug(paste("Processing user", screen.name, "with tweet", current.tweet$text))
      if (!screen.name %in% c(self$users.df$screenName, self$users.rejected.df$screenName)){
        user <- getUser(screen.name)
        user.df <- data.frame(user$id, stringsAsFactors = FALSE)
        for (field in self$user.fields){
          user.df[,field] <- user[[field]]
        }
        if (user.df$followersCount >= self$min.followers){
          self$users.df <- rbind(self$users.df, user.df)
        }
        else{
          self$users.rejected.df <- rbind(self$users.rejected.df, user.df[,self$rejected.fields])
        }
      }
      if (screen.name %in% self$users.df$screenName){
        count <- self$tweets.counter[[screen.name]]
        count <- ifelse(is.null(count), 0, count)
        self$tweets.counter[[screen.name]] <- count + 1
      }
      !user$screenName %in% self$users.rejected.df$screenName
    },
    getCounterAsDataFrame = function(){
      ret <- data.frame(screenName = names(self$tweets.counter),
                        count      = unlist(self$tweets.counter))
      ret <- ret[order(ret$count, decreasing = TRUE),]
      ret
    }
))
