#' Remove Accents from text
#'
removeAccents <- function(text){
  ret <- iconv(text, to = "ASCII//TRANSLIT")
  ret <- gsub("'|\\~", "", ret)
  ret
}

#' Normalize String
#' Remove accents, trim and lower input text
normalizeString <- function(text){
  removeAccents(trimws(tolower(text)))
}


#'
#'
getPackageDir <- function(){
  home.dir <- find.package("twitterAnalytics", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata/")
  if (!dir.exists(file.path(home.dir, data.subdir)))
    data.subdir <- "extdata/"
  file.path(home.dir, data.subdir)
}

#'
#'
promptUser <- function(prompt){
  # #1: asking for consent but somehow is not working
  if ( interactive() ) {
    print("In interactive mode")
  } else {
    print("Not in interactive mode")
  }
  #FIXME Forcing a y
  print("FIXME: Forcing an y")
  readline(prompt = prompt)
  "y"
}

#' getLogger
#' @export
genLogger <- function(r6.object){
  lgr::get_logger(class(r6.object)[[1]])
}

#' getClassLogger
#' @export
getLogger <- function(r6.object){
  #debug
  #r6.object <<- r6.object
  #TODO check if not a better solution
  ret <- r6.object$logger
  if (is.null(ret)){
    class <- class(r6.object)[[1]]
    stop(paste("Class", class, "don't seems to have a configured logger"))
  }
  ret
}
