
#'
#'
removeAccents <- function(text){
  ret <- iconv(text, to = "ASCII//TRANSLIT")
  ret <- gsub("'|\\~", "", ret)
  ret
}

# '
normalizeString <- function(text){
  removeAccents(trimws(tolower(text)))
}


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
