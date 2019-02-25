
#'
#'
removeAccents<-function(text){
  ret<-iconv(text, to='ASCII//TRANSLIT')
  ret<-gsub("'|\\~","",ret)
  ret
}

# '
normalizeString<-function(text){
  removeAccents(trimws(tolower(text)))
}


getPackageDir <- function(){
  home.dir <- find.package("twitterAnalytics", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata/")
  if (!dir.exists(file.path(home.dir, data.subdir)))
    data.subdir <- "extdata/"
  file.path(home.dir, data.subdir)
}
