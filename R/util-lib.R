
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
