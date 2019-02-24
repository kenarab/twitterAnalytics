

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
#' @import readr
#' @import futile.logger
#' @export
ArgentinaNamesRetriever.class <- R6::R6Class("ArgentinasNamesRetriever",
 public = list(
   tmp.dir = NA,
   data.dir = NA,
   zip.file = NA,
   data.file = NA,
   #state
   historic.names = NA,
   initialize = function(names.dt) {
     self$tmp.dir  <- file.path(home.dir, "tmp")
     self$data.dir <- file.path(home.dir, "data")
     dir.create(self$tmp.dir, showWarnings = FALSE, recursive = TRUE)
     dir.create(self$data.dir, showWarnings = FALSE, recursive = TRUE)
     self$zip.file <- paste(self$tmp.dir, "historico-nombres.zip", sep = "/")
     self$data.file <- paste(self$data.dir, "historico-nombres.csv", sep = "/")
     self
   },
   downloadData = function(){
     if ( !file.exists(self$zip.file)){
       data.url <- "http://infra.datos.gob.ar/catalog/otros/dataset/2/distribution/2.1/download/historico-nombres.zip"
       download.file(data.url,
                     destfile = self$zip.file)
       futile.logger::flog.info(paste("Downloading data from",data.url))
     }
     if ( !file.exists(self$data.file) ){
       unzip(self$zip.file, exdir = data.dir)
       futile.logger::flog.info(paste("Extracted from zip file",self$zip.file))

     }
     futile.logger::flog.info(paste("data.file has",
                                    system(paste("cat ", self$data.file, " | wc -l")),
                                           "rows"))
     self$data.file
   },
   loadData = function(){
     self$historic.names <- read_csv(file = self$data.file)
     names(self$historic.names) <- c("name", "count", "year")
     self
   },
   getDestFilename = function(dir, testcase.name, extension ){
     paste(dir, "/", testcase.name, ".", extension, sep = "")
   },
   generateTestData = function(testcase.name,
                               dataset,
                               names = NULL,
                               years = sort(unique(self$historic.names$year)),
                               testcase.folder = file.path("inst", "extdata")){
     filename     <- self$getDestFilename(dir = tempdir(), testcase.name = testcase.name,
                                           extension = "csv")
     filename.zip <- self$getDestFilename(dir = testcase.folder, testcase.name = testcase.name,
                                           extension = "csv.zip")
     write.csv(dataset,
               filename,
               row.names = FALSE)
     zip(filename.zip, filename)
     futile.logger::flog.info(paste("Saved", nrow(filtered.names), "rows into compressed file",
                                    filename.zip, "using", round(file.info(filename.zip)$size/1000),"kb"))
     filtered.names
   },
   readTestData = function(testcase.name,
                           testcase.folder = file.path("inst", "extdata")){
     filename.csv <- self$getDestFilename(dir = tempdir(), testcase.name = testcase.name,
                                               extension = "csv")
     filename.zip <- self$getDestFilename(dir = testcase.folder, testcase.name = testcase.name,
                                               extension = "csv.zip")
     unzip(filename.zip, exdir = tempdir())

     ret <- read.csv(filename.csv)
     futile.logger::flog.info(paste("Read", nrow(ret), "rows into compressed file",
                                    filename.zip, "using", round(file.info(filename.zip)$size/1000),"kb"))
     ret
   }
   ))
