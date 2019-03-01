

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
   name.year.count = NA,
   initialize = function(names.dt) {
     self$tmp.dir  <- file.path(home.dir, "tmp")
     self$data.dir <- file.path(home.dir, "data")
     self$zip.file <- paste(self$tmp.dir, "historico-nombres.zip", sep = "/")
     self$data.file <- paste(self$data.dir, "historico-nombres.csv", sep = "/")
     self
   },
   downloadData = function(){
     use.home.dir <- FALSE
     consent <- promptUser(prompt = paste("The folder", home.dir,
                          "will be created and more than 200mb of data will be",
                          "generated. Agree [y/n]?: "))
     #debug
     print(consent)
     if (consent == "y"){
       use.home.dir <- TRUE
       dir.create(self$tmp.dir, showWarnings = FALSE, recursive = TRUE)
       dir.create(self$data.dir, showWarnings = FALSE, recursive = TRUE)

       if ( !file.exists(self$zip.file)){
         data.url <-
           paste("http://infra.datos.gob.ar/catalog/otros/dataset/2/",
                 "distribution/2.1/download/historico-nombres.zip", sep = "")
         download.file(data.url,
                       destfile = self$zip.file)
         futile.logger::flog.info(paste("Downloading data from", data.url))
       }
     }
     if ( !file.exists(self$data.file)  & use.home.dir){
       unzip(self$zip.file, exdir = self$data.dir)
       futile.logger::flog.info(paste("Extracted from zip file", self$zip.file))
       lines.count <- trimws(system(paste("cat ", self$data.file, " | wc -l"),
                                    intern = TRUE))
       futile.logger::flog.info(paste("data.file has", lines.count,
                                      "rows"))
     }
     self$data.file
   },
   loadData = function(){
     self$name.year.count <- read_csv(file = self$data.file)
     names(self$name.year.count) <- c("name", "count", "year")
     self
   },
   getDestFilename = function(dir, testcase.name, extension ){
     paste(dir, "/", testcase.name, ".", extension, sep = "")
   },
   generateTestData = function(testcase.name,
                               dataset,
                               names = NULL,
                               years = sort(unique(self$name.year.count$year)),
                               dir.testcase = file.path("inst", "extdata")){
     filename.csv     <- self$getDestFilename(dir = tempdir(),
                                        testcase.name = testcase.name,
                                         extension = "csv")
     filename.zip <- self$getDestFilename(dir = dir.testcase,
                                        testcase.name = testcase.name,
                                         extension = "csv.zip")
     write.csv(dataset,
               filename.csv,
               row.names = FALSE)
     zip(filename.zip, filename.csv)
     futile.logger::flog.info(paste("Saved", nrow(filtered.names),
                                    "rows into compressed file",
                                    filename.zip, "using",
                                    round(file.info(filename.zip)$size / 1000),
                                    "kb"))
     filtered.names
   },
   readTestData = function(testcase.name,
                           dir.testcase = getPackageDir()){
     tmp.dir <- file.path(tempdir(), "unzip")
     dir.create(tmp.dir, showWarnings = FALSE)
     filename.csv <- self$getDestFilename(dir = tmp.dir,
                                          testcase.name = testcase.name,
                                          extension = "csv")
     filename.zip <- self$getDestFilename(dir = dir.testcase,
                                          testcase.name = testcase.name,
                                          extension = "csv.zip")


     unzip(zipfile = filename.zip, exdir = tmp.dir)

     ret <- readr::read.csv(filename.csv)
     futile.logger::flog.info(paste("Read", nrow(ret),
                                    "rows into compressed file",
                                    filename.zip, "using",
                                    round(file.info(filename.zip)$size / 1000),
                                    "kb"))
     unlink(tmp.dir)
     ret
   }
   ))
