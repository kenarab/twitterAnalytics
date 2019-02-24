

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
ArgentinasNamesRetriever.class <- R6::R6Class("ArgentinasNamesRetriever",
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
     self$zip.file <- paste(tmp.dir, "historico-nombres.zip", sep = "/")
     self$data.file <- paste(data.dir, "historico-nombres.csv", sep = "/")
     self
   },
   downloadData = function(){
     if ( !file.exists(self$zip.file) ){
       download.file("http://infra.datos.gob.ar/catalog/otros/dataset/2/distribution/2.1/download/historico-nombres.zip",
                     destfile = self$zip.file)
     }
     if ( !file.exists(self$data.file) ){
       unzip(self$zip.file, exdir = data.dir)
     }
     futile.logger::flog.info(paste("data.file has",
                                    system(paste("cat ", data.file, " | wc -l")),
                                           "rows"))
     self$data.file
   },
   loadData = function(){
     self$historic.names <- read_csv(file = self$data.file)
     names(historic.names) <- c("name", "count", "year")
     self$historic.names
   }
   ))
