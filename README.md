# TwitterAnalytics 
Package with tools for making twitter analytics

# Example for infering age distribution from a names list based on demographical data

library(tibble)
library(readr)
library(dplyr)


```R
# download dataset from [Historico de nombres](https://datos.gob.ar/dataset/otros-nombres-personas-fisicas). A mirror is at 
home.dir <- "~/.R/twitterAnalytics"
tmp.dir  <- file.path(home.dir, "tmp")
data.dir <- file.path(home.dir, "data")
dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data.dir, showWarnings = FALSE, recursive = TRUE)

zip.file <- paste(tmp.dir, "historico-nombres.zip", sep = "/")
data.file <- paste(data.dir, "historico-nombres.csv", sep = "/")

if ( !file.exists(data.file) ){
	download.file("http://infra.datos.gob.ar/catalog/otros/dataset/2/distribution/2.1/download/historico-nombres.zip",
	              destfile = zip.file)
    unzip(zip.file, exdir = data.dir)

}




#Check rows of file before uploading
system(paste("cat ", data.file, " | wc -l"))

# read data
historic.names <- read_csv(file = data.file)
names(historic.names) <- c("name", "count", "year")
head(historic.names)

#Generate analytics object
names.distribution.processor <- NamesDistribution.class$new(historic.names)

#Get top names used from 1970 to 2015 (last year with data)
years.from.70 <- 1970:2015
popular.names <- names.distribution.processor$getNamesRanking(years = years.from.70, n = 20)

names.distribution.processor$getNamesDistribution(names = popular.names$name,
	years = years.from.70,
	relative = TRUE)


# Generates a randomized usernames distribution

universe.size <- nrow(historic.names)
set.seed(111111)
names.sample <- historic.names[sample(1:universe.size,size = .001*universe.size, replace = FALSE),] %>% 
						group_by(name) %>%
					    summarize( count = sum(count)) %>% 
					    arrange(desc(count)) %>% 
					    filter(count >100)
nrow(names.sample)
# 4132 names sampled
sim.1 <- names.distribution.processor$simulateDistribution(names.count = names.sample, 
												 years = years.from.70,  
												 seed = 12345)
sim.1b <- names.distribution.processor$simulateDistribution(names.count = names.sample,
												 years = years.from.70,  
												 seed = 12345)

```




# Installation

```R
devtools::install_github("kenarab/twitterAnalytics", build_opts = NULL)
library(twitterAnalytics)
```


