# TwitterAnalytics 
Package with tools for making twitter analytics

# Example for infering age distribution from a set of demographical data

library(tibble)
library(readr)
library(dplyr)


```R

# Read testcase data
argentina.names.retriever <- ArgentinaNamesRetriever.class$new()
dataset.argentina.80s <- argentina.names.retriever$readTestData("argentina-80s")

# Creation of NamesDistribution.class 
names.distribution.processor <- NamesDistribution.class$new(dataset.argentina.80s)

#Get top names used in the 80s
years.80s <- 1980:1990
popular.names <- names.distribution.processor$getNamesRanking(years = years.80s, n = 20)

names.distribution.processor$getNamesDistribution(names = popular.names$name,
	years = years.80s,
	relative = TRUE)


# Simulation of possible distribution for a name profile
names.sample <- read.csv(file = "inst/extdata/names.to.profile.csv")


sim.1 <- names.distribution.processor$simulateDistribution(names.count = names.sample, 
							   years = years.80s,  
							   seed = 12345)
sim.1b <- names.distribution.processor$simulateDistribution(names.count = names.sample,
							   years = years.80s,  
							   seed = 12345)

```

# Generating testcases

As original database is 200mb, process can take some processing time

```R
# download dataset from [Historico de nombres](https://datos.gob.ar/dataset/otros-nombres-personas-fisicas). A mirror is at 
argentina.names.retriever <- ArgentinaNamesRetriever.class$new()
argentina.names.retriever$downloadData()
argentina.names.retriever$loadData()

# Generate 80s dataset
names.distribution.processor <- NamesDistribution.class$new(argentina.names.retriever$historic.names)


# Get top names used in 80s. Can take some seconds

years.80s <- 1980:1990
popular.names <- names.distribution.processor$getNamesRanking(years = years, n = 5000)
name.year.count.80s <- names.distribution.processor$getFilteredNameYearCount(names = popular.names$name, years = years.80s)

dummy <- argentina.names.retriever$generateTestData(testcase.name = "argentina-80s",
						    dataset = name.year.count.80s,
						    years = 1980:1990)



# Generates a randomized names distribution

universe.size <- nrow(argentina.names.retriever$name.year.count)
set.seed(111111)

names.sample <- argentina.names.retriever$name.year.count[
					sample(1:universe.size,size = .01*universe.size, replace = FALSE),] %>% 
						group_by(name) %>%
					    summarize( count = sum(count)) %>% 
					    arrange(desc(count)) %>% 
					    filter(count >100)
nrow(names.sample)
# 749 names sampled
write.csv(names.sample, file = "inst/extdata/names.to.profile.csv", row.names = FALSE)


```


# Installation

```R
devtools::install_github("kenarab/twitterAnalytics", build_opts = NULL)
library(twitterAnalytics)
```


