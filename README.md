# TwitterAnalytics 
Package with tools for making twitter analytics

# Example for infering age distribution from a names list based on demographical data

library(tibble)
library(readr)
library(dplyr)


```R

# Creation of NamesDistribution.class 
names.distribution.processor <- NamesDistribution.class$new(argentina.names.retriever$historic.names)

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

# Generating testcases

```R
# download dataset from [Historico de nombres](https://datos.gob.ar/dataset/otros-nombres-personas-fisicas). A mirror is at 
argentina.names.retriever <- ArgentinaNamesRetriever.class$new()
argentina.names.retriever$downloadData()
argentina.names.retriever$loadData()

# GEnerate 80s dataset
names.distribution.processor <- NamesDistribution.class$new(argentina.names.retriever$historic.names)


#Get top names used in 80s. Cant take some time
years.80s <- 1980:1990
popular.names <- names.distribution.processor$getNamesRanking(years = years, n = 5000)
name.year.count.80s <- names.distribution.processor$getFilteredNameYearCount(names = popular.names$name, years = years.80s)

dummy <- argentina.names.retriever$generateTestData(testcase.name = "argentina-80s",
										   dataset = name.year.count.80s,
										   years = 1980:1990)
```


# Installation

```R
devtools::install_github("kenarab/twitterAnalytics", build_opts = NULL)
library(twitterAnalytics)
```


