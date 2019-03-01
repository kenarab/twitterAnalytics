# TwitterAnalytics 
Package with tools for making twitter analytics

# Infering age distribution from a set of names. 

Example for infering age distribution in a set of names, running simulations derived of demographical data.

```R
# Load libraries
library(twitterAnalytics)
library(tibble)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(futile.logger)


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
names.year.sample <- read.csv(file = "inst/extdata/names.to.profile.csv")


#Generates 10 simulations 
sim.results <- names.distribution.processor$runSimulations(names.count = names.sample, 
							   years = years.80s,  
							   runs = 10,
							   seed = 12345)


results.norm <- sim.results$getResult()

ggplot <- ggplot(data = results.norm) +
  geom_point(aes(x = year, y = count, group = run, color = run))+
  geom_line(data=sim.results$statistics, aes(x = year, y = mean), color = "black")+
  geom_smooth(data=sim.results$statistics, aes(x = year, y = mean), color = "black")+
  ggtitle("Simulated distributions")

            

ggplot


```

# Generating sample data and testcases

As original database is 200mb, processing full database can take some time. A method for generating datasets for development and testcases generation is shown.

## Testcase generation example
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
name.year.count.80s <- names.distribution.processor$getFilteredNameYearCount(
									names = popular.names$name, 
									years = years.80s)

dummy <- argentina.names.retriever$generateTestData(testcase.name = "argentina-80s",
									dataset = name.year.count.80s,
									years = 1980:1990)

```



## Generation of a randomized names distribution dataset

```R

names.year.sample <- generateSampleDistribution(
                          argentina.names.retriever$name.year.count,
                          seed = 11111)
nrow(names.year.sample)
# 749 names sampled
write.csv(names.year.sample, file = "inst/extdata/names.to.profile.csv", row.names = FALSE)
```


# Installation

```R
devtools::install_github("kenarab/twitterAnalytics", build_opts = NULL)
library(twitterAnalytics)
```


