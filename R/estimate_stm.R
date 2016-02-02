#######################################################################################
##  Filename: estimate_stm.r
##  Purpose: Estimate topic model
##  Assumes packages: stm, tm, stringr
##  To install these packages run:
##  install.packages(c('stm','tm','stringr'))
##  Output to data/stm.RData
##  Last Edited: 2 Feb 2016
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages and Prepare Directory
#######################################################################################
rm(list=ls())
library(stm); library(tm); library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################################
## Prepare metadata
#######################################################################################
## load data
metadata <- read.csv('data/metadata_td_merged.csv', head = T)
## check dimensions,names
dim(metadata)
names(metadata)
## create a logical vector for missing observations
miss <- is.na(metadata$Name)
## drop missing observations
metadata <- metadata[!miss, ]
## subset metadata
metadata <- metadata[, c('Dynastic', 'ff', 'dail', 'gov')]
## check dimensions
dim(metadata)
#######################################################################################
## Prepare docs for stm
#######################################################################################
## Load R image file containing dtm
load("data/ireland_reduced.RData")
## check dimensions
dim(dtm)
## remove docs where metadata is missing
dtm <- dtm[!miss,]
## convert dtm to estimate dtm
temp <- readCorpus(dtm, type = "slam")
## check dimensions
dim(dtm)
#######################################################################################
## Prepare stm format to match docs and metadata
#######################################################################################
out <- prepDocuments(temp$documents, temp$vocab, metadata)
#######################################################################################
## Estimate model
#######################################################################################
modout15 <- stm(out$documents, out$vocab, K = 15,
                data = out$meta, seed = 5926696)
save.image('data/stm.RData')