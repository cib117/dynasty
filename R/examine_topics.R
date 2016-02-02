#######################################################################################
##  Filename: examine_topics.r
##  Purpose: Examine relationship between dynastic status and topics
##  Assumes packages: stm, wordcloud, stringr
##  To install these packages run:
##  install.packages(c('stm','wordcloud','stringr'))
##  Output to plots/topics.png, plots/wordcloud.png, plots/coefplot.png
##  Last Edited: 2 Feb 2016
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages and Prepare Directory
#######################################################################################
rm(list=ls())
library(stm); library(wordcloud); library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
######################################################################################
## load data
######################################################################################
load('data/stm.RData')
######################################################################################
## Plot topics
######################################################################################
png('plots/topics.png', width = 7, height = 9, unit = 'in', res = 1200)
plot.STM(modout15, type = "summary", xlim = c(0, .3))
dev.off()
######################################################################################
## Wordcloud of top topic
######################################################################################
png('plots/wordcloud.png',width = 9, height = 9, unit = 'in', res = 1200)
cloud(modout15, topic = 10)
dev.off()
######################################################################################
## Estimate relationship between covariates and topic prevalence
######################################################################################
prep <- estimateEffect(1:15 ~ Dynastic + ff + gov, modout15, meta = out$meta, 
                       uncertainty = "Global")
## Plot coefficients
png('plots/coefplot.png', width = 9, height = 11, unit = 'in', res = 1200)
plot.estimateEffect(prep, covariate = "Dynastic", method = "difference",
                    cov.value1 = 1, cov.value2 = 0,
                    main = "Differences in topical prevalence between dynastic and non-dynastic",
                    xlim = c(-.04, .04), labeltype = "custom",
                    custom.labels = c("Topic 1", "Topic 2",
                                      "Topic 3", "Topic 4",
                                      "Topic 5", "Topic 6",
                                      "Topic 7", "Topic 8",
                                      "Topic 9", "Topic 10",
                                      "Topic 11", "Topic 12",
                                      "Topic 13", "Topic 14",
                                      "Topic 15"))
dev.off()