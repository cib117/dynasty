#######################################################################################
##  Filename: dyn_time_plot.r
##  Purpose: Plot % of dyn legislators across time
##  Assumes packages:dplyr,ggplot2,stringr,tm,slam
##  To install these packages run:
##  install.packages(c('dplyr','stringr','ggplot2'))
##  Output to plots/dynasties_time.png
##  Last Edited: 2 Feb 2016
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages and Prepare Directory
#######################################################################################
rm(list = ls()) ## rm
library(ggplot2); library(dplyr); library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################################
## Get and prepare data
#######################################################################################
td <- read.csv('data/td_info.csv') ## td data
## join year up to dail start year
td$year <- 1973
td$year[td$dail == 21] <- 1977
td$year[td$dail == 22] <- 1981
td$year[td$dail == 23] <- 1982
td$year[td$dail == 24] <- 1983
td$year[td$dail == 25] <- 1987
td$year[td$dail == 26] <- 1989
td$year[td$dail == 27] <- 1997
td$year[td$dail == 28] <- 2002
td$year[td$dail == 29] <- 2007
## calculate dynastic percentage
td <- td %>% group_by(dail) %>% mutate(dynprop = mean(Dynastic))
#######################################################################################
## Get and prepare data
#######################################################################################
dyntime <- ggplot(td, aes(x = year, y = dynprop*100)) + 
  theme_bw() +
  ylab("Percentage of dynastic legislators") + ## y label
  xlab("Year") + ## x label
  geom_line() + ## line
  geom_point(size=2, shape=1)+ ## point
  ylim(0, 32)+ ## y axis limit
  theme(axis.title.x = element_text(size = 20, face = "bold"), ## bold face axis
        axis.title.y = element_text(size = 20,face = "bold"), ## bold face axis
        axis.text.x =element_text(size = 15), ## bigger axis ticks
        axis.text.y =element_text(size = 15)) ## bigger axis ticks
## save
ggsave('plots/dynasties_time.png',dyntime, width = 9.5, height = 9, units = 'in')
