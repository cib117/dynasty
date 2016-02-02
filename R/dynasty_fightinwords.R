#######################################################################################
##  Filename: bw_fightinwords.r
##  Purpose: Examine language differences between dynastic
##  and non-dynastic legislators.
##  Assumes packages:dplyr,ggplot2,stringr,tm,slam
##  To install these packages run:
##  install.packages(c('dplyr','stringr','tm','slam','ggplot2'))
##  Output to plots/dynasties_fw.png
##  Last Edited: 2 Feb 2016
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages and Prepare Directory
#######################################################################################
rm(list = ls())
library(tm); library(slam); library(dplyr); library(ggplot2); library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################################
## Get and prepare data
#######################################################################################
load('data/ireland_reduced.RData')
metadata <- read.csv('data/metadata_reduced.csv')
## Create a dail variable
metadata$dail <- rep(NA, nrow(metadata))
names(metadata)
head(metadata)
metadata$dail[metadata$date < 19770616] <- 20
metadata$dail[metadata$date >= 19770616 & metadata$date <= 19810527] <- 21
metadata$dail[metadata$date >= 19810611 & metadata$date <= 19820127] <- 22
metadata$dail[metadata$date >= 19820218 & metadata$date <= 19821104] <- 23
metadata$dail[metadata$date >= 19821124 & metadata$date <= 19870217] <- 24
metadata$dail[metadata$date >= 19870310 & metadata$date <= 19890525] <- 25
metadata$dail[metadata$date >= 19890615 & metadata$date <= 19921105] <- 26
metadata$dail[metadata$date >= 19921125 & metadata$date <= 19970515] <- 27
metadata$dail[metadata$date >= 19970626 & metadata$date <= 20020425] <- 28
metadata$dail[metadata$date >= 20020517 & metadata$date <= 20070426] <- 29

## Import TD information
td_data <- read.csv("data/td_info.csv")
## Merge the two dataframes
metadata.final <- left_join(metadata, td_data, by = c("speaker","dail"))
## delete multiple matches from left_join
metadata.final <- distinct(metadata.final, X.x)

## Get Dynastic and non Dynastic wordcounts
dyn_rows <-(metadata.final$Dynastic > 0) ## question is by dynastic leg
ndyn_rows <- (metadata.final$Dynastic == 0) ## question is by non-dynastic leg
dyn_dtm <- dtm[which(dyn_rows),] ## extract questions by dynastic leg
ndyn_dtm <- dtm[which(ndyn_rows),] ## extract questions by non-dynastic leg
dyncount <- col_sums(dyn_dtm) ## number of times word w is used by dynastic legislators
ndyncount <- col_sums(ndyn_dtm) ## number of times word w is used by nondynastic legislators

#######################################################################################
## Use Monroe et al.'s (2008) method to examine differences in words usage
#######################################################################################
words <- colnames(dtm) ## words
W <- length(words) ## number of words in dtm
y <- matrix(0, 2, W) ## create empty 2*W matric
colnames(y) <- words ## words are column names
rownames(y) <- c("g1", "g2") ## generic row names
y[1,as.character(words)] <- dyncount ## freq of word w for dynastic legis (y_kw^D)
y[2,as.character(words)] <- ndyncount## freq of word w for nondynastic legis (y_kw^N)
y.tot <- colSums(y) ## freq of word w in corpus (y_kw)
Y <- sum(y.tot) ## total words in corpus (n_K)
## prior
kappa <- 100000000 ## strong prior to reduce number of significant words
y.prior <- y.tot/Y ## freq of word w/total words in corpus (y_kw/n_k)
k.prior <- kappa*y.prior
## numerator of estimate for d
y.1.a <- y[1,] + k.prior*sum(y[1,])/sum(y.tot)
## numerator of estimate for n
y.2.a <- y[2,] + k.prior*sum(y[2,])/sum(y.tot)
## log-odds estimate for d
theta.1 <- log(y.1.a/(sum(y.1.a) - y.1.a))
## log-odds estimate for n
theta.2 <- log(y.2.a/(sum(y.2.a) - y.2.a))
## estimate
delta <- theta.1 - theta.2
## standard error of estimate
se <- sqrt(1/y.1.a + 1/y.2.a)
## ## z-statistic
zeta <- delta/se
#######################################################################################
## Plot estimates against word frequency as in Monroe et al.'s (2008) article
#######################################################################################
## Setup data frame for plotting
plotdf <- as.data.frame(cbind(log(y.tot), zeta)) ## log scale for plot
plotdf$words <- rownames(plotdf)
colnames(plotdf)[1] <- 'freq'
## create variables for plot
plotdf$dynwords <- ifelse(plotdf$zeta > 0, 1, 0) ## >0 are dyn words
plotdf$dynwords[abs(plotdf$zeta) < 1.96] <- 2 ## >1.96 are significant
## create cutoffs for plot
plotdf$size <- 0
plotdf$size[abs(plotdf$zeta) < 2] <- 1
plotdf$size[abs(plotdf$zeta) >= 2 & abs(plotdf$zeta) <= 5] <- 2
plotdf$size[abs(plotdf$zeta) > 5] <- 3
## Plot logged counts and zeta scores
fightinwords <- ggplot(plotdf,aes(x = freq, y = zeta, label = words)) +
  theme_bw() + ## bw gg theme
  geom_text(aes(size = size,colour = factor(dynwords))) + ## control size, colour
  scale_size_continuous(range = c(1,8)) +  ## size for text
  xlab("Frequency of word use") + ## x label
  ylab("Estimated difference in word use") + ## y label
  scale_color_manual(values = c("orange1", "steelblue1","grey")) + ## colours
  scale_x_continuous(breaks = c(log(100),log(1000),log(10000),log(100000)), ## x-axis points
                     labels = c("100", "1000", "10000","100000"))+ ## x-axis labels
  theme(legend.position="none",
        axis.title.x = element_text(size=20,face="bold"), ## axis size and face
        axis.title.y = element_text(size=20,face="bold"), ## axis size and face
        axis.text.x =element_text(size=15), ## label size
        axis.text.y =element_text(size=15), ## label size
        panel.border = element_blank(), ## no border
        panel.grid.major = element_blank(), ## no grid
        panel.grid.minor = element_blank()) + ## no grid
  annotate("text", x = c(4.5,4.5), y = c(12,-12), label = c("Dynastic","Other"),
           colour = c("steelblue1","orange1"), fontface="bold",size = 10)

ggsave('plots/dynasties_fw.png',fightinwords, width = 13,height = 9, units = 'in')