rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(xtable)

source("R//functions.R")

################
# Process  data#
################
iem <- read.csv("Data/FACE_IEM.csv", colClasses=c("ring"="factor","plot"="factor","time"="factor",
                                                  "coverage" = "NULL", "actual.cov" = "NULL"))

# reorder time
levels(iem$time)
iem$time <- factor(iem$time, levels = c(as.character(1:length(levels(iem$time)))))

#unify date for each time
iem$insertion <- as.Date(dmy(iem$insertion))
iem$sampling <- as.Date(dmy(iem$sampling))
iem$date <- as.Date(ave(apply(cbind(iem$insertion, iem$sampling), 1, mean), iem$time), origin = origin) # same date for same time

# change the unit from ug to ng
iem[, c("no", "nh", "p")] <- iem[, c("no", "nh", "p")] * 1000

# add id for later analysis
iem$id <- iem$ring:iem$plot

# pre or post co2
# time 4 is pre-co2 but need to be in post as well 
# as it's used as a baseline
iem$pre <- ifelse(iem$time %in% c(1:4), TRUE, FALSE )
iem$post <- ifelse(!(iem$time %in% c(1:3)), TRUE, FALSE )

#save
save(iem,file="output/data/FACE_IEM.RData")

#################
# Summary table #
#################
source("R/SummaryExlTable.R")

########
# Figs #
########
source("R/Figs.R")

#########
# Stats #
#########
source("R/Stats.R")


