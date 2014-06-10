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
library(scales)
library(xtable)
library(visreg)

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

##################
# soil variables #
##################
load("Data/FACE_TDR_ProbeDF.RData")

# subset iem
TdrIem <- subsetD(FACE_TDR_ProbeDF, Sample == "IEM")

# compute mean of soil variable for given period
SoilVarDD <- function(data, rings, plots, Start, End){
  sDF <- subset(data, Date >= Start & Date >= End & ring == rings & plot == plots)
  ddply(sDF, .(ring, plot),function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE))
}

IEMSoil <- ddply(iem, .(insertion, sampling, ring, plot), 
                 function(x) SoilVarDD(data = TdrIem, Start = x$insertion, End = x$sampling, rings = x$ring, plot = x$plot))

# merge
iem <- merge(iem, IEMSoil, by = c("insertion", "sampling", "ring", "plot"))

# save
save(iem, file = "output//data//FACE_IEM.RData")

load("output//data//FACE_IEM.RData")

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


