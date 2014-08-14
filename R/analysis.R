rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape2)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(scales)
library(xtable)
library(visreg)
library(lmerTest)

source("R//functions.R")

################
# Process  data#
################
# source("R/ProcessData.R")
load("output//data//FACE_IEM.RData")

#################
# Summary table #
#################
source("R/SummaryExlTable.R")

########
# Figs #
########
source("R/Figs.R")
# Don't worry about the warning message saying containing missing values. This
# is due to missing soil variable data during 1st incubation period.

#########
# Stats #
#########
source("R/Stats.R")


