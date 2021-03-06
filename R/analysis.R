rm(list=ls(all=TRUE))

source("R/pckg.R")

source("R//functions.R")

################
# Process  data#
################
# source("R/ProcessData.R")
load("output//data//FACE_IEM.RData")
# add NP ratio
iem$logNP <- log(with(iem, (no + nh)/p))

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

# save all ovjects
save.image(file = "output//data/AllObj.RData")
