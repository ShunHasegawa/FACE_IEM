rm(list=ls(all=TRUE))

source("R/pckg.R")

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

# save all objects so far
save.image(file = "output/data/Allobj.RData")

#########
# Stats #
#########
source("R/Stats.R")


