library(lubridate)
library(plyr)
library(reshape)

source("R/functions.R")

############################
# Correct NO3 based on CCV #
############################
fls <- dir(path = "Data/AQ2/NO3_NeedToBeCorrected/", pattern = ".csv$")

Crrtct.ccv.df(fls[1])
write.csv(Crrtct.ccv.df(fls[1]), paste("Data/AQ2/ReadyToProcess/", "Corrected_", fls[1], sep =""), row.names = TRUE)

Crrtct.ccv.df(fls[2])
write.csv(Crrtct.ccv.df(fls[2]), paste("Data/AQ2/ReadyToProcess/", "Corrected_", fls[2], sep =""), row.names = TRUE)



####################
# Process AQ2 date #
####################

fils <- dir(path = "Data/AQ2/ReadyToProcess/", pattern = ".csv$")

write.csv(cmbn.fls(fils), "Data/AQ2/processed.dat.csv", row.names = FALSE) 