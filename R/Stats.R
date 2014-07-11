# Data frame for ancova
postDF <- subsetD(iem, !pre)
save(postDF, filen = "output//data/postDF.RData")

###########
# Nitrate #
###########
source("R/Stats_NO.R")

#############
# Ammonium  #
#############
source("R/Stats_NH.R")

#############
# Phosphate #
#############
source("R/Stats_P.R")


