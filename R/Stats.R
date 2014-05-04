# pre or post co2
# time 4 is pre-co2 but need to be in post as well 
# as it's used as a baseline
iem$pre <- ifelse(iem$time %in% c(1:4), TRUE, FALSE )
iem$post <- ifelse(!(iem$time %in% c(1:3)), TRUE, FALSE )

#############
# Phosphate #
#############
source("R/Stats_P.R")
