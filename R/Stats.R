#############################################
# Preparation for ANCOVA with post-co2 data #
#############################################

##########################
## Add interaction term ##
##########################

postDF <- subsetD(iem, !pre)

# creat interactive term: Moist x Temp_Mean
postDF$MxT <- postDF$Moist * postDF$Temp_Mean

# plot all variables
scatterplotMatrix(~ Moist + Temp_Max + Temp_Min + Temp_Mean + MxT, 
                  data = postDF, diag = "boxplot")
# MxT seems highly corelated to Moist

###########################
# check multi-colinearity #
###########################
cor(postDF[, c("Moist", "Temp_Mean", "MxT")])
vif(lm(no ~ Moist + Temp_Mean + MxT, data = postDF)) # response var can be anything
1/vif(lm(no ~ Moist + Temp_Mean + MxT, data = postDF))
# vif<.2 nad 1/vif>>5 so collinarity is problem

# to avoid multicoliniarity, use conduct first centering
postDF$MxT <- with(postDF, (Moist - mean(Moist)) * (Temp_Mean - mean(Temp_Mean)))
scatterplotMatrix(~ Moist + Temp_Max + Temp_Min + Temp_Mean + MxT, 
                  data = postDF, diag = "boxplot")
cor(postDF[, c("Moist", "Temp_Mean", "MxT")])
vif(lm(nh ~ Moist + Temp_Mean + MxT, data = postDF))
1/vif(lm(nh ~ Moist + Temp_Mean + MxT, data = postDF))
# it looks fine now

# save
save(postDF, file = "output//data/postDF.RData")


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


