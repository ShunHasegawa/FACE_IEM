#########################
# Data frame for ancova #
#########################
postDF <- subsetD(iem, !pre)

# splite moist and temp into 3 levels
postDF <- within(postDF, {
  MoistLev <- cut(Moist, breaks = 3, labels = c("Dry", "Moderately wet", "Wet"))
  TempLev <- cut(Temp_Mean, breaks = 3, labels = c("Cold", "Moderately warm", "Hot"))
})

save(postDF, file = "output//data/postDF.RData")
load("output//data/postDF.RData")

##################################
# Moist & Temperature data frame #
##################################

# This is ruquired to plot predicated values from ANCOVA

# Split moisture and temperature into four then get the middle values and create
# three levels
# MT_Split <- llply(list(postDF$Moist, postDF$Temp_Mean),
#                   function(x) seq(min(x), max(x), length.out = 4))
# 
# MT_Lev <- llply(MT_Split, 
#                 function(x) round(x[-4] + (x[2] - x[1])/2, 2))
# 
# # Create vector that contins continuous values from min to max
# MT_val <- llply(list(postDF$Moist, postDF$Temp_Mean),
#                 function(x) seq(min(x), max(x), length.out = 150))
# 
# # using the above, create data frame with three levels of moisture (or temp) and
# # continuous temp (or moist)
# MTdf_temp <- expand.grid(MoistVal = MT_Lev[[1]], TempVal = MT_val[[2]])
# MTdf_moist <- expand.grid(MoistVal = MT_val[[1]], TempVal = MT_Lev[[2]])
# save(MTdf_temp, file = "output//data/MoistLev_TempCont_DF")
# save(MTdf_moist, file = "output//data/MoistCont_TempLev_DF")
load("output//data/MoistLev_TempCont_DF") # containes 3 levels of Moist & continuous Temp
load("output//data/MoistCont_TempLev_DF") # containes continuous moist  & 3 levels of Temp

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

#######################
# Summary Stats table #
#######################
# create summary list
StatSmmryLst <- list("Nitrate" = list(AnvF_no, Est_no),
                     "Ammonium" = list(AnvF_nh, Est_nh),
                     "Phosphate" = list(AnvF_P, Est_P))


# save in a single excel file
wb <- createWorkbook()
l_ply(c("Nitrate", "Ammonium", "Phosphate"), 
      function(x) CrSheetAnvTbl(workbook = wb, 
                                sheetName = x, 
                                smmaryLst = StatSmmryLst))
saveWorkbook(wb, "Output/Table/FACE_IEM_Ancv.xlsx")
