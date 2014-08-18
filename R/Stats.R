# Data frame for ancova
postDF <- subsetD(iem, !pre)
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
