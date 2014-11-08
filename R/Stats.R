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

########################
## CO2 x Moist x Temp ##
########################

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

################
## CO2 x Time ##
################

# create stat summary table for LMM with CO2 and time
CO2TimeStatList <- list('Nitrate' = AnvF_post_no, 
                        'Ammonium' = AnvF_post_nh, 
                        'Phosphate' = AnvF) 

Stat_CO2Time <- ldply(names(CO2TimeStatList), 
                      function(x) StatTable(CO2TimeStatList[[x]], variable = x))
save(Stat_CO2Time, file = "output//data/CO2Time_Stat.RData")






################################
# Plot predicted values and CI #
################################

# list of ancova models
Lst_ml <- list(no = Fml_ancv_NO, nh = Fml_ancv_NH, p = Fml_ancv_P) 

# bootstrap takes quite long time so apply parallel processing

## set up parallel backend

# registerDoParallel() is normally use to set it up but this time I need to
# export "BtsCI" and "Fml_ancv" so use the following

cl <- makeCluster(3, type = "SOCK") 
# three cores will be used as three models will be processed simultaneously as
# below. 

clusterExport(cl, c("MTdf_temp", "MTdf_moist", "BtsCI", "Lst_ml"))
# exporting objects in the global environment

registerDoSNOW(cl)
getDoParWorkers()

## against temperature ##
ciDF_vsTemp <- ldply(1:3, 
                    function(x) BtsCI(model = Lst_ml[[x]], 
                                      MoistVal = MTdf_temp$MoistVal, 
                                      TempVal = MTdf_temp$TempVal, 
                                      variable = names(Lst_ml[x])),
                    .parallel = TRUE,
                    .paropts = list(.export = c("MTdf_temp","BtsCI", "Lst_ml"),
                                    .packages = "lme4")
                    )

# add Moist level
MoistLabs <- c(expression(Dry),
               expression(Moderately~wet),
               expression(Wet))

# labels for facet_grid
ylabs <- c(expression(NO[3]^"-"),expression(NH[4]^"+"), expression(PO[4]^"3-"))

ciDF_vsTemp <- within(ciDF_vsTemp, {
  MoistLev <- factor(Moist, labels = MoistLabs)
  variable <- factor(variable, labels = ylabs)
})

## agianst moisture ##
ciDF_vsMoist <- ldply(1:3, 
                     function(x) BtsCI(model = Lst_ml[[x]], 
                                       MoistVal = MTdf_moist$MoistVal, 
                                       TempVal = MTdf_moist$TempVal, 
                                       variable = names(Lst_ml[x])),
                     .parallel = TRUE,
                     .paropts = list(.export = c("MTdf_moist","BtsCI", "Lst_ml"),
                                     .packages = "lme4")
)

# add Temp level
TempLabs <- c(expression(Cold),
               expression(Moderately~warm),
               expression(Hot))

ciDF_vsMoist <- within(ciDF_vsMoist, {
  TempLev <- factor(Temp_Mean, labels = TempLabs)
  variable <- factor(variable, labels = ylabs)
})

# save
save(ciDF_vsTemp, file = "output//data/ciDF_vsTemp.RData") 
save(ciDF_vsMoist, file = "output//data/ciDF_vsMoist.RData") 

# load
load("output//data/ciDF_vsTemp.RData")
load("output//data/ciDF_vsMoist.RData")

#############################
# conditioning scatter plot #
#############################
# need to create multiple graphs on one graphic area with ggplot2

## scatterplot of x and y variables (nutrient against temp at given moisture)

# melt postDF to plot them all together with facet_grid
postDF_Mlt <- melt(postDF, 
                   id = names(postDF)[which(!(names(postDF) %in% c("no", "nh", "p")))])

postDF_Mlt <- within(postDF_Mlt, {
  variable <- factor(variable, labels = ylabs)
  MoistLev <- factor(MoistLev, labels = MoistLabs)
  TempLev <- factor(TempLev, labels = TempLabs)
})

##############################################
## Scatter plot of predicated values and CI ##
##############################################

# Plot against moisture and temperature

MoistSct <- ScatterPlot(df = ciDF_vsMoist, xval = "Moist", breakn = 5, 
                        xlab = "Soil moisture (%)", gridval = "TempLev")


TempSct <-  ScatterPlot(df = ciDF_vsTemp, xval = "Temp_Mean", breakn = 5, 
                        xlab = expression(Soil~temperature~(degree * C)),
                        gridval = "MoistLev")

###################################################################
## Boxplot for environmental vairalbe (moisture and temperature) ##
###################################################################

MoistPlot <- envPlot(val = "Moist", ylab = "Given soil moisture (%)")
TempPlot <- envPlot(val = "Temp_Mean", ylab = expression(Given~soil~temperature~(degree * C)))


theme_set(theme_bw()) # set plot back ground as white


# merge the plots
Moist_pl <- arrangeGrob(TempPlot, MoistSct, ncol = 1, nrow = 2, 
                        heights = unit(c(1, 6), "inches"))

Temp_pl <- arrangeGrob(MoistPlot, TempSct, ncol = 1, nrow = 2, 
                        heights = unit(c(1, 6), "inches"))

  # grid.arrange creates graphs directly on the device, while arrangeGrob makes 
  # ggplot object which can be save using ggsave. but text font looks bold for
  # some reasons..

# save
ggsavePP(plot = Moist_pl, filename = "output//figs/FACE_manuscript/FACE_Pred_IEM_Moist", 
         width = 6, height = 7.5)

ggsavePP(plot = Temp_pl, filename = "output//figs/FACE_manuscript/FACE_Pred_IEM_Temp", 
         width = 6, height = 7.5)
