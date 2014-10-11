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

clusterExport(cl, c("MTdf_temp","BtsCI", "Lst_ml"))
# exporting objects in the global environment

registerDoSNOW(cl)
getDoParWorkers()

system.time(
  ciDF_vsTemp <- ldply(1:3, 
                    function(x) BtsCI(model = Lst_ml[[x]], 
                                      MoistVal = MTdf_temp$MoistVal, 
                                      TempVal = MTdf_temp$TempVal, 
                                      variable = names(Lst_ml[x])),
                    .parallel = TRUE,
                    .paropts = list(.export = c("MTdf_temp","BtsCI", "Lst_ml"),
                                    .packages = "lme4")
                    )
)

# add Moist level
ciDF_vsTemp$MoistLev <- factor(ciDF_vsTemp$Moist, 
                               labels = c("Dry", "Moderately wet", "Wet")) 

# save
save(ciDF_vsTemp, file = "output//data/ciDF_vsTemp.RData") 

#############################
# conditioning scatter plot #
#############################
# need to create multiple graphs on one graphic area with ggplot2

## scatterplot of x and y variables (nutrient against temp at given moisture)

# melt postDF to plot them all together with facet_grid
postDF_Mlt <- melt(postDF, 
                   id = names(postDF)[which(!(names(postDF) %in% c("no", "nh", "p")))])

# labels for facet_grid
ylabs <- c(expression(NO[3]^"-"),expression(NH[4]^"+"), expression(PO[4]^"3-"))

levels(ciDF_vsTemp$variable) <- ylabs
levels(postDF_Mlt$variable) <- ylabs

MoistLabs <- c(expression(Dry),
               expression(Moderately~wet),
               expression(Wet))

levels(ciDF_vsTemp$MoistLev) <- MoistLabs
levels(postDF_Mlt$MoistLev) <- MoistLabs

scatter <- ggplot(ciDF_vsTemp, 
                  aes(x = Temp_Mean, y = PredVal, col = co2, fill = co2, group = co2)) +
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .4, color = NA) +
  # color = NA removes the ribbon edge
  geom_point(data = postDF_Mlt, aes(x = Temp_Mean, y = log(value)), 
             alpha = .6, size = 1) +
  scale_color_manual(values = c("blue", "red"), 
                     labels =c("Ambient", expression(eCO[2]))) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_x_continuous(breaks = pretty(ciDF_vsTemp$Temp_Mean)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.12, .96), 
        legend.title = element_blank(),
        legend.key.size = unit(.2, "inch"),
        legend.background = element_rect(fill = alpha('white', 0)),
        axis.title = element_text(face = "plain")) +
  facet_grid(variable ~ MoistLev, scales = "free_y", labeller = label_parsed) +
  labs(x = expression(Soil~temperature~(degree * C)), 
       y = expression(log(IEM*-adsorbed~nutrients~(ng~cm^"-2"~d^"-1"))))

# moisture range
M <- with(postDF, seq(min(Moist), max(Moist), length.out = 4))
MoistDF <- data.frame(x = c(1:3), ymin = M[1:3] * 100, ymax = M[2:4] * 100)

theme_set(theme_bw()) # set plot back ground as white

MoistPlt <- ggplot(MoistDF, 
                   aes(xmin = x - 0.3, xmax = x + 0.3, ymin = ymin, ymax = ymax)) +
  geom_rect(fill = "gray30") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  coord_flip() +
  labs(x = "", y = "Given soil moisture (%)")

# merge the plots
pl <- arrangeGrob(MoistPlt, scatter, ncol = 1, nrow = 2, 
                  heights = unit(c(1.5, 5), "inches"))
# grid.arrange creates graphs directly on the device, while arrangeGrob makes 
# ggplot object which can be save using ggsave. but text font looks bold for
# some reasons..

ggsavePP(plot = pl, filename = "output//figs/FACE_manuscript/FACE_Pred_IEM_Temp", 
         width = 6, height = 8)
