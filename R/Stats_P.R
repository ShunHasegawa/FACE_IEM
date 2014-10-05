## ---- Stat_FACE_IEM_Phosphate_preCO2

###########
# Pre-CO2 #
###########

range(iem$p)
bxplts(value= "p", data= subsetD(iem, pre))
# remove the higher outlier

PRmOl <- subset(iem, p < max(p))
bxplts(value= "p", data= subsetD(PRmOl, pre))
# log transformation seems slightly better

# different random factor strucures
m1 <- lme(log(p) ~ co2 * time, random = ~1|block/ring/plot,  data = subsetD(PRmOl, pre))
RndmComp(m1)$anova
m2 <- RndmComp(m1)[[2]]
# m2 is slightly better

# autocorelation
atcr.cmpr(m2)$models
# model 4 looks better

Iml_pre <- atcr.cmpr(m2)[[4]]

# The starting model is:
Iml_pre$call

# model simplification
Anova(Iml_pre)

MdlSmpl(Iml_pre)
# time * co2 and co2 are removed

Fml_pre <- MdlSmpl(Iml_pre)$model.reml

# The final model is:
Fml_pre$call

Anova(Fml_pre)

summary(Fml_pre)

plot(allEffects(Fml_pre))

# model diagnosis
plot(Fml_pre)
qqnorm(Fml_pre, ~ resid(.)|ring)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

## ---- Stat_FACE_IEM_Phosphate_postCO2
############
# post-co2 #
############
bxplts(value= "p", data= subsetD(iem, post))
bxcxplts(value= "p", data= subsetD(iem, post), sval = 0.9, fval = 2)
# adding constant value of 1.56 may improve

bxplts(value= "p", ofst = 1.6, data= subsetD(iem, post))


# The initial model is

# box-cox lambda
Iml_post <- lmer((p + 1.6)^(-1.1515) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
           data = subsetD(iem, post))

# log transformation
Iml_post <- lmer(log(p) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
           data = subsetD(iem, post))
Anova(Iml_post)
# not much difference between the above two tranformations. so just use log for
# simplification purposes.

# The final model is:
Fml_post <- Iml_post
Fml_post@call

Anova(Fml_post)
AnvF_post_p <- Anova(Fml_post, test.statistic = "F")
AnvF_post_p

summary(Fml_post)

plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(resid(Fml_post))
qqline(resid(Fml_post))

# contrast

# contrast doesn't work with lmer. so use lme
lmeMod <- lme(log(p) ~ co2 * time, random = ~1|block/ring/id, 
              data = subsetD(iem, post))

cntrst<- contrast(lmeMod, 
                  a = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "elev"))
FACE_IEM_PostCO2_P_CntrstDf <- cntrstTbl(cntrst, data = iem[iem$post, ], digit = 2)
FACE_IEM_PostCO2_P_CntrstDf

## ---- Stat_FACE_IEM_Phosphate_postCO2_withSoilVar

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################

# plot all variables
scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")
scatterplotMatrix(~ log(p) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")
scatterplotMatrix(~ log(p) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")

scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")

# plot for each plot against soil variables
print(xyplot((p + 1.6)^(-1.1515) ~ log(Moist) | ring + plot, postDF, type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, postDF, type = c("r", "p")))

############
# Analysis #
############

# use lmer to get confidence intervals for predicted values
Iml_ancv <- lmer(log(p) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
  # random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv)

# model simplification
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
AnvF_P <- Anova(Fml_ancv, test.statistic = "F")
AnvF_P

# what if I allow this to reduce random factors
Anova(ml <- stepLmer(Iml_ancv, red.rndm = TRUE))
ml@call 
  # ring is removed. main effects are more significant than
  # before but there're interactions so it doesn't really
  # matter too much

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

##########################
## plot predicted value ##
##########################
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("IEM-P (Temp = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("IEM-P (Moist = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})


######################
# Plot all variables #
######################

## Moist & Temperature data frame ##

# Split moisture and temperature into four then get the middle values and create
# three levels
MT_Split <- llply(list(postDF$Moist, postDF$Temp_Mean),
                  function(x) seq(min(x), max(x), length.out = 4))

MT_Lev <- llply(MT_Split, 
                function(x) round(x[-4] + (x[2] - x[1])/2, 2))

# Create vecotr that contins continuous values from min to max
MT_val <- llply(list(postDF$Moist, postDF$Temp_Mean),
                     function(x) seq(min(x), max(x), length.out = 150))

# using the above, create data frame with three levels of moisture (or temp) and
# continuous temp (or moist)
MTdf_temp <- expand.grid(MoistVal = MT_Lev[[1]], TempVal = MT_val[[2]])
MTdf_moist <- expand.grid(MoistVal = MT_val[[1]], TempVal = MT_Lev[[2]])

#############################################
# compute predicted values and CI intervals #
#############################################a

# bootstrap takes quite long time so apply parallel processing

## set up parallel backend

# registerDoParallel() is normally use to set it up but this time I need to
# export "BtsCI" and "Fml_ancv" so use the following

cl <- makeCluster(3, type = "SOCK") 
# 3 cores will be used. not sure the difference from 2
clusterExport(cl, c("BtsCI", "Fml_ancv"))
# exporting objects in the global environment
registerDoSNOW(cl)
getDoParWorkers()

system.time(
  Lst_CI_new <- llply(list(MTdf_temp, MTdf_moist), 
                function(x) ddply(x, .(MoistVal, TempVal), 
                                  function(y) BtsCI(model = Fml_ancv, 
                                                    MoistVal = y$MoistVal, 
                                                    TempVal = y$TempVal),
                                  .parallel = TRUE,
                                  .paropts = list(.export = c("BtsCI", "Fml_ancv"),
                                                  .packages = "lme4")),
                .parallel = TRUE
                )
  )

# I compared the followings
  # 1. parallell = TRUE for ddply only
  # 2. parallell = TRUE for both of ddply and llply
  # 3. parallell = TRUE for llply only
  # Result: 1 looked slower than the other two. there was no clear difference 
  # between 2 and 3. but bootstrap is randomising data so the length is always
  # different. so not 100 % sure
 
# load("Data//Lst_CI_moist.RData")
# load("Data//Lst_CI_Temp.RData")
# Lst_CI_new <- list(Lst_CI_temp, Lst_CI_moist)
  # I didn't have time so I run the above codes on Domino and downloaded.

stopCluster(cl) # clear the above setting of parallel backend
getDoParWorkers()


lapply(Lst_CI_new, function(x) unique(x$Moist))
lapply(Lst_CI_new, function(x) unique(x$Temp_Mean))
lapply(Lst_CI_new, head, n = 20)

# re-format the data frame for plotting
Lst_CI_new[[1]] <- within(Lst_CI_new[[1]], {
  MoistVal = factor(MoistVal, levels = rev(unique(MoistVal)),
                    labels = c("Wet", "Moderately wet", "Dry"))
})
Lst_CI_new[[2]] <- within(Lst_CI_new[[2]], {
  TempVal = factor(TempVal, levels = rev(unique(TempVal)),
                    labels = c("Wet", "Moderately wet", "Dry"))
})

save(Lst_CIvsTemp, file = "output//data/FACE_IEM_PvsTemp_LstCI.RData")


load("output//data/FACE_IEM_PvsTemp_LstCI.RData")

#############################
# conditioning scatter plot #
#############################
# (P ~ temp at given moisture ragnes)

# need to create multiple graphs on one graphic area with ggplot2

# scatterplot of x and y variables (P against temp at given moisture)
MLev <- cut(postDF$Moist, breaks = M, include.lowest = TRUE)
postDF$MoistVal <- factor(MLev, labels = c("Dry", "Moderately wet", "Wet"))

scatter <- ggplot(Lst_CIvsTemp, aes(x = Temp_Mean, y = PredVal, col = co2, fill = co2, group = co2)) +
  geom_line() +
  facet_grid(MoistVal ~ .) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .2, color = NA) +
  # color = NA removes the ribbon edge
  geom_point(data = postDF, aes(x = Temp_Mean, y = log(p)), alpha = .6) +
  scale_color_manual(values = c("blue", "red"), 
                     labels =c("Ambient", expression(eCO[2]))) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.12, .96), 
        legend.title = element_blank(),
        legend.key.size = unit(.2, "inch"),
        legend.background = element_rect(fill = alpha('white', 0)),
        axis.title = element_text(face = "plain")) +
  labs(x = expression(Soil~temperature~(degree * C)), 
       y = expression(log(IEM*-adsorbed~PO[4]^"3-")))

# moisture range
MoistDF <- data.frame(x = c(1:3), ymin = M[1:3] * 100, ymax = M[2:4] * 100)

theme_set(theme_bw()) # set plot back ground as white
MoistPlt <- ggplot(MoistDF, 
                   aes(xmin = x - 0.3, xmax = x + 0.3, ymin = ymin, ymax = ymax)) +
  geom_rect(fill = "gray30") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "", y = "Given soil moisture (%)")

# merge the plots
pl <- arrangeGrob(scatter, MoistPlt, ncol = 2, nrow = 1, 
                  widths = unit(c(5, 1.5), "inches"))
# grid.arrange creates graphs directly on the device, while arrangeGrob makes 
# ggplot object which can be save using ggsave. but text font looks bold for
# some reasons..

ggsavePP(plot = pl, filename = "output//figs/FACE_manuscript/FACE_Pred_IEM_P_Temp", width = 6.5, height = 6)

################################################
# confidence interval for estimated parameters #
################################################
ciDF <- CIdf(model = Fml_ancv)

# calculate actual values
Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3],
  co2elev.Temp_Mean = ciDF [6, ] + ciDF[4, 3]
  )

Est.val

# reshape Est.val and make a table
Est_P <- ANCV_Tbl(Est.val)

## ---- Stat_FACE_IEM_Phosphate_preCO2_Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ---- Stat_FACE_IEM_Phosphate_postCO2_Smmry
# The starting model is:
Iml_post@call
Anova(Iml_post)

# The final model is:
Fml_post@call

# Chi-square
Anova(Fml_post)

# F-stest
AnvF_post_p

# contrast
FACE_IEM_PostCO2_P_CntrstDf

## ---- Stat_FACE_IEM_Phosphate_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv@call
Anova(Iml_ancv)

# The final model
Fml_ancv@call

# Chisq
Anova(Fml_ancv)

# F-test
AnvF_P

# 95% CI for estimated parameter
Est.val

# plot the predicted values
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("IEM-P (Temp = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("IEM-P (Moist = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})