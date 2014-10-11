## ---- Stat_FACE_IEM_Nitrate_preCO2

range(iem$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subsetD(iem, pre))
# sqrt seems slightly better

# different random factor strucures
m1 <- lme(sqrt(no) ~ co2 * time, random = ~1|block/ring/plot,  data = subsetD(iem, pre))
RndmComp(m1)$anova
# m3 is better
RnMl <- RndmComp(m1)[[3]]

# autocorelation
atcr.cmpr(RnMl)$models
# model 3 looks better
Iml_pre <- atcr.cmpr(RnMl)[[3]]

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
qqnorm(Fml_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

## ---- Stat_FACE_IEM_Nitrate_postCO2

############
# Post-CO2 #
############

bxplts(value= "no", data= subsetD(iem, post))
# log seems better, but remove one outlier
NoRmOl <- subsetD(iem, post)
NoRmOl <- subsetD(NoRmOl, no != min(no, na.rm = TRUE))
bxplts(value= "no", data= NoRmOl)

# The initial model is:
Iml_post <- lmer(log(no) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
                 data = NoRmOl)
Anova(Iml_post)
# keep interaction

# The final model is:
Fml_post <- Iml_post
Fml_post@call

Anova(Fml_post)
AnvF_post_no <- Anova(Fml_post, test.statistic = "F")
AnvF_post_no

summary(Fml_post)

plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(resid(Fml_post))
qqline(resid(Fml_post))

# contrast

# contrast doesn't work with lmer. so use lme
lmeMod <- lme(log(no) ~ co2 * time, random = ~1|block/ring/id, data = NoRmOl)

cntrst<- contrast(lmeMod, 
                  a = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "elev"))
FACE_IEM_PostCO2_NO_CntrstDf <- cntrstTbl(cntrst, data = iem[iem$post, ], digit = 2)
FACE_IEM_PostCO2_NO_CntrstDf

## ---- Stat_FACE_IEM_Nitrate_postCO2_withSoilVar

##########
# ANCOVA #
##########
# plot all variables
scatterplotMatrix(~ log(no) + Moist + Temp_Max + Temp_Min + Temp_Mean, 
                  data = postDF, diag = "boxplot")
scatterplotMatrix(~ log(no) + Moist + Temp_Max + Temp_Min + Temp_Mean, 
                  data = postDF, diag = "boxplot")

# plot for each plot against soil variables
print(xyplot(log(no) ~ Moist | ring + plot, postDF, type = c("r", "p")))
print(xyplot(log(no) ~ Temp_Mean | ring + plot, postDF, type = c("r", "p")))

## Analysis ##
Iml_ancv <- lmer(log(no) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id),  data = postDF)
# model simplification
Fml_ancv <- stepLmer(Iml_ancv)
AnvF_no <- Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
# looking at qqplot, there's one outlier so remove

# identifi the outlier and remove
qqval <- qqnorm(resid(Fml_ancv))[[2]]
min(qqval)
postDF[which(qqval == min(qqval)), "no"] <- NA

# rerun analysis
Iml_ancv <- lmer(log(no) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id),  
                 data = postDF, na.action = "na.omit")
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
# co2 is not significant so removed, but I would like to plot predicted vales
# for each treatment anyway so keep co2 factor in the model.
Fml_ancv <- update(Fml_ancv,~. + co2)
Anova(Fml_ancv)

AnvF_no <- Anova(Fml_ancv, test.statistic = "F")
AnvF_no
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
# looks prety good

# main effect
plot(allEffects(Fml_ancv))

# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv)

# calculate actual values
Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[4, ] + ciDF[1, 3],
  Moist = ciDF[2, ],
  Temp_Mean = ciDF[3, ]
  )

# reshape Est.val and make a table
Est_no <- ANCV_Tbl(Est.val)


#---------------------
#############################################
# compute predicted values and CI intervals #
#############################################a

# bootstrap takes quite long time so apply parallel processing

## set up parallel backend

# registerDoParallel() is normally use to set it up but this time I need to
# export "BtsCI" and "Fml_ancv" so use the following

cl <- makeCluster(2, type = "SOCK") 
# two cores will be used as two data frames will be processed simultaneously as
# below. but not sure if it makes difference when I use othere numbers

clusterExport(cl, c("BtsCI", "Fml_ancv"))
# exporting objects in the global environment

registerDoSNOW(cl)
getDoParWorkers()

system.time(
  Lst_CI_no <- llply(list(MTdf_temp, MTdf_moist), 
                      function(y) BtsCI(model = Fml_ancv, 
                                        MoistVal = y$MoistVal, 
                                        TempVal = y$TempVal),
                      .parallel = TRUE,
                      .paropts = list(.export = c("BtsCI", "Fml_ancv"),
                                      .packages = "lme4")
                      
  )
)

stopCluster(cl) # clear the above setting of parallel backend
getDoParWorkers()

# re-format the data frame for plotting
Lst_CI_no[[1]] <- within(Lst_CI_no[[1]], {
  MoistLev = factor(Moist, labels = c("Dry", "Moderately wet", "Wet"))
})
Lst_CI_no[[2]] <- within(Lst_CI_no[[2]], {
  TempLev = factor(Temp_Mean, labels = c("Cold", "Moderately warm", "Hot"))
})

save(Lst_CI_no, file  ="output//data/FACE_IEM_NO_PredVal.RData")

load("output//data/FACE_IEM_NO_PredVal.RData")

#############################
# conditioning scatter plot #
#############################
scatter <- ggplot(Lst_CI_no[[1]], 
                  aes(x = Temp_Mean, y = PredVal, col = co2, fill = co2, group = co2)) +
  geom_line() +
  facet_grid(. ~ MoistLev) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .2, color = NA) +
  # color = NA removes the ribbon edge
  geom_point(data = postDF, aes(x = Temp_Mean, y = log(no)), alpha = .6) +
  scale_color_manual(values = c("blue", "red"), 
                     labels =c("Ambient", expression(eCO[2]))) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_x_continuous(breaks = pretty(Lst_CI_no[[1]]$Temp_Mean)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.12, .96), 
        legend.title = element_blank(),
        legend.key.size = unit(.2, "inch"),
        legend.background = element_rect(fill = alpha('white', 0)),
        axis.title = element_text(face = "plain")) +
  labs(x = expression(Soil~temperature~(degree * C)), 
       y = expression(log(IEM*-adsorbed~NO[3]^"-")))

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

ggsavePP(plot = pl, filename = "output//figs/FACE_manuscript/FACE_Pred_IEM_P_Temp", width = 6.5, height = 6)





#---------------------













## ---- Stat_FACE_IEM_Nitrate_preCO2_Smmry

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ---- Stat_FACE_IEM_Nitrate_postCO2_Smmry

# The starting model is:
Iml_post@call
Anova(Iml_post)

# The final model is:
Fml_post@call

# Chi-square test
Anova(Fml_post)

# F-test
AnvF_post_no

# Contrast
FACE_IEM_PostCO2_NO_CntrstDf

## ---- Stat_FACE_IEM_Nitrate_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv@call
Anova(Iml_ancv)

# The final model
Fml_ancv@call

# Chi-square
Anova(Fml_ancv)

# F-test
AnvF_no

# 95% CI for estimated parameter
Est.val

# plot the predicted values
visreg(Fml_ancv, "Moist")
visreg(Fml_ancv, "Temp_Mean")
