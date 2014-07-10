## ---- Stat_FACE_IEM_Ammonium_preCO2

range(iem$nh)

###########
# Pre-CO2 #
###########

bxplts(value= "nh", data= subsetD(iem, pre))
 # remove two high values(outlier?)

nhRmOl <- subset(iem, nh < 800)
bxplts(value= "nh", data= subsetD(nhRmOl, pre))
bxplts(value= "nh", ofst= 10, data= subsetD(nhRmOl, pre))
  # none of the transformation seems to be working well, but let's carry on with
  # log anyway

# different random factor strucures
m1 <- lme(log(nh + 10) ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(nhRmOl, pre))
RndmComp(m1)$anova
  # m3 is better
RnMl <- RndmComp(m1)[[3]]

# autocorelation
atcr.cmpr(RnMl)$models
# model 5 looks better
Iml_pre <- atcr.cmpr(RnMl)[[5]]

# The starting model is:
Iml_pre$call

# model simplification
Anova(Iml_pre)

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
  # as expected,, not great...

## ---- Stat_FACE_IEM_Ammonium_postCO2

############
# Post-CO2 #
############

bxplts(value= "nh", data= subsetD(iem, post))
  # log seems better

# different random factor strucures
m1 <- lme(log(nh) ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(iem, post) )
RndmComp(m1)$anova
# m3 is better
RnMl <- RndmComp(m1)[[3]]

# autocorelation
atcr.cmpr(RnMl)$models
# model 5 looks better
Iml_post <- atcr.cmpr(RnMl)[[5]]

# The starting model is:
Iml_post$call

# model simplification
Anova(Iml_post)

Fml_post <- MdlSmpl(Iml_post)$model.reml

# The final model is:
Fml_post$call

Anova(Fml_post)

summary(Fml_post)

plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar
##########
# ANCOVA #
##########

######################
# Plot all variables #
######################
# plot for each plot against soil variables
# plot all variables
scatterplotMatrix(~ I(log(nh)) + Moist + Temp_Max + Temp_Min + Temp_Mean + MxT, 
                  data = postDF, diag = "boxplot")
print(xyplot(log(nh) ~ Moist | ring + plot, postDF, type = c("r", "p")))
print(xyplot(log(nh) ~ Temp_Mean | ring + plot, postDF, type = c("r", "p")))
print(xyplot(log(nh) ~ MxT | ring + plot, postDF, type = c("r", "p")))

############
# Analysis #
############
Iml_ancv <- lmer(log(nh) ~ co2 * (Moist + Temp_Mean + MxT) +
                   (1|block/ring/plot), data = postDF)
Anova(Iml_ancv)
# probably no co2 interaction, so remove
Iml_ancv2 <- lmer(log(nh) ~ co2 + Moist + Temp_Mean + MxT +
                    (1|block/ring/plot), data = postDF)
anova(Iml_ancv, Iml_ancv2)
  # Iml_ancv2's better
Anova(Iml_ancv2)
Anova(Iml_ancv2, test.statistic = "F")
  # all terms are highly significant!!
Fml_ancv <- Iml_ancv2

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv)

# calculate actual values
Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  MoistxTemp = ciDF[5, ]
)

Est.val

########################
# Plot predicted value #
########################
PltPrdVal(model = Fml_ancv, variable = "Moist", data = postDF)
PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", data = postDF)
PltPrdVal(model = Fml_ancv, variable = "MxT", data = postDF)

## ---- Stat_FACE_IEM_Ammonium_preCO2_Smmry

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ---- Stat_FACE_IEM_Ammonium_postCO2_Smmry

# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv@call
# use @ instead $ for lmer model
Anova(Iml_ancv)

# The final model
Fml_ancv@call
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")

# 95 % CI for estimates
Est.val

# plot the predicted values
PltPrdVal(model = Fml_ancv, variable = "Moist", data = postDF)
PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", data = postDF)
PltPrdVal(model = Fml_ancv, variable = "MxT", data = postDF)
