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

# The initial model is:
Iml_post <- lmer(log(nh) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
                 data = subsetD(iem, post))
Anova(Iml_post)

# There isn't co2xtime interaction but may be co2 effect. re analysis with data
# only after co2-switced-on.

Iml_post_2 <- lmer(log(nh) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
                   data = postDF)
Anova(Iml_post_2)
Anova(Iml_post_2, test.statistic = "F")

# The final model is:
Fml_post <- stepLmer(Iml_post_2)
Fml_post@call
# CO2 is removed but it was marginally significant so keep

Fml_post <- lmer(log(nh) ~ co2 + time + (1|block) + (1|ring) + (1|id), 
                 data = postDF)
Anova(Fml_post)

AnvF_post_nh <- Anova(Fml_post, test.statistic = "F")
AnvF_post_nh

# confidence interval for estimated parameters
CIdf_post <- CIdf(model = Fml_post)

summary(Fml_post)

plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(resid(Fml_post))
qqline(resid(Fml_post))

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar
##########
# ANCOVA #
##########

######################
# Plot all variables #
######################
# plot for each plot against soil variables
# plot all variables
scatterplotMatrix(~ I(log(nh)) + Moist + Temp_Max + Temp_Min + Temp_Mean, 
                  data = postDF, diag = "boxplot")
print(xyplot(log(nh) ~ Moist | ring + plot, postDF, type = c("r", "p")))
print(xyplot(log(nh) ~ Temp_Mean | ring + plot, postDF, type = c("r", "p")))

############
# Analysis #
############
Iml_ancv <- lmer(log(nh) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
Anova(Iml_ancv)
# model simplification


# Fml_ancv <- stepLmer(Iml_ancv)
#   error message saying no random effects,,,?

summary(Iml_ancv)
  # Yes no random effect... May I remove them..

# remove nonsignificant factos by hands
Iml_ancv2 <- lmer(log(nh) ~ co2 * Moist + Temp_Mean + 
                    (1|block) + (1|ring) + (1|id), data = postDF)
anova(Iml_ancv, Iml_ancv2)
  #Iml_ancv2's better
Anova(Iml_ancv2)
AnvF_nh <- Anova(Iml_ancv2, test.statistic = "F")
Fml_ancv <- Iml_ancv2
Fml_ancv_NH <- Fml_ancv

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
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3]
)

# reshape Est.val and make a table
Est_nh <- ANCV_Tbl(Est.val)

########################
# Plot predicted value #
########################
# PltPrdVal(model = Fml_ancv, variable = "Moist", data = postDF)
# PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", data = postDF)

## ---- Stat_FACE_IEM_Ammonium_preCO2_Smmry

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ---- Stat_FACE_IEM_Ammonium_postCO2_Smmry

# The starting model is:
Iml_post@call
Anova(Iml_post)

# no interaction but may be co2 effect so remove the data becore co2-swtich on
Iml_post_2@call
Anova(Iml_post_2)

# The final model is:
Fml_post@call

# Chi-square test
Anova(Fml_post)

# F test
AnvF_post_nh

# 95 % CI (only difference from the base is shown)
CIdf_post

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv@call
# use @ instead $ for lmer model
Anova(Iml_ancv)

# The final model
Fml_ancv@call

# Chi-square
Anova(Fml_ancv)

# F test
AnvF_nh

# squared R
rsquared.glmm(Fml_ancv)

# 95 % CI for estimates
Est.val

# plot the predicted values
# PltPrdVal(model = Fml_ancv, variable = "Moist", data = postDF)
# PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", data = postDF)
