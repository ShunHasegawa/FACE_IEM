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

Iml_pre_nh <- lmer(log(nh + 10) ~ co2 * time+ (1|block) + (1|ring) + (1|id), data = subsetD(nhRmOl, pre))

# The starting model is:
Iml_pre_nh@call
Anova(Iml_pre_nh)

# model simplification
Fml_pre_nh <- stepLmer(Iml_pre_nh)

# The final model is:
Fml_pre_nh@call
Anova(Fml_pre_nh)
AnvF_pre_nh <- Anova(Fml_pre_nh, test.statistic = "F")
AnvF_pre_nh


summary(Fml_pre_nh)

plot(allEffects(Fml_pre_nh))

# model diagnosis
plot(Fml_pre_nh)
  # as expected,, not great...
qqnorm(resid(Fml_pre_nh))
qqline(resid(Fml_pre_nh))

## ---- Stat_FACE_IEM_Ammonium_postCO2

############
# Post-CO2 #
############

bxplts(value= "nh", data= postDF)
  # log seems better

# The initial model is:
Iml_post_nh <- lmer(log(nh) ~ co2 * time + (1|block) + (1|ring) + (1|id), data = postDF)
Anova(Iml_post_nh)

# The final model is:
Fml_post_nh <- stepLmer(Iml_post_nh, alpha.fixed = 0.1)

Fml_post_nh@call

Anova(Fml_post_nh)
AnvF_post_nh <- Anova(Fml_post_nh, test.statistic = "F")
AnvF_post_nh

# confidence interval for estimated parameters
CIdf_post_nh <- CIdf(model = Fml_post_nh)

summary(Fml_post_nh)

plot(allEffects(Fml_post_nh))

# model diagnosis
plot(Fml_post_nh)
qqnorm(resid(Fml_post_nh))
qqline(resid(Fml_post_nh))

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
Iml_ancv_nh <- lmer(log(nh) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
Anova(Iml_ancv_nh)
# model simplification

# Fml_ancv_nh <- stepLmer(Iml_ancv_nh)
#   error message saying no random effects,,,?

summary(Iml_ancv_nh)
  # Yes no random effect... May I remove them..

# remove nonsignificant factos by hands
Iml_ancv_nh2 <- lmer(log(nh) ~ co2 * Moist + Temp_Mean + 
                    (1|block) + (1|ring) + (1|id), data = postDF)
anova(Iml_ancv_nh, Iml_ancv_nh2)
  #Iml_ancv_nh2's better
Anova(Iml_ancv_nh2)
AnvF_nh <- Anova(Iml_ancv_nh2, test.statistic = "F")
Fml_ancv_nh <- Iml_ancv_nh2

# main effects
plot(allEffects(Fml_ancv_nh))

# model diagnosis
plot(Fml_ancv_nh)
qqnorm(resid(Fml_ancv_nh))
qqline(resid(Fml_ancv_nh))

# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_nh)

# calculate actual values
Est.val_nh <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3]
)

# reshape Est.val_nh and make a table
Est_nh <- ANCV_Tbl(Est.val_nh)

########################
# Plot predicted value #
########################
PltPrdVal(model = Fml_ancv_nh, variable = "Moist", data = postDF)
PltPrdVal(model = Fml_ancv_nh, variable = "Temp_Mean", data = postDF)

## ---- Stat_FACE_IEM_Ammonium_preCO2_Smmry

# The starting model is:
Iml_pre_nh@call
Anova(Iml_pre_nh)

# The final model is:
Fml_pre_nh@call

# Chi-square test
Anova(Fml_pre_nh)

# F test
AnvF_pre_nh

## ---- Stat_FACE_IEM_Ammonium_postCO2_Smmry

# The starting model is:
Iml_post_nh@call
Anova(Iml_post_nh)

# no interaction but may be co2 effect so remove the data becore co2-swtich on
Iml_post_nh@call
Anova(Iml_post_nh)

# The final model is:
Fml_post_nh@call

# Chi-square test
Anova(Fml_post_nh)

# F test
AnvF_post_nh

# 95 % CI (only difference from the base is shown)
CIdf_post_nh

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv_nh@call
# use @ instead $ for lmer model
Anova(Iml_ancv_nh)

# The final model
Fml_ancv_nh@call

# Chi-square
Anova(Fml_ancv_nh)

# F test
AnvF_nh

# squared R
rsquared.glmm(Fml_ancv_nh)

# 95 % CI for estimates
Est.val_nh

# plot the predicted values
PltPrdVal(model = Fml_ancv_nh, variable = "Moist", data = postDF)
PltPrdVal(model = Fml_ancv_nh, variable = "Temp_Mean", data = postDF)
