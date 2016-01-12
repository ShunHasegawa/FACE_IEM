#############
# N:P ratio #
#############

## ---- Stat_FACE_IEM_NPRatio_preCO2
###########
# Pre-CO2 #
###########

boxplot(logNP ~ co2 * time, data = subsetD(iem, pre))

Iml_pre_np <- lmer(logNP ~ co2 * time+ (1|block) + (1|ring) + (1|id), data = subsetD(iem, pre))

# The starting model is:
Iml_pre_np@call
Anova(Iml_pre_np)

# model simplification
Fml_pre_np <- stepLmer(Iml_pre_np, alpha.fixed = .1)

# The final model is:
Fml_pre_np@call
Anova(Fml_pre_np)
AnvF_pre_np <- Anova(Fml_pre_np, test.statistic = "F")
AnvF_pre_np

# model diagnosis
plot(Fml_pre_np)
qqnorm(resid(Fml_pre_np))
qqline(resid(Fml_pre_np))

# what if I remove the lower outlier
which(qqnorm(resid(Fml_pre_np))$y == min(qqnorm(resid(Fml_pre_np))$y))

ml <- update(Iml_pre_np, subset = -17)
Anova(ml, test.statistic = "F")
plot(ml)
qqnorm(resid(ml))
qqline(resid(ml))
# improved but pretty much same result so stay with the above

## ---- Stat_FACE_IEM_NPRatio_postCO2

############
# NP ratio #
############

boxplot(logNP ~ co2 * time, data = postDF)

# The initial model is
Iml_post_NP <- lmer(logNP ~ co2 * time + (1|block) + (1|ring)  + (1|id), data = postDF)
Anova(Iml_post_NP, test.statistic = "F")

# The final model is:
Fml_post_NP <- stepLmer(Iml_post_NP, alpha.fixed = .1)
AnvF_post_np <- Anova(Fml_post_NP, test.statistic = "F")
AnvF_post_np

plot(allEffects(Fml_post_NP))

# model diagnosis
plot(Fml_post_NP)
qqnorm(resid(Fml_post_NP))
qqline(resid(Fml_post_NP))

## ---- Stat_FACE_IEM_NPRatio_postCO2_withSoilVar

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################

# plot all variables
scatterplotMatrix(~ logNP + Moist + Temp_Mean|co2, data = postDF, diag = "boxplot")

# plot for each ring
xyplot(logNP ~ Moist|co2, groups = ring, postDF, type = c("r", "p"))
xyplot(logNP ~ Temp_Mean|co2, groups = ring, postDF, type = c("r", "p"))

############
# Analysis #
############

# use lmer to get confidence intervals for predicted values
Iml_ancv_np <- lmer(logNP ~ co2 * (Moist + Temp_Mean) + 
                      (1|block) + (1|ring) + (1|id), data = postDF)
# random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv_np, test.statistic = "F")

# model simplification
Fml_ancv_np <- stepLmer(Iml_ancv_np, alpha.fixed = 0.1)
AnvF_NP <- Anova(Fml_ancv_np, test.statistic = "F")
AnvF_NP

# main effects
plot(allEffects(Fml_ancv_np))

# model diagnosis
plot(Fml_ancv_np)
qqnorm(resid(Fml_ancv_np))
qqline(resid(Fml_ancv_np))

##########################
## plot predicted value ##
##########################
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv_np, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("NP (Temp = ", x, ")", sep = ""),
            #             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv_np, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("NP (Moist = ", x, ")", sep = ""),
            #             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

################################################
# confidence interval for estimated parameters #
################################################
ciDF <- CIdf(model = Fml_ancv_np)

# calculate actual values
Est.val_np <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3],
  co2elev.Temp_Mean = ciDF [6, ] + ciDF[4, 3]
)

Est.val_np

# reshape Est.val and make a table
Est_NP <- ANCV_Tbl(Est.val_np)

## ---- Stat_FACE_IEM_NPRatio_preCO2_Smmry
# The starting model is:
Iml_pre_np@call
Anova(Iml_pre_np)

# The final model is:
Fml_pre_np@call
Anova(Fml_pre_np)

## ---- Stat_FACE_IEM_NPRatio_postCO2_Smmry
# The starting model is:
Iml_post_NP@call
Anova(Iml_post_NP)

# The final model is:
Fml_post_NP@call

# Chi-square
Anova(Fml_post_NP)

# F-stest
AnvF_post_np

## ---- Stat_FACE_IEM_NPRatio_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv_np@call
Anova(Iml_ancv_np)

# The final model
Fml_ancv_np@call

# Chisq
Anova(Fml_ancv_np)

# F-test
AnvF_NP

# squared R
rsquared.glmm(Fml_ancv_np)

# 95% CI for estimated parameter
Est.val_np

# plot the predicted values
par(mfrow = c(1,2))
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv_np, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("NP (Temp = ", x, ")", sep = ""),
            #             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv_np, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("NP (Moist = ", x, ")", sep = ""),
            #             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})
