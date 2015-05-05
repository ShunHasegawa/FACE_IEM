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

Iml_pre_p <- lmer(log(p) ~ co2 * time + (1|block) + (1|ring) + (1|id),  data = subsetD(PRmOl, pre))

# The starting model is:
Iml_pre_p@call
Anova(Iml_pre_p)

# model simplification
Fml_pre_p <- stepLmer(Iml_pre_p)

# The final model is:
Fml_pre_p@call

Anova(Fml_pre_p)
AnvF_pre_p <- Anova(Fml_pre_p, test.statistic = "F")
AnvF_pre_p

summary(Fml_pre_p)

plot(allEffects(Fml_pre_p))

# model diagnosis
plot(Fml_pre_p)
qqnorm(resid(Fml_pre_p))
qqline(resid(Fml_pre_p))

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
Iml_post_p <- lmer((p + 1.6)^(-1.1515) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
           data = subsetD(iem, post))

# log transformation
Iml_post_p <- lmer(log(p) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
           data = subsetD(iem, post))
Anova(Iml_post_p)
# not much difference between the above two tranformations. so just use log for
# simplification purposes.

# The final model is:
Fml_post <- Iml_post_p
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
FACE_IEM_PostCO2_P_CntrstDf <- cntrstTbl(cntrst, data = iem[iem$post, ], variable  = "p", digit = 2)
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
Iml_ancv_p <- lmer(log(p) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
  # random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv_p)

# model simplification
Fml_ancv_p <- stepLmer(Iml_ancv_p)
Anova(Fml_ancv_p)
AnvF_P <- Anova(Fml_ancv_p, test.statistic = "F")
AnvF_P

# what if I allow this to reduce random factors
Anova(ml <- stepLmer(Iml_ancv_p, red.rndm = TRUE))
ml@call 
  # ring is removed. main effects are more significant than
  # before but there're interactions so it doesn't really
  # matter too much

# main effects
plot(allEffects(Fml_ancv_p))

# model diagnosis
plot(Fml_ancv_p)
qqnorm(resid(Fml_ancv_p))
qqline(resid(Fml_ancv_p))

##########################
## plot predicted value ##
##########################
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv_p, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("IEM-P (Temp = ", x, ")", sep = ""),
#             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv_p, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("IEM-P (Moist = ", x, ")", sep = ""),
#             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# For some reasons, ylim doesn't work...

################################################
# confidence interval for estimated parameters #
################################################
ciDF <- CIdf(model = Fml_ancv_p)

# calculate actual values
Est.val_p <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3],
  co2elev.Temp_Mean = ciDF [6, ] + ciDF[4, 3]
  )

Est.val_p

# reshape Est.val and make a table
Est_P <- ANCV_Tbl(Est.val_p)

## ---- Stat_FACE_IEM_Phosphate_preCO2_Smmry
# The starting model is:
Iml_pre_p@call
Anova(Iml_pre_p)

# The final model is:
Fml_pre_p@call
Anova(Fml_pre_p)

## ---- Stat_FACE_IEM_Phosphate_postCO2_Smmry
# The starting model is:
Iml_post_p@call
Anova(Iml_post_p)

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
Iml_ancv_p@call
Anova(Iml_ancv_p)

# The final model
Fml_ancv_p@call

# Chisq
Anova(Fml_ancv_p)

# F-test
AnvF_P

# squared R
rsquared.glmm(Fml_ancv_p)

# 95% CI for estimated parameter
Est.val_p

# plot the predicted values
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv_p, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("IEM-P (Temp = ", x, ")", sep = ""),
#             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv_p, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("IEM-P (Moist = ", x, ")", sep = ""),
#             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})