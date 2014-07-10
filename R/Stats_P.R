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
# use box-cox lambda

# different random factor strucures
m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * time, random = ~1|block/ring/plot,  data = subsetD(iem, post))
RmMd <- RndmComp(m1)
RmMd$anova
# m3 is better

# autocorelation
ArgMd <- atcr.cmpr(RmMd[[3]])
ArgMd$models
# model 4 looks better

Iml_post <- ArgMd[[4]]

# The starting model is:
Iml_post$call

# model simplification
Anova(Iml_post)

MdlSmpl(Iml_post)
# no factor is removed

Fml_post <- MdlSmpl(Iml_post)$model.reml

# The final model is:
Fml_post$call

Anova(Fml_post)

summary(Fml_post)

# contrast
cntrst<- contrast(Fml_post, 
                  a = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "elev"))
FACE_IEM_PostCO2_P_CntrstDf <- cntrstTbl(cntrst, data = iem[iem$post, ], digit = 2)
FACE_IEM_PostCO2_P_CntrstDf

# model diagnosis
plot(Fml_post)
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))

## ---- Stat_FACE_IEM_Phosphate_postCO2_withSoilVar

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################

# plot all variables
scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + Moist + Temp_Max + Temp_Min + Temp_Mean + MxT,
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
Iml_ancv <- lmer(log(p) ~ co2 * (Moist + Temp_Mean + MxT) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
  # random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv)

# model simplification
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")

# what if I allow this to reduce random factors
Anova(ml <- stepLmer(Iml_ancv, red.rndm = TRUE))
ml@call 
  # ring is removed. main effects are more significant than before but there're
  # interactions so it doesn't really matter too much

# main effects
plot(allEffects(Fml_ancv))

##########################
## plot predicted value ##
##########################
par(mfrow = c(2,2))
# moist
PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = 12),
          ylab = "IEM-P (Temp = 12)",
          ylim = c(0, 6),
          trans = exp,
          data = iem)
PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = 23), 
          ylab = "IEM-P (Temp = 23)",
          ylim = c(0, 6),
          trans = exp,
          data = iem)
# temp
PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = .05), 
          ylab = "IEM-P (Moist = .05)",
          ylim = c(0, 6),
          trans = exp,
          data = iem)
PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = .25), 
          ylab = "IEM-P (Moist = .25)",
          ylim = c(0, 6),
          trans = exp,
          data = iem)

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
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3],
  co2elev.Temp_Mean = ciDF [6, ] + ciDF[4, 3]
  )

Est.val

## ---- Stat_FACE_IEM_Phosphate_preCO2_Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ---- Stat_FACE_IEM_Phosphate_postCO2_Smmry
# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

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
Anova(Fml_ancv, test.statistic = "F")


# 95% CI for estimated parameter
Est.val

# plot the predicted values
par(mfrow = c(2,2))
# moist
PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = 12), ylab = "IEM-P (Temp = 12)")
PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = 23), ylab = "IEM-P (Temp = 23)")
# temp
PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = .05), ylab = "IEM-P (Moist = .05)")
PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = .25), ylab = "IEM-P (Moist = .25)")

