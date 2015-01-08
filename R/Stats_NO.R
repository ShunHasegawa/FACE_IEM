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
Anova(Iml_post, test.statistic = "F")
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
FACE_IEM_PostCO2_NO_CntrstDf <- cntrstTbl(cntrst, data = iem[iem$post, ], variable = "no", digit = 2)
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
Fml_ancv_NO <- Fml_ancv
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

# squared R
rsquared.glmm(Fml_ancv)

# 95% CI for estimated parameter
Est.val

# plot the predicted values
visreg(Fml_ancv, "Moist")
visreg(Fml_ancv, "Temp_Mean")
