## ---- Stat_FACE_IEM_Nitrate_preCO2

range(iem$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subsetD(iem, pre))
# sqrt seems slightly better

Iml_pre_no <- lmer(sqrt(no) ~ co2 * time + (1|block) + (1|ring) + (1|id), data = subsetD(iem, pre))

# The starting model is:
Iml_pre_no@call
Anova(Iml_pre_no)
Anova(Iml_pre_no, test.statistic = "F")

# model simplification
Fml_pre_no <- stepLmer(Iml_pre_no)

# The final model is:
Fml_pre_no@call

Anova(Fml_pre_no)
AnvF_pre_no <- Anova(Fml_pre_no, test.statistic = "F")
AnvF_pre_no

summary(Fml_pre_no)

plot(allEffects(Fml_pre_no))

# model diagnosis
plot(Fml_pre_no)
qqnorm(resid(Fml_pre_no))
qqline(resid(Fml_pre_no))

## ---- Stat_FACE_IEM_Nitrate_postCO2

############
# Post-CO2 #
############

bxplts(value= "no", data= postDF)
# log seems better, but remove one outlier
NoRmOl <- postDF
NoRmOl <- subsetD(NoRmOl, no != min(no, na.rm = TRUE))
bxplts(value= "no", data= NoRmOl)

# The initial model is:
Iml_post_no <- lmer(log(no) ~ co2 * time + (1|block) + (1|ring) + (1|id), data = NoRmOl)
Anova(Iml_post_no)

# The final model is:
Fml_post_no <- stepLmer(Iml_post_no, alpha.fixed = .1)
Fml_post_no@call

Anova(Fml_post_no)
AnvF_post_no <- Anova(Fml_post_no, test.statistic = "F")
AnvF_post_no

summary(Fml_post_no)

plot(allEffects(Fml_post_no))

# model diagnosis
plot(Fml_post_no)
qqnorm(resid(Fml_post_no))
qqline(resid(Fml_post_no))

# contrast

# contrast doesn't work with lmer. so use lme
lmeMod <- lme(log(no) ~ co2 * time, random = ~1|block/ring/id, data = NoRmOl)

cntrst<- contrast(lmeMod, 
                  a = list(time = levels(postDF$time), co2 = "amb"),
                  b = list(time = levels(postDF$time), co2 = "elev"))
FACE_IEM_PostCO2_NO_CntrstDf <- cntrstTbl(cntrst, data = postDF, variable = "no", digit = 2)
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
Iml_ancv_no <- lmer(log(no) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id),  data = postDF)
# model simplification
Fml_ancv_no <- stepLmer(Iml_ancv_no)
AnvF_no <- Anova(Fml_ancv_no)
Anova(Fml_ancv_no, test.statistic = "F")
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))
# looking at qqplot, there's one outlier so remove

# identifi the outlier and remove
qqval <- qqnorm(resid(Fml_ancv_no))[[2]]
min(qqval)
postDF[which(qqval == min(qqval)), "no"] <- NA

# rerun analysis
Iml_ancv_no <- lmer(log(no) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id),  
                 data = postDF, na.action = "na.omit")
Fml_ancv_no <- stepLmer(Iml_ancv_no)
Anova(Fml_ancv_no)
  # co2 is not significant so removed, but I would like to plot predicted values
  # for each treatment anyway so keep co2 factor in the model.
Fml_ancv_no <- update(Fml_ancv_no,~. + co2)
Anova(Fml_ancv_no)

AnvF_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_no
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))
# looks prety good

# main effect
plot(allEffects(Fml_ancv_no))

# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_no)

# calculate actual values
Est.val_no <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[4, ] + ciDF[1, 3],
  Moist = ciDF[2, ],
  Temp_Mean = ciDF[3, ]
  )

# reshape Est.val_no and make a table
Est_no <- ANCV_Tbl(df = Est.val_no)

## ---- Stat_FACE_IEM_Nitrate_preCO2_Smmry

# The starting model is:
Iml_pre_no@call

# Chi test
Anova(Iml_pre_no)

# F test
Anova(Iml_pre_no, test.statistic = "F")

# The final model is:
Fml_pre_no@call

# Chi test
Anova(Fml_pre_no)

# F test
AnvF_pre_no

## ---- Stat_FACE_IEM_Nitrate_postCO2_Smmry

# The starting model is:
Iml_post_no@call
Anova(Iml_post_no)

# The final model is:
Fml_post_no@call

# Chi-square test
Anova(Fml_post_no)

# F-test
AnvF_post_no

# Contrast
FACE_IEM_PostCO2_NO_CntrstDf

## ---- Stat_FACE_IEM_Nitrate_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv_no@call
Anova(Iml_ancv_no)

# The final model
Fml_ancv_no@call

# Chi-square
Anova(Fml_ancv_no)

# F-test
AnvF_no

# squared R
rsquared.glmm(Fml_ancv_no)

# 95% CI for estimated parameter
Est.val_no

# plot the predicted values
visreg(Fml_ancv_no, "Moist")
visreg(Fml_ancv_no, "Temp_Mean")
