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

# different random factor strucures
m1 <- lme(log(no) ~ co2 * time, random = ~1|block/ring/plot,  data = NoRmOl)
RndmComp(m1)$anova
  # model4 looks better
RnMl <- RndmComp(m1)[[4]]

# autocorelation
atcr.cmpr(RnMl)$models
  # model 4 looks better
Iml_post <- atcr.cmpr(RnMl)[[4]]

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
Anova(Fml_ancv)
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
AnvF_NO <- Anova(Fml_ancv, test.statistic = "F")
AnvF_NO
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
# looks prety good

# main effect
plot(allEffects(Fml_ancv))

# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv)

# calculate actual values
Est.val <- ciDF

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
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

## ---- Stat_FACE_IEM_Nitrate_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv@call
Anova(Iml_ancv)

# The final model
Fml_ancv@call
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")

# 95% CI for estimated parameter
Est.val

# plot the predicted values
visreg(Fml_ancv, "Moist")
visreg(Fml_ancv, "Temp_Mean")
