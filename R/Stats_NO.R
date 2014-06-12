## ---- Stat_FACE_IEM_Nitrate_preCO2

range(iem$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subsetD(iem, pre))
# sqrt seems slightly better

# different random factor strucures
m1 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring/plot,  data = subsetD(iem, pre))
m2 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring,  data = subsetD(iem, pre))
m3 <- lme(sqrt(no) ~ co2 * time, random = ~1|id,  data = subsetD(iem, pre))
anova(m1, m2, m3)
# m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
# model 3 looks better

Iml_pre <- atcr.cmpr(m1, rndmFac="ring/plot")[[3]]

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

bxplts(value= "no", data= subset(iem, post))
bxplts(value= "no", ofst = 30, data= subset(iem, post))
# log seems better

# different random factor strucures
m1 <- lme(log(no + 30) ~ co2 * time, random = ~1|ring/plot,  data = subsetD(iem, post))
m2 <- lme(log(no + 30) ~ co2 * time, random = ~1|ring,  data = subsetD(iem, post))
m3 <- lme(log(no + 30) ~ co2 * time, random = ~1|id,  data = subsetD(iem, post))
anova(m1, m2, m3)
# m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
# model 4 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[4]]

# The starting model is:
Iml_post$call

# model simplification
Anova(Iml_post)

MdlSmpl(Iml_post)
# co2xtime, co2 are removed

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
scatterplotMatrix(~ I(log(no + 30)) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean, data = iem,
                  diag = "boxplot")
# negative correlation with Temp_Max

# plot for each plot against soil variables
print(xyplot(log(no + 30) ~ log(Moist) | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot(log(no + 30) ~ Temp_Min | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

## Analysis ##
Iml_ancv <- lme(log(no + 30) ~ co2 * (time + Moist + Temp_Max), 
           random = ~1|ring/plot,  data = subsetD(iem, !pre))
SmMd <-  MdlSmpl(Iml_ancv)$model.reml
Anova(SmMd)

# Temp_Max may be removed
SmMdML <- MdlSmpl(Iml_ancv)$model.ml
SmMd2 <- update(SmMdML, ~. - Temp_Max)
anova(SmMdML, SmMd2)
Anova(SmMd2)

Fml_ancv <- update(SmMd2, method = "REML")
Anova(Fml_ancv)
summary(Fml_ancv)
plot(allEffects(Fml_ancv))

plot(Fml_ancv)
qqnorm(Fml_ancv, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_ancv))
qqline(residuals.lm(Fml_ancv))
# no co2 effect

## ---- Stat_FACE_IEM_Nitrate_postCO2_Smmry

# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

## ---- Stat_FACE_IEM_Nitrate_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv$call
Anova(Iml_ancv)

# The final model
Fml_ancv$call
Anova(Fml_ancv)
