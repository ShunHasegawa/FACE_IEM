## ---- StatNitratePreCO2 ---- 

range(iem$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subset(iem, pre))
# sqrt seems slightly better

# different random factor strucures
m1 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring/plot, subset = pre, data = iem)
m2 <- lme(sqrt(no) ~ co2 * time, random = ~1|ring, subset = pre, data = iem)
m3 <- lme(sqrt(no) ~ co2 * time, random = ~1|id, subset = pre, data = iem)
anova(m1, m2, m3)
# m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
# model 3 looks better

Iml_pre <- atcr.cmpr(m1, rndmFac="ring/plot")[[3]]

# The starting model is:
Iml_pre$call

# model simplification
anova(Iml_pre)

MdlSmpl(Iml_pre)
# time * co2 and co2 are removed

Fml_pre <- MdlSmpl(Iml_pre)$model.reml

# The final model is:
Fml_pre$call

anova(Fml_pre)

summary(Fml_pre)

plot(allEffects(Fml_pre))

# model diagnosis
plot(Fml_pre)
qqnorm(Fml_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

## ---- StatNitratePreCO2Smmry ---- 
# The starting model is:
Iml_pre$call
anova(Iml_pre)

# The final model is:
Fml_pre$call
anova(Fml_pre)


## ---- StatNitratePostCO2 ---- 

############
# Post-CO2 #
############

bxplts(value= "no", data= subset(iem, post))
bxplts(value= "no", ofst = 30, data= subset(iem, post))
# log seems better

# different random factor strucures
m1 <- lme(log(no + 30) ~ co2 * time, random = ~1|ring/plot, subset = post, data = iem)
m2 <- lme(log(no + 30) ~ co2 * time, random = ~1|ring, subset = post, data = iem)
m3 <- lme(log(no + 30) ~ co2 * time, random = ~1|id, subset = post, data = iem)
anova(m1, m2, m3)
# m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
# model 4 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[4]]

# The starting model is:
Iml_post$call

# model simplification
anova(Iml_post)

MdlSmpl(Iml_post)
# co2xtime, co2 are removed

Fml_post <- MdlSmpl(Iml_post)$model.reml

# The final model is:
Fml_post$call

anova(Fml_post)

summary(Fml_post)

plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))

## ---- StatNitratePostCO2Smmry ---- 
# The starting model is:
Iml_post$call
anova(Iml_post)

# The final model is:
Fml_post$call
anova(Fml_post)

