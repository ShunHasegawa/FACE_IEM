# pre or post co2
# time 4 is pre-co2 but need to be in post as well 
# as it's used as a baseline
iem$pre <- ifelse(iem$time %in% c(1:4), TRUE, FALSE )
iem$post <- ifelse(!(iem$time %in% c(1:3)), TRUE, FALSE )

#############
# Phosphate #
#############

### Pre-CO2 ###
range(iem$p)
bxplts(value= "p", data= subset(iem, pre))
  # remove the higher outlier

PRmOl <- subset(iem, p < max(p))
bxplts(value= "p", data= subset(PRmOl, pre))
  # log transformation seems slightly better

# different random factor strucures
m1 <- lme(log(p) ~ co2 * time, random = ~1|ring/plot, subset = pre, data = PRmOl)
m2 <- lme(log(p) ~ co2 * time, random = ~1|ring, subset = pre, data = PRmOl)
m3 <- lme(log(p) ~ co2 * time, random = ~1|id, subset = pre, data = PRmOl)
anova(m1, m2, m3)
  # m2 is slightly better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
  # model 4 looks better

Iml_Pre <- atcr.cmpr(m2, rndmFac="ring")[[4]]

# The stating model is:
Iml_Pre$call

# model simplification
anova(Iml_Pre)

MdlSmpl(Iml_Pre)
  # time * co2 and co2 are removed

Fml_pre <- MdlSmpl(Iml_Pre)$model.reml

# The final model is:
Fml_pre$call

anova(Fml_pre)

summary(Fml_pre)

plot(allEffects(Fml_pre))

# model diagnosis
plot(Fml_pre)
qqnorm(Fml_pre, ~ resid(.)|ring)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

### post-co2 ###

