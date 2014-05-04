range(iem$nh)

###########
# Pre-CO2 #
###########

bxplts(value= "nh", data= subset(iem, pre))
 # remove two high values(outlier?)

nhRmOl <- subset(iem, nh < 800)
bxplts(value= "nh", data= subset(nhRmOl, pre))
bxplts(value= "nh", ofst= 10,data= subset(nhRmOl, pre))
  # none of the transformation seems to be working well,
  # but let's carry on with log anyway

# different random factor strucures
m1 <- lme(log(nh + 10) ~ co2 * time, random = ~1|ring/plot, subset = pre, data = nhRmOl)
m2 <- lme(log(nh + 10) ~ co2 * time, random = ~1|ring, subset = pre, data = nhRmOl)
m3 <- lme(log(nh + 10) ~ co2 * time, random = ~1|id, subset = pre, data = nhRmOl)
anova(m1, m2, m3)
  # m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
  # model 5 looks better

Iml_pre <- atcr.cmpr(m1, rndmFac="ring/plot")[[5]]

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
  # as expected,, not great...

############
# Post-CO2 #
############

bxplts(value= "nh", data= subset(iem, post))
  # log seems better

# different random factor strucures
m1 <- lme(log(nh) ~ co2 * time, random = ~1|ring/plot, subset = post, data = iem)
m2 <- lme(log(nh) ~ co2 * time, random = ~1|ring, subset = post, data = iem)
m3 <- lme(log(nh) ~ co2 * time, random = ~1|id, subset = post, data = iem)
anova(m1, m2, m3)
  # m2 is better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
  # model 3 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[3]]

# The starting model is:
Iml_post$call

# model simplification
anova(Iml_post)

MdlSmpl(Iml_post)
  # co2 x time, co2 are removed

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

