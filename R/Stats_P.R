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

Iml_pre <- atcr.cmpr(m2, rndmFac="ring")[[4]]

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
qqnorm(Fml_pre, ~ resid(.)|ring)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

### post-co2 ###
bxplts(value= "p", data= subset(iem, post))
bxplts(value= "p", ofst = 1.1, data= subset(iem, post))
# inverse  seems better

# different random factor strucures
m1 <- lme(1/(p + 1) ~ co2 * time, random = ~1|ring/plot, subset = post, data = iem)
m2 <- lme(1/(p + 1) ~ co2 * time, random = ~1|ring, subset = post, data = iem)
m3 <- lme(1/(p + 1) ~ co2 * time, random = ~1|id, subset = post, data = iem)
anova(m1, m2, m3)
# m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
# model 5 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[5]]

# The starting model is:
Iml_post$call

# model simplification
anova(Iml_post)

MdlSmpl(Iml_post)
# no factor is removed

Fml_post <- MdlSmpl(Iml_post)$model.reml

# The final model is:
Fml_post$call

anova(Fml_post)

summary(Fml_post)

plot(allEffects(Fml_post))

# contrast
cntrst<- contrast(Fml_post, 
                  a = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(iem$time[iem$post, drop = TRUE]), co2 = "elev"))
FACE_IEM_PostCO2_P_CntrstDf <- cntrstTbl(cntrst, data = iem[iem$post, ], digit = 2)

# model diagnosis
plot(Fml_post)
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))