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
m1 <- lme(log(p) ~ co2 * time, random = ~1|ring/plot,  data = subsetD(PRmOl, pre))
m2 <- lme(log(p) ~ co2 * time, random = ~1|ring,  data = subsetD(PRmOl, pre))
m3 <- lme(log(p) ~ co2 * time, random = ~1|id,  data = subsetD(PRmOl, pre))
anova(m1, m2, m3)
# m2 is slightly better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
# model 4 looks better

Iml_pre <- atcr.cmpr(m2, rndmFac="ring")[[4]]

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
m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * time, random = ~1|ring/plot,  data = subsetD(iem, post))
m2 <- lme((p + 1.6)^(-1.1515) ~ co2 * time, random = ~1|ring,  data = subsetD(iem, post))
m3 <- lme((p + 1.6)^(-1.1515) ~ co2 * time, random = ~1|id,  data = subsetD(iem, post))
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


## ---- Stat_FACE_IEM_Phosphate_preCO2_Smmry
# The starting model is:
Iml_pre$call
xtable(Anova(Iml_pre), floating = FALSE)

# The final model is:
Fml_pre$call
xtable(Anova(Fml_pre), floating = FALSE)

## ---- Stat_FACE_IEM_Phosphate_postCO2_Smmry
# The starting model is:
Iml_post$call
xtable(Anova(Iml_post), floating = FALSE)

# The final model is:
Fml_post$call
xtable(Anova(Fml_post), floating = FALSE)

print(xtable(FACE_IEM_PostCO2_P_CntrstDf, floating = FALSE),include.rownames= FALSE)
