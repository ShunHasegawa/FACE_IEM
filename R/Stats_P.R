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

Iml_post <- atcr.cmpr(m1, rndmFac="ring/plot")[[4]]

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

# fit soil variable
library(lattice)
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Moist | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))

?xyplot
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | ring, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Moist | co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | co2, subsetD(iem, !pre), type = c("r", "p")))



m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * time + Moist + Temp_Mean + Temp_Min + Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, post))
m2 <- lme((p + 1.6)^(-1.1515) ~ co2 * time + Temp_Min, 
          random = ~1|ring/plot,  data = subsetD(iem, post))

m1 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Mean + Temp_Min + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
Anova(m8)

m2 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Mean), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m3 <- lme((p + 1.6)^(-1.1515) ~ co2 + Moist + Temp_Mean + Temp_Min + Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m4 <- lme((p + 1.6)^(-1.1515) ~ co2 + Moist + Temp_Mean + Temp_Min, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m5 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Min), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m6 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Min + Temp_Max + Temp_Mean) + Temp_Min:Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), , method = "ML")
m7 <- lme((p + 1.6)^(-1.1515) ~ co2 * Moist * Temp_Min * Temp_Max * Temp_Mean, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
m7 <- lme((p + 1.6)^(-1.1515) ~ (co2 + Moist + Temp_Min + Temp_Max + Temp_Mean)^4, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
m7.1 <- lme((p + 1.6)^(-1.1515) ~ (co2 + Moist + Temp_Min + Temp_Max + Temp_Mean)^2, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")


m8 <- stepAIC(m7.1)
Anova(m8)
m7.2 <- update(m8, ~. - Moist:Temp_Min)
anova(m8, m7.2)
m9 <- stepAIC(m7.2)
Anova(m9)
summary(m9)
AIC(m9)

qqnorm(m1, ~ resid(.)|id)
qqnorm(residuals.lm(m1))
qqline(residuals.lm(m1))

AIC(m1)

m9 <- update(m8, method = "REML")
Anova(m9)
AIC(m9)
AIC(m1)

Anova(m8)

Anova(m6)

anova(m1, m6)
summary(m1)
plot(allEffects(m8))

boxplot(Moist ~ co2 * time, data = iem)
Anova(m1)

head(iem)
SoilVarDF <- iem[, c("co2", "time", "Moist", "Temp_Mean", "Temp_Min", "Temp_Max")]
SoilVarDF_CO2 <- ddply(SoilVarDF, .(time, co2),
                       function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")],na.rm = TRUE))
SoilVarMlt <- melt(SoilVarDF_CO2, id = c("co2", "time"))
SoilVarMlt$type <- factor(ifelse(SoilVarMlt$variable != "Moist", "Temp", "Moist"))
head(SoilVarMlt)

theme_set(theme_bw())
p <- ggplot(SoilVarMlt, aes(x = time, y = value, shape = variable, col = co2))
pl <- p + geom_point() +
  facet_grid(type ~., scale = "free_y")
ggsavePP(filename = "output//figs/FACE_IEM_SoilVarSummary", plot = pl, width = 6, height = 4)

plot(allEffects(m1))
summary(m1)
qqnorm(m1, ~ resid(.)|id)
qqnorm(residuals.lm(m1))
qqline(residuals.lm(m1))

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

