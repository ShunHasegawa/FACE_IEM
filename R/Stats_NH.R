## ---- Stat_FACE_IEM_Ammonium_preCO2

range(iem$nh)

###########
# Pre-CO2 #
###########

bxplts(value= "nh", data= subsetD(iem, pre))
 # remove two high values(outlier?)

nhRmOl <- subset(iem, nh < 800)
bxplts(value= "nh", data= subsetD(nhRmOl, pre))
bxplts(value= "nh", ofst= 10, data= subsetD(nhRmOl, pre))
  # none of the transformation seems to be working well,
  # but let's carry on with log anyway

# different random factor strucures
m1 <- lme(log(nh + 10) ~ co2 * time, random = ~1|ring/plot, data = subsetD(nhRmOl, pre))
m2 <- lme(log(nh + 10) ~ co2 * time, random = ~1|ring, data = subsetD(nhRmOl, pre))
m3 <- lme(log(nh + 10) ~ co2 * time, random = ~1|id, data = subsetD(nhRmOl, pre))
anova(m1, m2, m3)
  # m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
  # model 5 looks better

Iml_pre <- atcr.cmpr(m1, rndmFac="ring/plot")[[5]]

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
  # as expected,, not great...

## ---- Stat_FACE_IEM_Ammonium_postCO2

############
# Post-CO2 #
############

bxplts(value= "nh", data= subsetD(iem, post))
  # log seems better

# different random factor strucures
m1 <- lme(log(nh) ~ co2 * time, random = ~1|ring/plot, data = subsetD(iem, post) )
m2 <- lme(log(nh) ~ co2 * time, random = ~1|ring, data = subsetD(iem, post) )
m3 <- lme(log(nh) ~ co2 * time, random = ~1|id, data = subsetD(iem, post) )
anova(m1, m2, m3)
  # m2 is better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
  # model 3 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[3]]

# The starting model is:
Iml_post$call

# model simplification
Anova(Iml_post)

MdlSmpl(Iml_post)
  # co2 x time, co2 are removed

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

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar
##########
# ANCOVA #
##########

#######################
# plot soil variables #
#######################
# plot all variables
scatterplotMatrix(~ I(log(nh)) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean, data = iem,
                  diag = "boxplot")

# plot for each plot against soil variables
print(xyplot(log(nh) ~ log(Moist) | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot(log(nh) ~ Temp_Max | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

############
# Analysis #
############
# Iml_ancv <- lme(log(nh) ~ co2 * log(Moist), 
#                 random = ~1|block/ring/plot, data = subsetD(iem, !pre))
# it gives me error message for some reasons.. so try lmer

Iml_ancv <- lmer(log(nh) ~ co2 * log(Moist) +
                   (1|block/ring/plot), data = subsetD(iem, !pre))

Anova(Iml_ancv)

Fml_ancv <- lmer(log(nh) ~ co2 + log(Moist) +
                   (1|block/ring/plot), data = subsetD(iem, !pre))

anova(Iml_ancv, Fml_ancv)
Anova(Fml_ancv)

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))

# not great...

# plot predicted value
PltPr <- function(){
  visreg(Fml_ancv, xvar = "Moist", 
         by = "co2", 
         trans = exp, 
         level = 1, # take random factor into accound
         overlay = TRUE, print.cond=TRUE, 
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")),
         ylim  = c(50, 100))
  
  timePos <- seq(50, 90, length.out = 10)
  times <- c(5:14)
  
  for (i in 1:10){
    lines(x = range(iem$Moist[iem$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(iem$Moist[iem$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
}

PltPr()

## ---- Stat_FACE_IEM_Ammonium_preCO2_Smmry

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ---- Stat_FACE_IEM_Ammonium_postCO2_Smmry

# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

## ---- Stat_FACE_IEM_Ammonium_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv$call
Anova(Iml_ancv)

# The final model
Fml_ancv$call
Anova(Fml_ancv)

# plot the predicted values
PltPr()
