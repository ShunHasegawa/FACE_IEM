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

## ---- Stat_FACE_IEM_Phosphate_postCO2_withSoilVar
############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################

# plot all variables
scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = iem, diag = "boxplot")
scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = iem, diag = "boxplot")

# plot for each plot against soil variables
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

# co2  x time
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | co2, subsetD(iem, !pre), type = c("r", "p"), 
             panel = panel.superpose, groups = time))

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | time, subsetD(iem, !pre), type = c("r", "p"), 
             panel = panel.superpose, groups = co2))

# ring  x time
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring , subsetD(iem, !pre), type = c("r", "p"), 
              panel = panel.superpose, groups = time))

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring , subsetD(iem, !pre), type = c("r", "p"), 
              panel = panel.superpose, groups = id))

############
# Analysis #
############
m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * time * (Moist + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m3 <- MdlSmpl(m1)$model.reml
Anova(m3)
AIC(m3)
plot(allEffects(m3))

qqnorm(m3, ~ resid(.)|id)
qqnorm(residuals.lm(m3))
qqline(residuals.lm(m3))
# pretty much same result as above


# reverse transormation
ReTrf <- function(x) x^(-1/1.1515)-1.6

# plot predicted value
par(mfrow = c(3, 4))
for (i in c(5:14)){
  visreg(m3, 
         xvar = "Temp_Max", 
         by = "co2", 
         trans = ReTrf, 
         level = 1, # take random factor into accound
         overlay = TRUE, 
         print.cond=TRUE, 
         cond = list(time = i),
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")),
         main = paste("Time =", i),
         legend = FALSE, 
         ylim = c(0, 15))
  lines(x = range(iem$Temp_Max[iem$time == i]), y = c(0, 0), lwd = 2)
}
plot.new()
legend("topright", leg = c("amb", "elev", "Temp range"), 
       col = c("blue", "red", "black"), lty = 1, lwd = 2, 
       bty = "n")

par(mfrow = c(1,2))
for (i in c("amb", "elev")){
  visreg(m3, 
         xvar = "Temp_Max", 
         by = "time", 
         overlay = TRUE, 
         print.cond=TRUE, 
         cond = list(co2 = i),
         main = paste("Time =", i),
         legend = TRUE, 
         ylim = c(0, 0.6))
}

##########################
# Run lme for each month #
##########################
# each month
ResLmeMonth <- dlply(subset(iem, !pre), .(time), LmeMonth)
ResLmeMonth

# months where we see co2 enhancement
iem$GroupMonth <- factor(ifelse(iem$time %in% c(5,6,7), "enhance1",
                                ifelse(iem$time %in% c(12, 13, 14), "enhance2", "nd")))
ResLmeEnhancedMonth <- dlply(subset(iem, !pre), .(GroupMonth), LmeMonth)
ResLmeEnhancedMonth

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

