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
m1 <- lme(log(p) ~ co2 * time, random = ~1|block/ring/plot,  data = subsetD(PRmOl, pre))
RndmComp(m1)$anova
m2 <- RndmComp(m1)[[2]]
# m2 is slightly better

# autocorelation
atcr.cmpr(m2)$models
# model 4 looks better

Iml_pre <- atcr.cmpr(m2)[[4]]

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
m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * time, random = ~1|block/ring/plot,  data = subsetD(iem, post))
RmMd <- RndmComp(m1)
RmMd$anova
# m3 is better

# autocorelation
ArgMd <- atcr.cmpr(RmMd[[3]])
ArgMd$models
# model 4 looks better

Iml_post <- ArgMd[[4]]

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

scatterplotMatrix(~ log(p) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = iem, diag = "boxplot")
scatterplotMatrix(~ log(p) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = iem, diag = "boxplot")

scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = iem, diag = "boxplot")

# plot for each plot against soil variables
print(xyplot((p + 1.6)^(-1.1515) ~ log(Moist) | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

############
# Analysis #
############

# use lmer to get confidence intervals for predicted values
m1 <- lmer(log(p) ~ co2 * Moist * Temp_Mean +
                   (1|block/ring/plot), data = subsetD(iem, !pre))
m2 <- lmer(log(p) ~ co2 * (Moist + Temp_Mean) +
                   (1|block/ring/plot), data = subsetD(iem, !pre))
anova(m1, m2)
# m2 is better
Anova(m2)
Anova(m2, test.statistic = "F")
# significant co2:moist and co2:temp interaction
Fml_ancv <- m2

# main effects
plot(allEffects(Fml_ancv))

##########################
## plot predicted value ##
##########################

# vs Moist
PltPr_Moist <- function(){
  visreg(Fml_ancv, 
         xvar = "Moist",
         by = "co2", 
         trans = exp,
         overlay = TRUE, 
         print.cond=TRUE, 
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")),
         ylim = c(0, 6))
  
  timePos <- seq(0, 5, length.out = 10)
  times <- c(5:14)
  for (i in 1:10){
    lines(x = range(iem$Moist[iem$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(iem$Moist[iem$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
  legend("topright", lty = 1, leg = "Moisture range", bty = "n")
}
PltPr_Moist()

# vs Temp
PltPr_Temp <- function(){
  visreg(Fml_ancv, 
         xvar = "Temp_Mean",
         by = "co2", 
         trans = exp,
         overlay = TRUE, 
         print.cond=TRUE, 
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")),
         ylim = c(0, 6))
  
  timePos <- seq(0, 5, length.out = 10)
  times <- c(5:14)
  for (i in 1:10){
    lines(x = range(iem$Temp_Mean[iem$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(iem$Temp_Mean[iem$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
  legend("topright", lty = 1, leg = "Temperature range", bty = "n")
}
PltPr_Temp()

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

# confidence interval for estimated parameters
test <- confint(Fml_ancv, method = "boot")
test


newDF$id <- newDF$ring:newDF$plot
testPr <- cbind(newDF,predict(mlmer, newdata = newDF))
names(testPr)[7] <- "predict"

a <- summary(Fml_ancv)
a$coefficients



m1 <- lm(log(p) ~ co2 * (Moist + Temp_Mean), data = subsetD(iem, !pre))
summary(m1)
anova(m1)
visreg(m1, "Temp_Mean", by = "co2", overlay = TRUE, trans = exp)
visreg(m1, "Moist", by = "co2", overlay = TRUE, trans = exp)

plot(log(p) ~ Temp_Mean, subsetD(iem, !pre), col = co2)
a <- lm(log(p) ~ co2 * Temp_Mean, data = subsetD(iem, !pre))
cf <- coef(a)
abline(cf[1], cf[3])
abline(cf[1] + cf[2], cf[3] + cf[4], col = "red")

par(mfrow = c(1,2))
plot(p ~ Temp_Mean, subsetD(iem, !pre), col = co2)
plot(p ~ Moist, subsetD(iem, !pre), col = co2)
a <- lm(log(p) ~ co2 * Temp_Mean, data = subsetD(iem, !pre))
cf <- coef(a)
abline(cf[1], cf[3])
abline(cf[1] + cf[2], cf[3] + cf[4], col = "red")
?abline




## Plot predicted values for each block ##

# Create a data frame for explanatory
expDF <- with(iem, expand.grid(ring = unique(ring), 
                               plot = unique(plot),
                               Moist = seq(min(Moist, na.rm = TRUE), max(Moist, na.rm = TRUE), length.out= 100),
                               Temp_Mean = seq(min(Temp_Mean, na.rm = TRUE), max(Temp_Mean, na.rm = TRUE), length.out= 100)))
expDF <- within(expDF, {
  block = recode(ring, "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
  co2 = factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
})

# adjust moisture range for each block
boxplot(Moist ~ block, data = expDF)

AdjexpDF <- ldply(list("A", "B", "C"), function(x) BlkminMoist(variable = x, data = expDF))
boxplot(Moist ~ block, data = AdjexpDF)

# predicted values
PredVal <- predict(Fml_ancv, level = 0:3, newdata = AdjexpDF)
PredDF <- cbind(AdjexpDF, PredVal[, c(3:7)])

# plot
theme_set(theme_bw())
p <- ggplot(PredDF, aes(x = Moist, y = exp(predict.block), col = co2))
p + geom_line(group = co2) +
  geom_point(aes(x = Moist, y = p, col = co2), data = subsetD(iem, !pre)) + 
  scale_color_manual("co2", values = c("blue", "red")) +
  facet_grid(.~block) +
  labs(y = "IEM-P")


p <- ggplot(PredDF, aes(x = log(Moist), y = predict.block, col = co2))
p + geom_line() +
  geom_point(aes(x = log(Moist), y = (p + 1.6)^(-1.1515), col = co2), data = subsetD(iem, !pre)) + 
  scale_color_manual("co2", values = c("blue", "red")) +
  facet_grid(.~block, scale = "free_x") + 
  labs(y = expression((p+1.6)^(-1.1515)))


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

## ---- Stat_FACE_IEM_Phosphate_postCO2_withSoilVar_Smmry
# The initial model
Iml_ancv$call
Anova(Iml_ancv)

# The final model
Fml_ancv$call
Anova(Fml_ancv)

# plot the predicted values
PltPr_Moist()
