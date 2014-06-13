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
print(xyplot((p + 1.6)^(-1.1515) ~ log(Moist) | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

############
# Analysis #
############
Iml_ancv <- lme((p + 1.6)^(-1.1515) ~ co2 * (time + log(Moist) + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
Fml_ancv <- MdlSmpl(Iml_ancv)$model.reml
Anova(Fml_ancv)
summary(Fml_ancv)
plot(allEffects(Fml_ancv))

qqnorm(Fml_ancv, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_ancv))
qqline(residuals.lm(Fml_ancv))
# pretty much same result as above


# reverse transormation
ReTrf <- function(x) x^(-1/1.1515)-1.6

# plot predicted value
PltPr <- function(){
  par(mfrow = c(3, 4))
  for (i in c(5:14)){
    visreg(Fml_ancv, 
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
  par(mfrow = c(1,1))
}

PltPr()

# for each time point
par(mfrow = c(1,2))
for (i in c("amb", "elev")){
  visreg(Fml_ancv, 
         xvar = "Temp_Max", 
         by = "time", 
         overlay = TRUE, 
         print.cond=TRUE, 
         cond = list(co2 = i),
         main = paste("CO2 =", i),
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

#####################################
# Plot IEM-P against Moist and Temp #
#####################################
theme_set(theme_bw())
iem$TrP <- (iem$p + 1.6)^(-1.1515)

p <- ggplot(subsetD(iem, !pre), aes(x = Temp_Max, y = log(Moist), size = TrP, col = TrP))
p2 <- p + geom_point(alpha = .5) + 
  scale_size(range = c(8, 1)) +
  scale_color_gradientn(colours = c("red", "yellow", "blue"))

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output/figs/FACE_IEM_P_withSoilVar_ring", plot = pl, width = 6, height = 6)

pl  <- p2
ggsavePP(file = "output/figs/FACE_IEM_P_withSoilVar", plot = pl, width = 6, height = 6)




# Hypothesis
# Temperature has negative effects only when soil moisture is really low
# However temperature and moisture is correlated so unable to include in regression model at the same time
# split soil moisture into multiple levels and do regression against Temp for each level of moisture

##################
# Split moisture #
##################

# Moist distribution
postCo2 <- subsetD(iem, !pre)

MoistHist <- hist(log(postCo2$Moist),breaks = 10)
MoistHist$counts
# minimum number of observation is 1; unable to do analysis
# so reduce the number of breaks

MoistHist <- hist(log(postCo2$Moist),breaks = 2)


MoistHist <- hist(postCo2$Moist,breaks = 2)

# break Moist according to histgram = every .02 (= 2 %)
MoistBr <- MoistHist$breaks





postCo2$MoistLev <- factor(ifelse(postco2$Moist <= .08, "low", "high"))
postCo2$MoistLev <- factor(ifelse(postco2$Moist <= .05, "low", "high"))

plot(Moist ~ MoistLev, data = postCo2)

xtabs(~ ring + MoistLev, data = postCo2)

# analysis
Iml_ancv <- lme((p + 1.6)^(-1.1515) ~ co2 * MoistLev * Temp_Max, 
                random = ~1|ring/plot,  data = subsetD(postCo2))

Anova(Iml_ancv)
m1 <- update(Iml_ancv, method = "ML")
m1. <- update(m1, ~. - co2:MoistLev:Temp_Max)
anova(m1, m1.)
Anova(m1.)
m2 <- MdlSmpl(m1.)$model.ml
m3 <- update(m2, ~. - co2:Temp_Max)
anova(m2, m3)
m4 <- MdlSmpl(m3)
Anova(m4$model.reml)

plot(allEffects(m4$model.reml))


# ignore random factors
postCo2$MoistLev <- cut(log(postCo2$Moist), breaks = 2, include.lowest = TRUE,labels = c("L", "H"))

head(postCo2)
boxplot(Moist ~ MoistLev, data = postCo2)
m1 <- lm((p + 1.6)^(-1.1515) ~ co2 * MoistLev * Temp_Max, data = postCo2)
anova(m1)
m2 <- step(m1)
plot(allEffects(m1))


ReTrf <- function(x) x^(-1/1.1515)-1.6
m3 <- m2$model.reml
# plot predicted value
PltPr <- function(){
  par(mfrow = c(2, 2))
  for (i in c(1:4)){
    visreg(m3, 
           xvar = "Temp_Max", 
           by = "co2", 
           level = 1, # take random factor into accound
           overlay = TRUE, 
           print.cond=TRUE, 
           line.par = list(col = c("blue", "red")),
           points.par = list(col = c("blue", "red")),
           main = paste("MoistLev =", 2))
  }
  plot.new()
  legend("topright", leg = c("amb", "elev", "Temp range"), 
         col = c("blue", "red", "black"), lty = 1, lwd = 2, 
         bty = "n")
  par(mfrow = c(1,1))
}

PltPr()
, 
ylim = c(0, 15)
lines(x = range(iem$Temp_Max[iem$time == i]), y = c(0, 0), lwd = 2)













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
PltPr()
