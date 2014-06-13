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

# Soil moisture and temperature seems to be correlated
# Temperature may regulate soil moisture

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

############
# Blocking #
############
# Figs from above showed similarrity in Soil Moist and Temp_Max
# between Ring 1&2, 3&4, and 5&6 so block them

iem$block  <- recode(iem$ring, "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")

# Note Temp_Max and log(Moist) appears to be correlated so shouln't be 
# placed in a multiple regression model
Iml_ancv <- lme((p + 1.6)^(-1.1515) ~ co2 * log(Moist), 
                random = ~1|block/ring/plot,  data = subsetD(iem, !pre))
Anova(Iml_ancv)
Fml_ancv <- MdlSmpl(Iml_ancv)$model.reml
Anova(Fml_ancv)
plot(allEffects(Fml_ancv))

# plot predicted value
PltPr_Moist <- function(){
  visreg(Fml_ancv, 
         xvar = "Moist",
         by = "co2", 
         trans = ReTrf,
         level = 1, # take random factor into accound
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
}
PltPr_Moist()

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
