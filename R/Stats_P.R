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
                  data = postDF, diag = "boxplot")
scatterplotMatrix(~ log(p) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")
scatterplotMatrix(~ log(p) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")

scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")

# plot for each plot against soil variables
print(xyplot((p + 1.6)^(-1.1515) ~ log(Moist) | ring + plot, postDF, type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, postDF, type = c("r", "p")))

############
# Analysis #
############

# use lmer to get confidence intervals for predicted values
Iml_ancv <- lmer(log(p) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
  # random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv)

# model simplification
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
AnvF_P <- Anova(Fml_ancv, test.statistic = "F")
AnvF_P

# what if I allow this to reduce random factors
Anova(ml <- stepLmer(Iml_ancv, red.rndm = TRUE))
ml@call 
  # ring is removed. main effects are more significant than
  # before but there're interactions so it doesn't really
  # matter too much

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

##########################
## plot predicted value ##
##########################
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("IEM-P (Temp = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("IEM-P (Moist = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})


######################
# Plot all variables #
######################

# Moist & Temperature data frame

# Devide Moisture and Temperature by 6, then get the middle values
M <- seq(range(postDF$Moist)[1], range(postDF$Moist)[2], length.out = 4)
Mval <- round(M[-4] + (M[2] - M[1])/2, 2)

Tv <- seq(range(postDF$Temp_Mean)[1], range(postDF$Temp_Mean)[2], length.out = 4)
Tval <- round(Tv[-4] + (Tv[2] - Tv[1])/2, 0)

MTdf <- expand.grid(MoistVal = Mval,TempVal = Tval)

# compute predicted values and CI intervals
Lst_CI <- ddply(MTdf, .(MoistVal, TempVal), 
                function(x) BtsCI(model = Fml_ancv,
                                  MoistVal = x$MoistVal,
                                  TempVal = x$TempVal),
                .progress = "text")

# re-format the data frame for plotting
Lst_CI <- within(Lst_CI, {
  MoistVal = factor(MoistVal, levels = rev(unique(MoistVal)))
  TempVal = factor(TempVal)
})

save(Lst_CI, file = "output//data/FACE_IEM_P_LstCI")

# Add moist and temp levels to postDF. e.g. when 14.5 < Temp < 17.5, TempVal =
# 16 to overlay actual values on p2

MLev <- cut(postDF$Moist, breaks = M, include.lowest = TRUE)
TLev <- cut(postDF$Temp_Mean, breaks = Tv, include.lowest = TRUE)

postDF$MoistVal <- factor(MLev, labels = Mval)
postDF$TempVal <- factor(TLev, labels = Tval)

## Plot
theme_set(theme_bw())
p <- ggplot(Lst_CI, aes(x = co2, y = PredVal, col = co2))
p2 <- p + geom_point(size = 3) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = .5) +
  scale_color_manual(values = c("blue", "red"), expression(CO[2])) +
  facet_grid(MoistVal ~ TempVal, labeller=label_both) +
  labs(y = "log(IEM-P)") +
  geom_point(data = postDF, aes(x = co2, y = log(p)), position  = "jitter", alpha = .5)
p2
ggsave(filename = "output//figs/FACE_IEM_P_TempMoist.pdf", plot = p2, width = 8, height = 8)

################################################
# confidence interval for estimated parameters #
################################################
ciDF <- CIdf(model = Fml_ancv)

# calculate actual values
Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3],
  co2elev.Temp_Mean = ciDF [6, ] + ciDF[4, 3]
  )

Est.val

# reshape Est.val and make a table
Est_P <- ANCV_Tbl(Est.val)

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
Iml_ancv@call
Anova(Iml_ancv)

# The final model
Fml_ancv@call

# Chisq
Anova(Fml_ancv)

# F-test
Anova(Fml_ancv, test.statistic = "F")

# 95% CI for estimated parameter
Est.val

# plot the predicted values
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("IEM-P (Temp = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("IEM-P (Moist = ", x, ")", sep = ""),
            ylim = c(0, 6),
            trans = exp,
            data = postDF)
})