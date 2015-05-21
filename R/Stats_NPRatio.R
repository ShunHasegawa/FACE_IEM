## ---- Stat_FACE_IEM_NPRatio_postCO2

#############
# N:P ratio #
#############
# add NP ratio
iem$NP <- with(iem, (no + nh)/p)

# NP ratio of parcent change
iem$SumN <- with(iem, no + nh) # total of nitrate and ammonium
iem <- ddply(iem, .(ring, plot, co2, block, id), 
             function(x) { d  <- x[order(x$date), ]
                           df <- within(d, {
                             pcN = exp(Delt(d$SumN, type = "log"))
                             pcP = exp(Delt(d$p, type = "log"))
                             pcNP = pcN/pcP
                             }
                             )
                           return(df)
                           }
             )

## ---- Stat_FACE_IEM_Analyse_NP
############
# NP ratio #
############
bxplts(value= "NP", data= subsetD(iem, post), lambda = seq(-.1, .1, length = 10))
# use log

# The initial model is
Iml_post_NP <- lmer(log(NP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                    data = subsetD(iem, post))
Anova(Iml_post_NP, test.statistic = "F")
# co2 x time interaction
plot(allEffects(Iml_post_NP))

## ---- Stat_FACE_IEM_Analyse_NP_plot
# model diagnosis
plot(Iml_post_NP)
qqnorm(resid(Iml_post_NP))
qqline(resid(Iml_post_NP))

## ---- Stat_FACE_IEM_Analyse_pcNP
##############################
# NP ratio of Percent change #
##############################
bxplts(value= "pcNP", data= subsetD(iem, post))

# The initial model is
Iml_post_pcNP <- lmer(log(pcNP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                      data = subsetD(iem, post))
Anova(Iml_post_pcNP, test.statistic = "F")
# no co2 effect

## ---- Stat_FACE_IEM_Analyse_pcNP_plot
# model diagnosis
plot(Iml_post_pcNP)
qqnorm(resid(Iml_post_pcNP))
qqline(resid(Iml_post_pcNP))

## ---- Stat_FACE_IEM_NPRatio_ANCOVA
##########
# ANCOVA #
##########
postDF <- subsetD(iem, !pre)

############
# NP ratio #
############

scatterplotMatrix(~ log(NP) + Moist + Temp_Mean, data = postDF, diag = "boxplot")
xyplot(log(NP) ~ Moist|ring, group = id, postDF, type = c("r", "p"))
xyplot(log(NP) ~ Temp_Mean|ring, group  = id, postDF, type = c("r", "p"))

Iml_ancv_NP <- lmer(NP ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = postDF)
Iml_ancv_NP2 <- lmer(log(NP) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = postDF)
ldply(list(Iml_ancv_NP, Iml_ancv_NP2), r.squared)
pl <- llply(list(Iml_ancv_NP, Iml_ancv_NP2), plot)
print(pl[[1]], position = c(0, 0, .5, 1), more = TRUE)
print(pl[[2]], position = c(0.5, 0, 1, 1))
par(mfrow = c(1, 2))
l_ply(list(Iml_ancv_NP, Iml_ancv_NP2), function(x) {
  qqnorm(resid(x))
  qqline(resid(x))
  })
# Iml_ancv_NP2 is a lot better
Anova(Iml_ancv_NP2, test.statistic = "F")

Fml__ancv_NP <- stepLmer(Iml_ancv_NP2)
Anova(Fml__ancv_NP, test.statistic = "F")
plot(allEffects(Fml__ancv_NP))

## ---- Stat_FACE_IEM_NPRatio_ANCOVA_plot
plot(Fml__ancv_NP)
qqnorm(resid(Fml__ancv_NP))
qqline(resid(Fml__ancv_NP))

## ---- Stat_FACE_IEM_pcNP_ANCOVA
########################
# NP ratio of % change #
########################
scatterplotMatrix(~ log(pcNP) + Moist + Temp_Mean, data = postDF, diag = "boxplot")
xyplot(log(pcNP) ~ Moist|ring, group = id, postDF, type = c("r", "p"))
xyplot(log(pcNP) ~ Temp_Mean|ring, group  = id, postDF, type = c("r", "p"))

Iml_ancv_pcNP <- lmer(log(pcNP) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = postDF)
Anova(Iml_ancv_pcNP, test.statistic = "F")
Fml__ancv_pcNP <- stepLmer(Iml_ancv_pcNP)
Anova(Fml__ancv_pcNP, test.statistic = "F")
plot(allEffects(Fml__ancv_pcNP))

## ---- Stat_FACE_IEM_pcNP_ANCOVA_plot
plot(Fml__ancv_pcNP)
qqnorm(resid(Fml__ancv_pcNP))
qqline(resid(Fml__ancv_pcNP))

