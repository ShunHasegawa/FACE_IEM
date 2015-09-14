## ---- Stat_FACE_IEM_NPRatio_postCO2

#############
# N:P ratio #
#############

## ---- Stat_FACE_IEM_Analyse_NP
############
# NP ratio #
############

bxplts(value= "logNP", data= subsetD(iem, post), lambda = seq(-.1, .1, length = 10))
# use log

# The initial model is
Iml_post_NP <- lmer(logNP ~ co2 * time + (1|block) + (1|ring)  + (1|id), data = subsetD(iem, post))
summary(Iml_post_NP)
Anova(Iml_post_NP, test.statistic = "F")
# co2 x time interaction

# The final model is:
Fml_post_NP <- Iml_post_NP
AnvF_post_np <- Anova(Fml_post_NP, test.statistic = "F")
AnvF_post_np


plot(allEffects(Fml_post_NP))

## ---- Stat_FACE_IEM_Analyse_NP_plot
# model diagnosis
plot(Fml_post_NP)
qqnorm(resid(Fml_post_NP))
qqline(resid(Fml_post_NP))

# contrast

# contrast doesn't work with lmer. so use lme
tdf <- subsetD(iem, post)
tdf$co2 <- relevel(tdf$co2, "elev")
# tdf$time <- relevel(tdf$time, 5)

lmeMod <- lme(logNP ~ co2 * time, random = ~1|block/ring/id, data = tdf)

cntrst<- contrast(lmeMod, 
                  a = list(time = levels(tdf$time), co2 = "amb"),
                  b = list(time = levels(tdf$time), co2 = "elev"))

FACE_IEM_PostCO2_NP_CntrstDf <- cntrstTbl(cntrst, data = tdf, variable  = "logNP", digit = 2)
FACE_IEM_PostCO2_NP_CntrstDf

# what if I remove NH and P outliers
which(iem$p == max(iem$p))
which(iem$nh > 800)
ttdf <- subsetD(iem[-c(15, 17, 63), ], post)
iem[c(15, 17, 63), ]
tml <- lmer(logNP ~ co2 * time + (1|block) + (1|ring)  + (1|id), data = ttdf)
Anova(tml, test.statistic = "F")
Anova(Fml_post_NP, test.statistic = "F")
ldply(list(Fml_post_NP, tml), r.squared)

## ---- Stat_FACE_IEM_Phosphate_postCO2_withSoilVar

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################

# plot all variables
scatterplotMatrix(~ logNP + Moist + Temp_Mean|co2, data = postDF, diag = "boxplot")

# plot for each ring
xyplot(logNP ~ Moist|co2, groups = ring, postDF, type = c("r", "p"))
xyplot(logNP ~ Temp_Mean|co2, groups = ring, postDF, type = c("r", "p"))

############
# Analysis #
############

# use lmer to get confidence intervals for predicted values
Iml_ancv_np <- lmer(logNP ~ co2 * (Moist + Temp_Mean) + 
                      (1|block) + (1|ring) + (1|id), data = postDF)
# random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv_np, test.statistic = "F")

# model simplification
Fml_ancv_np <- stepLmer(Iml_ancv_np, alpha.fixed = 0.1)
AnvF_NP <- Anova(Fml_ancv_np, test.statistic = "F")
AnvF_NP

# main effects
plot(allEffects(Fml_ancv_np))

# model diagnosis
plot(Fml_ancv_np)
qqnorm(resid(Fml_ancv_np))
qqline(resid(Fml_ancv_np))

##########################
## plot predicted value ##
##########################
par(mfrow = c(1,2))
# moist
l_ply(c(12, 23), function(x){
  PltPrdVal(model = Fml_ancv_np, variable = "Moist", cond = list(Temp_Mean = x),
            ylab = paste("NP (Temp = ", x, ")", sep = ""),
            #             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

# temp
l_ply(c(.05, .25), function(x){
  PltPrdVal(model = Fml_ancv_np, variable = "Temp_Mean", cond = list(Moist = x),
            ylab = paste("NP (Moist = ", x, ")", sep = ""),
            #             ylim = c(0, 6),
            trans = exp,
            data = postDF)
})

################################################
# confidence interval for estimated parameters #
################################################
ciDF <- CIdf(model = Fml_ancv_np)

# calculate actual values
Est.val_np <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ],
  co2elev.Moist = ciDF[5, ] + ciDF[3, 3],
  co2elev.Temp_Mean = ciDF [6, ] + ciDF[4, 3]
)

Est.val_np

# reshape Est.val and make a table
Est_NP <- ANCV_Tbl(Est.val_np)

## ---- FACE_IEM_Analyse_NP_figure

# Geometric mean for each treatment----
NP_ring <- ddply(iem, .(date, block, co2, ring), summarise, Rgeo = gm_mean(NP))
NP_co2 <- ddply(NP_ring, .(date, co2), summarise, 
                Mean = gm_mean(Rgeo), 
                SE = geoCI(Rgeo)[3])

theme_set(theme_bw())
p <- ggplot(NP_co2, aes(x = date, y = log(Mean), group = co2, science_theme))
p2 <- p + 
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
             linetype = "dashed", col = "black") +
  geom_line(aes(linetype = co2), position = position_dodge(20)) + 
  geom_errorbar(aes(ymin = log(Mean) - log(SE), 
                    ymax = log(Mean) + log(SE)),
                width = 0,
                position = position_dodge(20)) + 
  geom_point(aes(fill = co2),
             shape = 21, 
             position = position_dodge(20),
             size = 4) +
  labs(x = "Month", y = expression(log(R[NP]))) +
  scale_x_date(breaks= date_breaks("3 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2012-6-15", "2014-3-29"))) +
  scale_fill_manual(values = c("black", "white"), 
                    labels = c("Ambient", expression(eCO[2]))) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Ambient", expression(eCO[2]))) +
  science_theme + 
  theme(legend.position = c(.8, .8))
ggsavePP(filename = "output//figs/FACE_NPRatio_CO2", plot = p2, width = 6.65, height = 3)

