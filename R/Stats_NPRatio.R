## ---- Stat_FACE_IEM_NPRatio_postCO2

#############
# N:P ratio #
#############
# add NP ratio
iem$NP <- with(iem, no + nh/p)

# parcent change
iem$SumN <- with(iem, no + nh) # total of nitrate and ammonium
iem <- ddply(iem, .(ring, plot, co2, block, id), function(x) PerChange(x, type = "log"))
iem <- within(iem, {
  # Delt function within PerChange returns log(x(t)/x(t-k)) so transform it back
  # to normal scale
  pcP <- exp(pcP) 
  pcN <- exp(pcN)
  pcNP <- pcN/pcP
})

############
# post-co2 #
############
plot(NP ~ no, data = iem)
  # Due to high concetnration of nitrate compared to nh and p, NP ratios is
  # highly dependent on nitrate concentraion.

bxplts(value= "NP", data= subsetD(iem, post))
# use log

# The initial model is
Iml_post_NP <- lmer(log(NP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                    data = subsetD(iem, post))
Anova(Iml_post_NP, test.statistic = "F")
# no co2 effect

# model diagnosis
plot(Iml_post_NP)
qqnorm(resid(Iml_post_NP))
qqline(resid(Iml_post_NP))
  # one obvious outlier at the left bottom

# Remove the outlier and re-run
a <- which(qqnorm(resid(Fml_post_NP))$y == min(qqnorm(resid(Fml_post_NP))$y))
Iml_post_NP3 <- lmer(log(NP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                    data = subsetD(iem, post), subset = -a)
plot(Iml_post_NP3)
qqnorm(resid(Iml_post_NP3))
qqline(resid(Iml_post_NP3))
  # improved
Anova(Iml_post_NP3, test.statistic = "F")
  # no co2 effect anyway

##############################
# NP ratio of Percent change #
##############################
bxplts(value= "pcNP", data= subsetD(iem, post))

# The initial model is
Iml_post_pcNP <- lmer(log(pcNP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                      data = subsetD(iem, post))
Anova(Iml_post_pcNP, test.statistic = "F")
# no co2 effect

# model diagnosis
plot(Iml_post_pcNP)
qqnorm(resid(Iml_post_pcNP))
qqline(resid(Iml_post_pcNP))
