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
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring, subsetD(iem, !pre), type = c("r", "p")), group = id)

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | ring + plot, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot,
             groups = co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring,
             groups = co2, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring + plot,
             groups = co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring ,
             groups = co2, subsetD(iem, !pre), type = c("r", "p")))

boxplot(iem$Moist)
print(xyplot((p + 1.6)^(-1.1515) ~ I(Temp_Max*Moist) | ring + plot,
             groups = co2, subsetD(iem, !pre), type = c("r", "p")))

scatterplot(p2 ~ Temp_Max | co2, iem)
scatterplot(p2 ~ Moist | co2, iem)
scatterplot(p2 ~ I(Moist^(1/3)) | co2, iem)



print(xyplot((p + 1.6)^(-1.1515) ~ Moist | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Moist | co2, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Moist | ring, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | ring, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | ring, subsetD(iem, !pre), type = c("r", "p"), group = id))

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | co2, subsetD(iem, !pre), type = c("r", "p"), group = id))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | co2, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | ring, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring, subsetD(iem, !pre), type = c("r", "p")))


print(xyplot((p + 1.6)^(-1.1515) ~ Moist | co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Mean | co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Min | co2, subsetD(iem, !pre), type = c("r", "p")))
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | co2, subsetD(iem, !pre), type = c("r", "p")))



m1 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m1 <- lme((p + 1.6)^(-1.1515) ~ co2*Moist, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m1 <- lme((p + 1.6)^(-1.1515) ~ co2*Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
anova(m1)
plot(allEffects(m1))

m2 <- lme((p + 1.6)^(-1.1515) ~ co2 * time + Temp_Min, 
          random = ~1|ring/plot,  data = subsetD(iem, post))

m1 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Mean + Temp_Min + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))


m2 <- lme((p + 1.6)^(-1.1515) ~ (co2+Moist+Temp_Max)^2, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
Anova(m2)
scatterplotMatrix(~ p + Moist + Temp_Max + Temp_Min + Temp_Mean, data = iem)
scatterplotMatrix(~ p2 + Moist + Temp_Max + Temp_Min + Temp_Mean, data = iem, diag = "boxplot")
scatterplotMatrix(~ p2 + Moist + Temp_Max + I((Moist) * Temp_Max), data = iem)
cor(iem[, c("Moist", "Temp_Max", "Temp_Mean", "Temp_Min")])

scatterplotMatrix(~ p2  +  Moist + Temp_Max + Temp_Min + Temp_Mean, 
                  data = subset(iem, co2  == "elev"), diag = "boxplot")
scatterplotMatrix(~ p2  +  Moist + Temp_Max + Temp_Min + Temp_Mean, 
                  data = subset(iem, co2  == "amb"), diag = "boxplot")
scatterplotMatrix(~ p2  +  Moist + I(Temp_Max^(-1)) + Temp_Min + Temp_Mean, 
                  data = subset(iem, co2  == "elev"), diag = "boxplot")
scatterplotMatrix(~ p2  +  Moist + I(Temp_Max^(-1)) + Temp_Min + Temp_Mean, 
                  data = subset(iem, co2  == "amb"), diag = "boxplot")



boxplot(Temp_Max ~ co2, data = iem)
bxplts(value = "Temp_Max", data = iem)

scatterplotMatrix(~ log(p + 1.6) + Moist + Temp_Max + Temp_Min + Temp_Mean, data = iem)


library(tree)
model <- tree(p2 ~ Moist + Temp_Max, data = iem)
plot(model)
text(model)

iem$tm <- iem$Temp_Max * iem$Moist
iem$T2 <- iem$Temp_Max^2
iem$M2 <- iem$Moist^2

m1 <- lme((p + 1.6)^(-1.1515) ~ co2 +Moist + Temp_Max + tm + T2 + M2, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * (Moist + Temp_Max + tm + T2 + M2), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
anova(m1)

m2 <- update(m1, ~. - M2)
m3 <- update(m2, ~. - T2)
m4 <- update(m3, ~. - tm)
m5 <- update(m4, ~. - co2)
plot(m5)
plot(allEffects(m5))

anova(m5)
Anova(m5)

m1 <- lme((p + 1.6)^(-1.1515) ~ co2 *(Moist + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
m2 <- lme((p + 1.6)^(-1.1515) ~ co2 *(Moist + Temp_Min), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
m3 <- lme((p + 1.6)^(-1.1515) ~ co2 *(Moist + Temp_Mean), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
anova(m1, m2, m3)
anova(m3)
m4 <- update(m3, ~. -co2:Moist)
anova(m4)





iem$tm <- iem$Temp_Min * iem$Moist
iem$T2 <- iem$Temp_Min^2
iem$M2 <- iem$Moist^2
m3 <- lme((p + 1.6)^(-1.1515) ~ co2 *(Moist + Temp_Min + tm + T2 + M2), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")
m4 <- update(m3, ~. -co2:M2)
m5 <- update(m4, ~. -co2:T2)
m6 <- update(m5, ~. -co2:tm)
m7 <- update(m6, ~. -M2)
m8 <- update(m7, ~. -tm)
m9 <- update(m8, ~. -co2:Moist)
m10 <- update(m9, ~. -co2:Temp_Min)
m11 <- update(m10, ~. +co2:T2)
m12 <- update(m11, ~. -Temp_Min)
m13 <- update(m12, ~. -co2:T2)
anova(m13)




anova(m1)
m4 <- stepAIC(m1)
anova(m4)
visreg(m4, xvar = "Moist", print.cond=TRUE)
visreg(m4, xvar = "Temp_Max", print.cond=TRUE)
with(iem, scatter3d(Moist, p2, Temp_Max, fit = "additive", rev =1))
plot(allEffects(m3))



Anova(m2)
m3 <- update(m2, ~. - co2:Moist)
anova(m2, m3)
anova(m3)
Anova(m3)

m1 <- lme((p + 1.6)^(-1.1515) ~ co2 *(Moist + I(Moist^2) + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre), method = "ML")

anova(m1)



a <- stepAIC(m1)
Anova(a)
plot(a)
dev.off()
visreg(a, xvar = "Moist", overlay = TRUE, print.cond=TRUE)
visreg(a, xvar = "tm", overlay = TRUE, print.cond=TRUE)


m1 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))


m1 <- lme((p + 1.6)^(-1.1515) ~ co2*I(Temp_Max^(-0.9899)), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
plot(m1)
Anova(m1)
plot(allEffects(m1))


m2 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Min + I(Temp_Min^2)), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))

m2 <- lme((p + 1.6)^(-1.1515) ~ co2*Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))

m3 <- lme((p + 1.6)^(-1.1515) ~ co2 + Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m3 <- lme((p + 1.6)^(-1.1515) ~ co2 + Moist, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))

 <- lme((p + 1.6)^(-1.1515) ~ co2 + Temp_Max + Moist, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m5 <- lme((p + 1.6)^(-1.1515) ~ Moist, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m6 <- lme((p + 1.6)^(-1.1515) ~ co2 +Moist + Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))
m7 <- lme((p + 1.6)^(-1.1515) ~ Moist + Temp_Max, 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))

Anova(m7)
avPlot(m7, ask = F)



summary(m4)
plot(allEffects(m5))
AIC(m1)

plot(allEffects(m1))

names(iem)
?scatterplotMatrix


?scatterplot

Anova(m1)



plot(m1)
m1 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Mean + Temp_Min + Temp_Max), 
          random = ~1|id,  data = subsetD(iem, !pre), method = "ML")


library(visreg)

summary(m1)


bxplts(value= "p", data= subsetD(iem, !pre))
m1 <- lme((p + 1.6)^(-1.1515) ~ co2*(Moist + Temp_Mean + Temp_Min + Temp_Max), 
          random = ~1|ring/plot,  data = subsetD(iem, !pre))

Anova(m1)


library(Rcmdr)

with(subset(iem, co2 == "elev"), scatter3d(Temp_Min, p2, Temp_Max, fit = "additive", rev =1))

newdat <- expand.grid(Moist = seq(min(iem$Moist), max(iem$Moist), length.out = 10), 
                      Temp_Min = seq(min(iem$Temp_Min), max(iem$Temp_Min), length.out = 10),
                      Temp_Mean = seq(min(iem$Temp_Mean), max(iem$Temp_Mean), length.out = 10),
                      Temp_Max = seq(min(iem$Temp_Max), max(iem$Temp_Max), length.out = 10),
                      co2=c("amb","elev"))


newdat$pred <- predict(m1, newdat, level = 0)
head(newdat)

pr <- predict(m1, )
?predict
intm1 <- intervals(m1)
str(intm1)
m1Est <- data.frame(intm1$fixed)
m1Est



AmbVar <- m1Est[c(1, 3:6), ]
EleVar <- AmbVar + m1Est[c(2, 7:10), ]
  
iem$p2 <- (iem$p + 1.6)^(-1.1515)

theme_set(theme_bw())
p <- ggplot(iem, aes(x = Temp_Min, y = p2, col = co2))
p + geom_point()


str(m1Est)

summary(m1Est)


visreg(m1, xvar = "Temp_Min", by = "co2", overlay = TRUE, print.cond=TRUE)
visreg(m1, xvar = "Temp_Max", trans=tr, by = "co2", overlay = TRUE, print.cond=TRUE)

plot(allEffects(m1, transfor))

a <- allEffects(m1) 
str(a)

range((iem$p + 1.6)^(-1.1515))

range(iem$p)

visreg(m1, xvar = "Temp_Min", by = "co2", overlay = TRUE)



range((iem$p + 1.6)^(-1.1515))

range(iem$p)

str(m1)

tr <- function(x) x^(-1/1.1515)-1.6



visreg(m1, xvar = "Temp_Min", by = "co2", trans = tr, level = 1, overlay = TRUE)


visreg(m1, xvar = "Temp_Max", by = "co2", trans = tr, level = 1, 
       overlay = TRUE, ylim = c(0, 40))
visreg(m1, xvar = "Moist", by = "co2", trans = tr, level = 1, overlay = TRUE)
visreg(m1, xvar = "Temp_Mean", by = "co2", trans = tr, level = 1, overlay = TRUE)


visreg(m1, xvar = "Temp_Min", by = "id", REform = ~1|ID)



visreg(m1, "Temp_Min", by="id", level = 1)

visreg(m1, xvar = "Temp_Max", by = "co2", overlay = TRUE)
visreg(m1, xvar = "Temp_Mean", by = "co2", overlay = TRUE)
visreg(m1, xvar = "Moist", by = "co2", overlay = TRUE)
visreg(m1, by = "co2", overlay = TRUE)
?visreg



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


p <- ggplot(SoilVarMlt, aes(x = time, y = value, shape = variable, col = ring))
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

