## ---- Stat_FACE_IEM_NPRatio_postCO2

#############
# N:P ratio #
#############
# add NP ratio
iem$NP <- with(iem, (no + nh)/p)

## ---- Stat_FACE_IEM_Analyse_NP
############
# NP ratio #
############
bxplts(value= "NP", data= subsetD(iem, post), lambda = seq(-.1, .1, length = 10))
# use log

# The initial model is
Iml_post_NP <- lmer(log(NP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                    data = subsetD(iem, post))
summary(Iml_post_NP)
Anova(Iml_post_NP, test.statistic = "F")
# co2 x time interaction
plot(allEffects(Iml_post_NP))

## ---- Stat_FACE_IEM_Analyse_NP_plot
# model diagnosis
plot(Iml_post_NP)
qqnorm(resid(Iml_post_NP))
qqline(resid(Iml_post_NP))

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

# Response ratio of Rnp ----
NP_ring$logRatio <- log(NP_ring$Rgeo)

# compute SE using bootstrap for each month
diff.means <- function(d, f){ 
  n <- nrow(d)
  gp1 <- 1:table(as.numeric(d$co2))[1]
  m1 <- sum(d[gp1, "logRatio"] * f[gp1])/sum(f[gp1])
  m2 <- sum(d[-gp1, "logRatio"] * f[-gp1])/sum(f[-gp1])
  c(m2 - m1)
}

NP_ring_boot <- ddply(NP_ring, .(date), function(x) {
  x <- x[order(x$co2), ]
  b <- boot(x, diff.means, R = 999, stype = "f", strata = x[, "co2"])
  return(summary(b))
})

p <- ggplot(NP_ring_boot, aes(x = date, y = original))
p2 <- p + 
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
                     linetype = "dashed", col = "black") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line() + 
  geom_errorbar(aes(ymin = original - bootSE, ymax = original + bootSE), width = 0) + 
  geom_point(shape = 21, size = 4, fill = "black") +
  labs(x = "Month", y = expression(log(R[NP_eCO2]/R[NP_amb]))) +
  scale_x_date(breaks= date_breaks("3 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2012-6-15", "2014-3-29"))) +
  science_theme + 
  theme(legend.position = c(.8, .8))
p2
ggsavePP(filename = "output//figs/FACE_ResponseRatio_NPRatio", plot = p2, width = 6.65, height = 3)

# vs temp ----
# merge data frame
MT_df <- ddply(iem, .(date, pre, post), summarise, 
               temp = mean(Temp_Mean, na.rm = TRUE), 
               Moist = mean(Moist, na.rm = TRUE))
dd <- merge(MT_df, NP_ring_boot, by = "date")
p <- ggplot(subset(dd, !pre), aes(x = temp, y = original))
plot_Temp <- p + 
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(size = 4) + 
  geom_smooth(method = lm, colour = "black", se = FALSE, alpha = .7, size = .7) + 
  science_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  annotate("text", x = 19.2, y = .4, label = "italic(R)^2==0.5*0", parse = TRUE, hjust = 0) + 
  annotate("text", x = 19.2, y = .32, label = "italic(P)<0.05", parse = TRUE, hjust = 0) + 
  labs(x = expression(Soil~temperature~(degree*C)), 
       y = expression(log(italic(R[NP_eCO2])/italic(R[NP_amb]))))
plot_Temp
p3 <- arrangeGrob(p2, plot_Temp, nrow = 2)
ggsavePP(filename = "output//figs/FACE_ResponseRatio_NPRatio.pdf", plot = p3, 
         width = 6.65, height = 6.65)
ggsavePP(filename = "output//figs/FACE_manuscript/FACE_ResponseRatio_NPvsTemp", 
         plot = plot_Temp,
         width = 3.14, height = 3.14)

# Stats
m1 <- lm(original ~ temp + Moist, data = dd, subset = !pre)
m2 <- update(m1, ~. - Moist)
anova(m1, m2)
summary(m2)
anova(m2)
########
plot(original ~ temp, type = "n", ylim = c(-.7, 1.5), data = dd)
points(original ~ temp, pch = 19, cex = 2, ylim = c(-.7, 1.5), 
     data = dd, subset = co2on)
abline(lm(original ~ temp, data = dd))
abline(h = 0, lty = 2, col = "red")


# preco2
pre <- subset(NP_ring, date < as.Date("2012-9-20"))
pre_ring <- ddply(pre, .(ring, co2), summarise, Rgeo = gm_mean(Rgeo))
pre_ring$logRatio <- log(pre_ring$Rgeo)
preNP_ring_boot <- boot(pre_ring, diff.means, R = 999, stype = "f", 
                        strata = pre_ring[, "co2"])

# geometric mean and se
dd <- merge(NP_co2_Raio, Rio_CI)
plot(log(Rgeo) ~ date, data = dd, pch = 19, cex = 2, ylim = c(-1, 2))
with(dd, arrows(date, log(Rgeo) - log(SE), date, log(Rgeo) + log(SE), length = .1, angle = 90, code = 3))
abline(h = 0, lty = 2, col = "red")
abline(v = as.numeric(as.Date("2012-9-18")), lty = 2)

##














# ratio for each block




NP_co2_Raio_block <- ddply(NP_ring, .(date, block), 
                     summarise, Rgeo = Rgeo[co2 == "elev"]/Rgeo[co2 == "amb"])

temp_block <- ddply(iem, .(date, block), summarise, 
                    temp = mean(Temp_Mean, na.rm = TRUE), 
                    Moist = mean(Moist, na.rm = TRUE))

dd2 <- merge(NP_co2_Raio_block, temp_block, by = c("date", "block"))
dd2 <- subset(dd2, date > as.Date("2012-9-18"))
xyplot(log(Rgeo) ~ temp|block, dd2, type = c("r", "p"))
xyplot(log(Rgeo) ~ Moist|block, dd2, type = c("r", "p"))

mmmm <- lmer(log(Rgeo) ~ temp + (1|block), data = dd2)
r.squared(mmmm)
summary(mmmm)
Anova(mmmm, test.statistic = "F")
plot(allEffects(mmmm))
visreg(mmmm, "temp", by = "Moist")
visreg(mmmm, "temp", by = "block")
?visreg
r.squared(mmmm)

plot(log(Rgeo) ~ temp, data = dd2, subset = date > as.Date("2012-9-18"))



# geometric mean for co2 treatment
NP_co2_Raio <- ddply(NP_co2_Raio_block, .(date), summarise, Rgeo = gm_mean(Rgeo))
# confidence interval
Rio_CI <- ddply(NP_co2_Raio_block, .(date), function(x) {
  data.frame(lci = geoCI(x$Rgeo)[1], uci = geoCI(x$Rgeo)[2], SE = geoCI(x$Rgeo)[3])
  })

geoCI <- function(x) exp(ci(log(x))[c(2, 3, 4)])

dd <- merge(NP_co2_Raio, Rio_CI)
plot(log(Rgeo) ~ date, data = dd, pch = 19, cex = 2, ylim = c(-1, 2))
with(dd, arrows(date, log(Rgeo) - log(SE), date, log(Rgeo) + log(SE), length = .1, angle = 90, code = 3))
abline(h = 0, lty = 2, col = "red")
abline(v = as.numeric(as.Date("2012-9-18")), lty = 2)
preMean <- mean(log(dd$Rgeo[dd$date < as.Date("2012-9-18")]))
abline(h = preMean, lty = 4, lwd = 2)

NP_co2_Raio_block$time <- as.factor(NP_co2_Raio_block$date)

plot(log(NP_co2_Raio_block$Rgeo))
tdf <- subset(NP_co2_Raio_block, date < as.Date("2012-9-18") & date > as.Date("2012-08-1"))
NP_co2_Raio_block_Month <- ddply(tdf, .(block), summarise,  Rgeo = gm_mean(Rgeo))
ci(log(NP_co2_Raio_block_Month$Rgeo[1:3]), confidence = .9)

log()


geoCI(NP_co2_Raio_block_Month$Rgeo)
exp(sd(log(NP_co2_Raio_block_Month$Rgeo))/sqrt(3))


gm_mean(NP_co2_Raio_block_Month$Rgeo)


ll <- ci(log(NP_co2_Raio_block_Month$Rgeo))[4]
abline(h = preMean + ll, lty = 3, lwd = 2)
abline(h = preMean - ll, lty = 3, lwd = 2)

ci(ml)
summary(ml)
anova(ml)
par(mfrow = c(1, 1))
plot(Rgeo ~ date, data = NP_co2_Raio, pch = 19, cex = 2)
abline(h = 1, lty = 2, col = "red")
abline(v = as.numeric(as.Date("2012-9-18")), lty = 2)

# arithmetric mean
NP_ring <- ddply(iem, .(date, block, co2, ring), 
                 function(x) Crt_SmryDF(data = x, val = "NP"))
# co2 mean
NP_co2 <- ddply(NP_ring, .(date, co2), function(x) Crt_SmryDF(data = x, val = "Mean"))
plot(Mean ~ date, data = NP_co2, col = co2, pch = 19, cex = 2, ylim = c(0, 300))
with(NP_co2, arrows(date, Mean + SE, date, Mean - SE, code = 3, angle = 90, length = .1, col = co2))
abline(v = as.numeric(as.Date("2012-9-18")), lty = 2)

par(mfrow = c(1, 2))
dd <- ddply(NP_co2, .(date), summarise, R = Mean[co2 == "elev"]/Mean[co2 == "amb"])
plot(R ~ date, data = dd, pch = 19, cex = 2)
abline(h = 1, lty = 2)


ddm <- merge(temp_M, dd, by = "date")
ddm <- subset(ddm, date > as.Date("2012-9-18"))
plot(log(Rgeo) ~ temp, data = ddm, pch = 19, cex = 2)
abline(coef(mm <- lm(log(Rgeo) ~ temp, data = ddm)))
summary(mm)
par(mfrow = c(2,2))
plot(mm)
# ratio NPelev/NPamb for each block
head(NP_ring)
NP_Co2Ratio <- ddply(NP_ring, .(date, block), summarise, R = log(Mean[co2 == "elev"]/Mean[co2 == "amb"]))
NP_Co2Ratio_Mean <- ddply(NP_Co2Ratio, .(date), function(x) Crt_SmryDF(data = x, val = "R"))
plot(Mean ~ date, data = NP_Co2Ratio_Mean, pch = 19, cex = 2, ylim = c(0, 6))
plot(exp(Mean) ~ date, data = NP_Co2Ratio_Mean, pch = 19, cex = 2)

plot(Mean ~ date, data = NP_Co2Ratio_Mean, pch = 19, cex = 2, ylim = c(-1, 2))
with(NP_Co2Ratio_Mean, arrows(date, Mean + SE, date, Mean - SE, code = 3, angle = 90, length = .1))
abline(h = 0, lty = 2, col = "red")
abline(v = as.numeric(as.Date("2012-9-18")), lty = 2)


names(iem)
p <- ggplot(data, aes_string(x = "date", y = "Mean", col = colfactor))

p2 <- p + geom_line(size = 1, position = position_dodge(10), alpha = .8) + 
  geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = colfactor), 
                width = 15,
                position = position_dodge(10),
                , alpha = .8) + 
  labs(x = "Month", y = ylab) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed", col = "black") +
  scale_x_date(breaks= date_breaks("2 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2012-6-15", "2014-4-2"))) +
  theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))







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
