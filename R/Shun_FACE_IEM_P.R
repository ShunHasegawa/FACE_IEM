###############################
# plot against soil variables #
###############################

# plot all variables
scatterplotMatrix(~ I((p + 1.6)^(-1.1515)) + Moist + Temp_Max + Temp_Min 
                  + Temp_Mean + I(Temp_Max * Moist), data = df)
# Temp_Max may be positively correlated

# plot for each plot against soil variables
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring + plot, subsetD(iem, !pre), type = c("r", "p")))

# co2  x time
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | co2, subsetD(iem, !pre), type = c("r", "p"), 
             panel = panel.superpose, groups = time))

# ring  x time
print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring , subsetD(iem, !pre), type = c("r", "p"), 
             panel = panel.superpose, groups = time))

print(xyplot((p + 1.6)^(-1.1515) ~ Temp_Max | ring , subsetD(iem, !pre), type = c("r", "p"), 
             panel = panel.superpose, groups = id))

############
# Analysis #
############
m1 <- lme((p + 1.6)^(-1.1515) ~ co2 * time * (Moist + Temp_Max), 
          random = ~1|ring/plot,  data = df)

Anova(m1)
m2 <- update(m1, method = "ML")
m3 <- stepAIC(m2)
Anova(m3)
fml <- update(m3, method = "REML")
Anova(fml)

# model diagnosis
qqnorm(fml, ~ resid(.)|id)
qqnorm(residuals.lm(fml))
qqline(residuals.lm(fml))

################
# main effects #
################
plot(allEffects(fml))

# reverse transormation
ReTrf <- function(x) x^(-1/1.1515)-1.6

# plot predicted value
par(mfrow = c(3, 4))
for (i in levels(df$time)){
  visreg(m3, 
         xvar = "Temp_Max", 
         by = "co2", 
         trans = ReTrf, 
         level = 1, # take random factor into accound
         overlay = TRUE, 
         print.cond=TRUE, 
         cond = list(time = i),
         line.par = list(col = c("blue", "red")),
         main = paste("Time =", i),
         legend = FALSE, 
         ylim = c(0, 15))
  lines(x = range(iem$Temp_Max[iem$time == i]), y = c(0, 0), lwd = 2)
}
legend("topright", leg = c("amb", "elev", "Temp range"), col = c("blue", "red", "black"), 
       lty = 1, lwd = 2, bty="n")
