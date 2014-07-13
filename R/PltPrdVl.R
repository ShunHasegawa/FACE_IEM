Data <- data.frame(X=runif(100,0,100))
Data$Y <- Data$X + rnorm(100, mean=0, sd=10)
Data$g <- rep(letters[1:10],each=10)      
lmefit <- lmer(Y ~ X + (1|g),
               data=Data)
Data$Ypred <- predict(lmefit,Data,re.form=NA)
bb2 <- bootMer(lmefit,
              FUN=function(x)predict(x, re.form=NA),
              nsim=500)
str(bb2)
head(bb$t)
lci <- apply(bb$t, 2, quantile, 0.025)
uci <- apply(bb$t, 2, quantile, 0.975)
head(lci)
#----

Iml_ancv <- lmer(log(p) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
# random factor needs to be coded separatedly as above to use "step"
Anova(Iml_ancv)

# model simplification
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv, test.statistic = "F")


predval <- predict(Fml_ancv, 
                   postDF[, c("block", "co2","ring", "id", "Moist", "Temp_Mean")])
predval <- predict(Fml_ancv)


newDF <- postDF
newDF$Moist <- median(newDF$Moist) 
newDF$Temp_Mean <- median(newDF$Temp_Mean) - 5

bb <- bootMer(Fml_ancv,
              FUN=function(x) predict(x, newDF, re.form = NA),
              nsim=500)
lci <- apply(bb$t, 2, quantile, 0.025)
uci <- apply(bb$t, 2, quantile, 0.975)

newDF2 <- cbind(postDF, lci, uci)


head(nesDF)
theme_set(theme_bw())
p <- ggplot(newDF2, aes(x = Temp_Mean, y = p, col = co2, fill = co2))
pl <- p + geom_ribbon(aes(ymin = exp(lci), ymax = exp(uci)), alpha = .3) +
  geom_point() +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~treatment)) +
  scale_fill_manual(values = c("blue", "red"), expression(CO[2]~treatment))
p <- ggplot(newDF2, aes(x = Temp_Mean, y = log(p), col = co2, fill = co2))
pl <- p + geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .3) +
  geom_point() +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~treatment)) +
  scale_fill_manual(values = c("blue", "red"), expression(CO[2]~treatment))
pl


p <- ggplot(newDF2, aes(x = Moist, y = p, col = co2, fill = co2))
pl <- p + 
  geom_point() +
  geom_ribbon(aes(ymin = exp(lci), ymax = exp(uci)), alpha = .5) +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~treatment)) +
  scale_fill_manual(values = c("blue", "red"), expression(CO[2]~treatment))

p <- ggplot(newDF2, aes(x = Moist, y = log(p), col = co2, fill = co2))
pl <- p + 
  geom_point() +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5) +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~treatment)) +
  scale_fill_manual(values = c("blue", "red"), expression(CO[2]~treatment))
pl




expDF <- with(iem, expand.grid(ring = unique(ring), 
                               plot = unique(plot),
                               Moist = seq(min(Moist, na.rm = TRUE), max(Moist, na.rm = TRUE), length.out= 100),
                               Temp_Mean = median(Temp_Mean, na.rm = TRUE)))

# temperature and give moist
# expDF <- with(iem, expand.grid(ring = unique(ring), 
#                                plot = unique(plot),
#                                Temp_Mean = seq(min(Temp_Mean, na.rm = TRUE), max(Temp_Mean, na.rm = TRUE), length.out= 100),
#                                Moist = median(Moist, na.rm = TRUE)))
expDF <- within(expDF, {
  block = recode(ring, "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
  id = ring:plot
  co2 = factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
})


bb3 <- bootMer(Fml_ancv,
              FUN=function(x) predict(x, expDF, re.form = NA),
              nsim=500)
lci <- apply(bb3$t, 2, quantile, 0.025)
uci <- apply(bb3$t, 2, quantile, 0.975)



nesDF <- cbind(expDF, lci, uci)
head(nesDF)

p <- ggplot(postDF, aes(x = Moist, y = log(p), col = co2))
pl <- p + geom_ribbon(data = nesDF, (x = Moist, ymin = lci, ymax = uci))


pl





