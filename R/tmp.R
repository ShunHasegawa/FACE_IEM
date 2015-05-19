load("output//data/Temp.RData")
library(lme4)
library(nlme)
library(plyr)
library(lattice)


tdf <- within(tdf,{
  bc <- block:co2
  bct <- bc:time
})
m1 <- aov(log(p) ~ co2 * time + Error(block/co2/time), data = tdf)
summary(m1)
# If we ignore Time x Block interaction (within CO2), DF 40  is merged to
# residuals in "Within" (40 + 462)
m2 <- aov(log(p) ~ co2 * time + Error(block/co2), data = tdf)
summary(m2)






tdf$timepol10 <- poly(as.numeric(tdf$time), 10)
tdf$timepol9 <- poly(as.numeric(tdf$time), 9)
tdf$date2 <- scale(as.numeric(tdf$date))

fff <- function(x){
  df <- tdf
  df$timepol <- poly(as.numeric(df$time), x)
  m1 <- lmer(log(p) ~ co2 * timepol + (date2|block/co2/plot), data = df)
  return(m1)
}


an <- llply(1:10, fff)
anova(fff(1),
      fff(2), 
      fff(3), 
      fff(4), 
      fff(5), 
      fff(6), 
      fff(7), 
      fff(8), 
      fff(9), 
      fff(10))
lapply(an, AIC)

summary(m1)



tdf$timeN <-  as.numeric(as.character(tdf$time))
xyplot(log(p) ~ as.numeric(date) | plot + block , data=tdf, group=co2)

m2 <- lmer(log(p) ~ co2 + (date2|block) + (1|ring/plot), data = tdf)
m3 <- lmer(log(p) ~ co2 + (1|block) + (1|ring) + (date2|plot), data = tdf)
anova(m1, m2)
summary(m1)
summary(m2)
anova(m2)
library(lmerTest)

summary(m1)
anova(m1)
anova(m2)



str(tdf)

sbdf <- ddply(subsetD(tdf, time == "4"), .(ring, block, co2), summarise, p = mean(p, na.rm = TRUE))
aov3 <- aov(log(p) ~ co2 * time + Error(block/co2), data = sbdf1)
aov2 <- aov(log(p) ~ co2 + time + Error(block), data = sbdf1)
# aov3 <- aov(log(p) ~ co2 + Error(block), data = sbdf1)
# aov4 <- aov(log(p) ~ co2 + Error(block/co2), data = sbdf1)
summary(aov1)
summary(aov2)
summary(aov3)

tdf$bt <- tdf$block:tdf$time

#############

# tdf$plotEN <- with(tdf,interaction(co2,block))
# tdf$dateEN <- with(tdf,interaction(time,plotEN))
# lme2 <- lme(log(p)~block+co2*time,random=list(~1|plotEN,~1|dateEN),data=tdf)
# anova(lme2) # note the altered denDF AND the altered F-value for the time and co2:time terms

# The above is same as:
tdf$bt <- with(tdf, block:time)
tdf$bc <- with(tdf, block:co2)
tdf$bct <- with(tdf, bc:time)
lme3 <- lme(log(p) ~ co2 * time, 
            random = list(~1|block,~1|ring, ~1|bt), data = tdf)
anova(lme3)


lme4 <- lme(log(p) ~ co2 * time, 
            random = list(~1|block, ~1|ring, ~1|bct), data = tdf)
anova(lme4)


tdf$rt <- with(tdf, ring:time)
lme5 <- lme(log(p) ~ co2 * time, 
            random = list(~1|block, ~1|ring, ~1|id, ~1|rt), data = tdf)
lme6 <- lme(log(p) ~ co2 * time, 
            random = list(~1|block, ~1|ring, ~1|rt), data = tdf)
anova(lme5)
anova(lme6)
lme7 <- lme(log(p) ~ co2 * time, random = list(~1|block), data = tdf)
lme8 <- lme(log(p) ~ co2 * time, random = list(~1|block, ~1|id), data = tdf)

# if subplots are randomly located everytime


##########################
tdf <- tdf[, c("time", "block", "co2", "plot", "p")]
save(tdf, file = "output//data/tdf.RData")
str(tdf)
aov1 <- aov(log(p) ~ co2 * time + Error(block/co2/plot), data = tdf)
summary(aov1)
aov2 <- aov(log(p) ~ co2 * time + Error(block/co2/time), data = tdf)
summary(aov2)
###########################


summary(lme5)
aov1 <- aov(log(p) ~ co2 * time + Error(block/co2 + block/co2/time), data = tdf)

aov2 <- aov(log(p) ~ co2 * time + Error(block/co2/plot + block/co2/time), data = tdf)
summary(aov1)
summary(aov2)

aov1 <- aov(log(p) ~ co2 * time + Error(block/co2 + block/co2/plot + block/time), data = tdf)


aov1 <- aov(log(p) ~ co2 * time + Error(block/co2/plot + block/time), data = tdf)
summary(aov1)

# This is fundamentally same as below

# Remove subplots by taking mean for each ring, avoinding pseudo-replication 
# within each ring
sbdf <- ddply(tdf, .(block, time, co2), summarise, p = mean(p, na.rm = TRUE))
xtabs(~block + time, data = sbdf)
aov1 <- aov(log(p) ~ co2 * time + Error(block/co2), data = sbdf)
aov2 <- aov(log(p) ~ co2 * time + Error(block/co2 + block/time), data = sbdf)
summary(aov1)
summary(aov2)
# DF is the same as above. This is how psuedoreplications were treated in the 
# above model. Note that DF for residual for time and co2:time is given in
# Error:Within. This structure is the same as split-plot design, spliting block
# into two rings (amb vs. eCO2), and split each ring into 11 (Time1 - Time11).
# In Error Within: 
  # Total DF is 11 time points x 2 co2 rings x 3 blocks - 1 = 65
  # DF for is block:co2 is 2 rings x 3 blocks - 1 = 5
  # DF for time and time:co2 is 10 + 10 = 20 
  # DF for residuals is 65 - (5 + 20) = 40

# But time is not directly within co2, but actually within subplot within co2 
# within block. In orther words, split ring into 8 subplots, and split subplot 
# into 11 subsubplots. So the number of replicates for time is larger yet plot
# size is smaller than difined in the above models.

# If I add subplot within co2 within block:
aov2 <- aov(log(p) ~ co2 * time + Error(block/co2/plot), data = tdf)
summary(aov2)
# Error: block:co2:plot is added and DF for time and time:co2 is now 460. 
# In Error: Within:
  # Total DF is 11 time points x 8 subplots x 2 co2 rings x 3 block -1 = 527
  # DF for subplot is 8 x 2 x 3 = 47
  # DF for time, co2:time is 10 + 10 = 20
  # So that DF for residuals is 527 - (47 + 20) = 460




# or
aov4 <- aov(log(p) ~ co2 * time + Error(block/co2/plot), data = tdf)
summary(aov4)
# residual for time and co2:time is given in Within subject (i.e. subplot)


aov5 <- lme(log(p) ~ co2 * time, random=list(~1|block,~1|ring, ~1|bt2, ~1|id), data = tdf)
aov6 <- lme(log(p) ~ co2 * time, random=list(~1|block,~1|ring, ~1|id), data = tdf)
anova(aov4)
anova(aov5)
anova(aov6)





















aov1 <- aov(log(p) ~ co2 * time + Error(block/co2/plot), data = tdf)
summary(aov1)




lme2 <- lme(log(p) ~ time, random = ~1|block/time, data = sbdf1)
summary(lme2)

aov1 <- aov(log(p) ~ co2 + Error(block/co2), data = sbdf)
aov2 <- aov(log(p) ~ time + Error(block/time), data = sbdf)


summary(aov2)


lme1 <- lme(log(p) ~ co2 * time, random = ~1|block/co2, data = sbdf)
anova(lme1)


anova(lme1)
summary(lme1)



lmer3 <- lme(log(p) ~ co2, random =~1|block, data = sbdf)
summary(lmer3)
anova(lmer3)

ml1 <- lme(log(p) ~ time * co2, random = ~1|block/ring/id, data = tdf)
anova(ml1)

ml2 <- lme(log(p) ~ time, random = ~1|block/ring/id, data = tdf)





ml2 <- lme(log(p) ~ time * co2, random = list(~1|block, ~1|ring, ~1| ~1|id), data = tdf)
anova(ml2)

tdf$plotEN <- with(tdf,interaction(co2,block))
tdf$dateEN <- with(tdf,interaction(time,plotEN))
lme2 <- lme(log(p)~block+co2*time,random=list(~1|plotEN,~1|dateEN),data=tdf)


lmer1 <- lmer(log(p) ~ co2 * time +  (1|block/co2) + (1|id), data = tdf)
lmer2 <- lmer(log(p) ~ co2 * time +  (1|block/co2) + (1|id), data = tdf)
lmer3 <- lmer(log(p) ~ co2 * time +  (1|block) + (1|ring) + (1|id), data = tdf)

summary(lmer1)
anova(lmer1)
anova(lmer2)
anova(lmer3)


###
tdf <- within(tdf, {
  bc <- block:co2
  bcp <- block:co2:plot
})
m1 <- lmer(log(p) ~ co2 * time + (1|block) +(1|bc) + (1|bcp), data = tdf)
m2 <- lmer(log(p) ~ co2 * time + (1|block) +(1|ring) + (1|id), data = tdf)

anova(m1)
anova(m2)
summary(m2)

###





aov1 <- aov(log(p) ~ co2 * time + Error(block/co2 + block/time), data = tdf)
aov1 <- aov(log(p) ~ co2 * time + Error(block/co2 + block/time + id), data = tdf)
summary(aov1)


anova(ml2)

lmer1 <- lmer(log(p) ~ time + (1|block) + (1|ring) + (1|id), data = tdf)
summary(lmer1)

# to make data frame unbalanced, remove rondom rows
tdf <- some(tdf, n = nrow(tdf) - 20) 

# Fit the same main effects but different orders using lme
ml_co2 <- lme(log(p) ~ time + co2, random = ~1|block/ring/id, data = tdf)
ml_time <- lme(log(p) ~ co2 + time, random = ~1|block/ring/id, data = tdf)

# F test with typeI SS
llply(list(ml_co2, ml_time), anova)
  # F values are slightyly different between the models. it's becuase SS were 
  # sequentially allocated. anova.lme has "type" argument which allows you to
  # choose typeI or typeIII SS, but not typeII for some reasons..

# Fit the same models usign lmer
lmer1 <- lmer(log(p) ~ co2 + time + (1|block) + (1|ring)  + (1|id), data = tdf)
lmer2 <- lmer(log(p) ~ time + co2 + (1|block) + (1|ring)  + (1|id), data = tdf)

# F test with type II SS
llply(list(lmer1, lmer2), function(x) Anova(x, test.statistic = "F"))
  # The results were consistent and the order of main terms doesn't matter.

# F test with type I SS
require(lmerTest)
llply(list(lmer1, lmer2), function(x) anova(x, type = 1, ddf = "Kenward-Roger"))
  # SS and associatd F values differ between the two models. 

# But note that SS and associated F vlue for time in the 1st model and co2 in
# the 2nd models are indentical to typeII SS given by Anova above.
llply(list(lmer1, lmer2), function(x) anova(x, type = 1, ddf = "Kenward-Roger")[2, ])
Anova(lmer1, test.statistic = "F")
# This is how typeII is calculated. The term of interest is alway fit after the
# other main terms, as such order of main terms in a model doen't matter in
# Anova.

# Manually extract typII SS-associated F and P values from lme
llply(list(ml_co2, ml_time), anova)

ml_time <- lme(log(p) ~ co2 + time, random = ~1|block/ring/id, data = tdf, method = "ML")
ml_time2 <- lme(log(p) ~ co2, random = ~1|block/ring/id, data = tdf, method = "ML")
ml <- lm(log(p) ~ co2 + time, data = tdf)
ml2 <- lm(log(p) ~ co2, data = tdf)
anova(ml, ml2, test = "Chisq")

lme2 <- update(lmer1, ~. -time)
anova(ml_time, ml_time2, )
?anova
anova(ml_time)
anova(ml_time2)

# As described above the F value for co2 in the 1st model and for time in the 
# second model should be same as the F values you woud've gotten with typeII SS.
# But these valuse are slightly different than the values given by lmer. It's 
# simply because different method to approximate denominator degrees of freedom 
# is used in Anova and anova. Anova uses "Kenward-Roger" and anova uses
# "Satterthwaite approximation" (don't really know the difference to be
# honest..).

# So let's reculculate F values using Satterthwaite approximateion for each of 
# time and co2. (I don't know how to change this with Anova, so need to run two
# models with typeI SS in order to get TypeII SS for each term)
llply(list(lmer1, lmer2), function(x) anova(x, type = 1, ddf="Satterthwaite")[2, ])

# They are identical to lme results
anova(ml_co2)[3, ]
anova(ml_time)[3, ]
