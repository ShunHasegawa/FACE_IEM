load("output//data/Temp.RData")
str(tdf)

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
