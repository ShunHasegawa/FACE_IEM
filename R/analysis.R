rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)

source("R/functions.R")

################
# Process  data#
################
iem <- read.csv("Data/FACE_IEM.csv", colClasses=c("ring"="factor","plot"="factor","time"="factor",
                                                  "coverage" = "NULL", "actual.cov" = "NULL"))

# reorder time
levels(iem$time)
iem$time <- factor(iem$time, levels = c(as.character(1:length(levels(iem$time)))))

#unify date for each time
iem$insertion <- as.Date(dmy(iem$insertion))
iem$sampling <- as.Date(dmy(iem$sampling))
iem$date <- as.Date(ave(apply(cbind(iem$insertion, iem$sampling), 1, mean), iem$time), origin = origin) # same date for same time

# change the unit from ug to ng
iem[, c("no", "nh", "p")] <- iem[, c("no", "nh", "p")] * 1000

# add id for later analysis
iem$id <- iem$ring:iem$plot

#save
save(iem,file="output/data/FACE_IEM.RData")

#################
# Summary table #
#################
source("R/SummaryExlTable.R")


########
# Figs #
########
source("R/Figs.R")


##############
# Phosphate #
##############
# pre co2 #

levels(iem2$time)
iem2<-subset(iem,p<max(p))
iem2$time2 <- ifelse(iem2$time %in% c("1","2","3","4"),"pre","post")

model1<-lme(log(p)~time*co2,random=~1|ring/plot,subset=time2=="pre",data=iem2)
anova(model1) 

##auto-correlation
model2<-lme(log(p)~co2*time,random=~1|ring/plot,subset=time2=="pre",data=iem2)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)

#model2.3 looks best
anova(model2.3)
m1 <- ana(model2.3)
fnl.anova <- m1$anova.reml
fnl.anova

# only post co2 #
model1 <- lme(log(p)~time*co2,random=~1|ring/plot,subset=time2=="post",data=iem2)
anova(model1) ##there is a significant interactive effect

##auto-correlation
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)
#model1 looks best
anova(model1)
m1 <- ana(model1)
fnl.model <- m1$model.reml
fnl.anova <- m1$anova.reml
fnl.anova

# contrast and look at each month
levels(iem2$time)[5:14]
contrast(fnl.model,
         a=list(time=levels(iem2$time)[5:14],co2="amb"),
         b=list(time=levels(iem2$time)[5:14],co2="elev"))


#
time3<-iem2$time
levels(time3)[5:8]<-"gr"
levels(time3)[6:7]<-"wint"
levels(time3)
model1<-lme(log(p)~time*co2,random=~1|ring/plot,subset=time2=="post",method="ML",data=iem2)
model1.1<-lme(log(p)~time3*co2,random=~1|ring/plot,subset=time2=="post",method="ML",data=iem2)
anova(model1,model1.1)
model1.2<-update(model1.1,method="REML")
anova(model1.2)
summary(model1.2)
contrast(model1.2,
         a=list(time3=levels(time3)[5:6],co2="amb"),
         b=list(time3=levels(time3)[5:6],co2="elev"))


#############auto correlation
time3<-relevel(time2,"post") #####need postive correlation in order to use contrast function
model5<-lme(log(p)~time3*co2,random=~1|ring/plot,data=iem2)
time4<-as.numeric(iem2$time)
model5.1<-update(model5,correlation=corARMA(q=2,form=~time4))
anova(model5,model5.1)
model6<-update(model5,correlation=corARMA(q=2,form=~1|ring/plot)) ##### R book 2nd p.701
anova(model5.1,model6) 
#->model5.1=model6 -> random~1|ring/plot has already defined time series

#####
#P, post-co2, only Oct to March (difference between the treatment have seen during this period)
m1<-lme(log(p)~time*co2,random=~1|ring/plot,subset=time%in% c("5","6","7","8"),data=iem2)
#auto-correlation
model2.2<-update(m1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(m1,correlation=corARMA(q=2))
model2.4<-update(m1,correlation=corAR1()) 
model2.5<-update(m1,correlation=corARMA(q=1))
anova(m1,model2.2,model2.3,model2.4,model2.5)
#m1 looks the best
anova(m1)
m2 <- update(m1,method="ML")
m3 <- update(m2,~.-time:co2)
anova(m2,m3)
anova(m3)
m4 <- stepAIC(m3)
anova(m4)
  #co2 effects is not significant
plot(allEffects(m4))
####
boxplot(log(p[time %in% c("5","6","7","8")])~co2[time %in% c("5","6","7","8")]:time[time %in% c("5","6","7","8")],data=iem2)
boxplot(p[time %in% c("5","6","7","8")]~co2[time %in% c("5","6","7","8")]:time[time %in% c("5","6","7","8")],data=iem2)
boxplot(sqrt(p[time %in% c("5","6","7","8")])~co2[time %in% c("5","6","7","8")]:time[time %in% c("5","6","7","8")],data=iem2)

#####
##P, post-co2, only Oct to May (difference between the treatment have seen during this period)
summary(iem2)
levels(iem2$time)
m1<-lme(log(p)~time*co2,random=~1|ring/plot,subset=time%in% c("5","6","7","8","9"),data=iem2)
anova(m1)
plot(allEffects(m1))
model2.2<-update(m1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(m1,correlation=corARMA(q=2))
model2.4<-update(m1,correlation=corAR1()) 
model2.5<-update(m1,correlation=corARMA(q=1))
anova(m1,model2.2,model2.3,model2.4,model2.5)
#m1 looks the best
#model simplification from m1
m2 <- ana(m1)
m3 <- m2$model.reml
summary(m3)
#contrast
ordered(as.numeric(iem2$time))

contrast(m1,a=list(time=c("5","6","7","8","9"),co2="amb"),
            b=list(time=c("5","6","7","8","9"),co2="elev"))
anova(m3)



######BACI analysis


time2<-iem2$time
levels(time2)[1:4]<-"pre"
levels(time2)[2:7]<-"post"
levels(time2)
is.factor(iem2$time)
model1<-lme(log(p)~co2*time2,random=~1|time/ring/plot,data=iem2)
anova(model1,type="marginal")
summary(model1)
##not significant as it does not take monthx*co2 interaction into account


###########
# Nitrate #
###########
# contrast between pre and post-co2

par(mfrow=c(2,2))
boxplot(no~co2*time, data = iem)
boxplot(no~co2*time, data = iem)
boxplot(log(no)~co2*time, data = iem)
boxplot(no^(1/3)~co2*time, data = iem)

# power(1/3) looks better
#pre co2
iem$time2 <- factor(ifelse(iem$time %in% as.character(c(1:4)), "pre", "post"))

model1 <- lme((no)^(1/3) ~ time * co2, random = ~1|ring/plot, subset = time2 == "pre", data = iem)
m1 <- ana(model1) 
anova(m1$model.reml)


##auto-correlation
model2<-lme((no)^(1/3)~co2*time,random=~1|ring/plot,subset=time2=="pre",data=iem)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)

# model2.3 looks best
m1 <- ana(model2.3)
m1$model.reml

anova(m1$model.reml)

# post co2 with Sep data as baseline
model1<-lme((no)^(1/3)~time*co2,random=~1|ring/plot,subset = time %in% as.character(c(4:10)),data=iem)
anova(model1)
m1 <- ana(model1)
m1
anova(m1$model.reml)

##auto-correlation
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)
#model2.4 looks best
anova(model2.4)
ana(model2.4)

############
# Ammonium #
############
par(mfrow=c(1,3))
range(iem$nh)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
boxplot(nh~co2*time,data = iem, subset = nh < 0.8 & as.numeric(time) > 2)
boxplot(log(nh)~co2*time,data = iem, subset = nh < 0.8 & as.numeric(time) > 2)
boxplot(sqrt(nh)~co2*time,data = iem, subset = nh < 0.8 & as.numeric(time) > 2)
boxplot(nh^(1/3)~co2*time,data = iem, subset = nh < 0.8 & as.numeric(time) > 2)

# homogeneity of variance may not be met, but may be log looking better
contrasts(time)<-NULL
options(contrasts=c("contr.treatment","contr.poly"))
model1<-lme(log(nh)~co2*time,random=~1|ring/plot,data=postco2)
anova(model3,type="marginal")
model2<-update(model1,method="ML")
model3<-update(model2,~.-time:co2)
anova(model2,model3)
anova(model3,type="marginal")
model4<-update(model3,~.-co2)
anova(model3,model4)
model5<-update(model4,method="REML")
anova(model5,type="marginal")

plot(model5,resid(.,type="p")~fitted(.)|ring:plot) ###check model
qqnorm(model5,~resid(.)|ring:plot)


## pre co2 ##
summary(iem)

model1<-lme(log(nh)~time*co2,random=~1|ring/plot,subset= time %in% c(1:4) ,data=iem)
anova(model1) 

##auto-correlation
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

#model2.5 looks best
m1 <- ana(model2.5)
m1$anova.reml

## post co2 ##
model1<-lme(log(nh)~time*co2,random=~1|ring/plot,subset=time %in% c(4:10),data=iem)
anova(model1)

# auto-correlation
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

#model1 looks best
m1 <- ana(model1)
m1$anova.reml



subset(iem,iem$p==max(iem$p))
################################
##################################
##################################N:P ratio
names(iem2)
np<-with(iem2,(no+nh)/p)

range(np)
par(mfrow=c(2,2))
boxplot(np~co2*time,data=iem2)
boxplot(log(np)~co2*time,data=iem2)
boxplot(sqrt(np)~co2*time,data=iem2)
boxplot(np^(1/3)~co2*time,data=iem2)

time2<-iem2$time
levels(time2)[1:4]<-"pre"
levels(time2)[2:7]<-"post"

model2<-lme(log(np)~time*co2,random=~1|ring/plot,subset=time2=="post",data=iem2)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
anova(model2.4)
            
#############
#############ancova
post<-subset(iem,as.numeric(iem$time)>4)            
names(iem)
cov<-iem$p[iem$time==4]
cov2<-rep(cov,6)
par(mfrow=c(2,2))
boxplot(p~co2:time,data=post)
boxplot(log(p)~co2:time,data=post)
boxplot(sqrt(p)~co2:time,data=post)

model1<-lme(log(p)~time*co2*cov2,random=~1|ring/plot,data=post)
summary(model1)
anova(model1)
mod2<-update(model1,method="ML")
mod3<-update(mod2,~.-time:co2:cov2)
anova(mod2,mod3)
mod4<-update(mod3,~.-time:co2)
mod5<-update(mod3,~.-time:cov2)
mod6<-update(mod3,~.-co2:cov2)
anova(mod3,mod4,mod5,mod6)
#mod5 is best
anova(mod5)
mod7<-update(mod5,~.-co2:cov2)
anova(mod5,mod7)
anova(mod7)
mod8<-update(mod7,~.-time:co2)
anova(mod7,mod8)
##time:co2 cannot be taken out
mod9<-update(mod7,~.-cov2)
anova(mod7,mod9)
#mod9
anova(mod9)
mod10<-update(mod9,method="REML")
anova(mod9)



###############################
# with soil moisture and temp #
###############################
# combine soil moisture data during incubation
# incubation start date

strt.d <- c(as.Date("2012-06-06"), unique(iem$date)[1:9])
iem$srt.day <- rep(strt.d, each = 48)
some(iem)

# soil data
load("C:/Users/30034013/SkyDrive/Documents/PhD.HIE/R/soil.variable/Data/output/ring.means.Rdata")
head(ring.means)

# soil data is from 20th sept 2013 so extract iem data after this
iem.sep <- subset(iem, srt.day > as.Date("2012-09-20"))
iem.sip <- droplevels(iem.sep)

# ring mean of iem
iem.ring <- with(iem.sep, aggregate(iem.sep[c("no", "nh", "p")], 
                                    list(time = time, date = date, srt.day = srt.day, ring = ring, co2 = co2), 
                                    mean, na.rm = TRUE))

head(ring.means)
# mean soil variable for each incubation period
inc.soil.temp.moist.mean <- function(strtD, endD){
  a <- subset(ring.means, ring.means$Date >= strtD & ring.means$Date <= endD)
  d <- with(a, aggregate(a[c("moist", "temp")], list(ring = ring), mean, na.rm = TRUE))
  return(d)
}

SrD <- unique(iem.ring$srt.day)
enD <- unique(iem.ring$date)
  
soilva.ls <- lapply(1:6, function(x) inc.soil.temp.moist.mean(SrD[x], enD[x]))
soilvas <- rbind.fill(soilva.ls)
soilvas$date <- rep(enD, each = 6)

# marge
iem.ring.soil <- merge(iem.ring, soilvas, by = c("ring", "date"))
iem.ring.soil

#
boxplot(iem.ring.soil$p~time:co2, data = iem.ring.soil)
boxplot(log(p)~time:co2, data = iem.ring.soil)

m1 <- lme(log(p) ~ time * co2 + moist + temp, random = ~1|ring, data = iem.ring.soil)
anova(m1)

plot(p ~ moist, data = iem.ring.soil)
plot(no ~ moist, data = iem.ring.soil)
plot(nh ~ moist, data = iem.ring.soil)
plot(p ~ temp, data = iem.ring.soil)
plot(no ~ temp, data = iem.ring.soil)
plot(nh ~ temp, data = iem.ring.soil)

boxplot(iem.ring.soil$nh~time:co2, data = iem.ring.soil)
boxplot(nh ~ time:co2, data = iem.ring.soil)
m1 <- lme(nh ~ time * co2 + moist + temp, random = ~1|ring, data = iem.ring.soil)
anova(m1)
a <stepAIC(update(m1, method = "ML"))

ana(m1)
summary(ana(m1)$model.reml)
