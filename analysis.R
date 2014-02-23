rm(list=ls(all=TRUE))

#library
source("functions/list_library.R")
(.packages())

#function for model simplification "ana"
source("functions/model_simplification.R")

iem<-read.table("Data/r.iem2.txt",header=T,colClasses=c("ring"="factor","plot"="factor","time"="factor"))

#unify date for each time
iem$date<-as.Date(dmy(as.character(iem$date)))
iem$date<-ave(iem$date,iem$time,FUN=mean) #same time = same date

#remove coverage columns
iem <- iem[,-c(5,6)]

# reorder time
iem$time <- factor(iem$time, levels = c(as.character(1:10)))

#save
save(iem,file="output/data/iem.R")

##############
# Phosphate #
##############

#####
#pre co2
#####
levels(iem2$time)
iem2$time2<-ifelse(iem2$time %in% c("1","2","3","4"),"pre","post")
model1<-lme(log(p)~time*co2,random=~1|ring/plot,subset=time2=="pre",data=iem2)
anova(model1,type="marginal") 
##auto-correlation
model2<-lme(log(p)~co2*time,random=~1|ring/plot,subset=time2=="pre",data=iem2)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2.3 looks best
anova(model2.3)
model3<-update(model2.3,method="ML")
model4<-update(model3,~.-co2:time)
model5<-update(model4,~.-co2)
anova(model3,model4,model5)
anova(model5)
model6<-update(model5,method="REML")
anova(model6)


#####
#only post co2
#####
model1<-lme(log(p)~time*co2,random=~1|ring/plot,subset=time2=="post",data=iem2)
anova(model1,type="marginal") ##there is a significant interactive effect
##auto-correlation
model2<-lme(log(p)~co2*time,random=~1|ring/plot,subset=time2=="post",data=iem2)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2 looks best
anova(model2)
model3<-update(model2,method="ML")
model4<-update(model3,~.-co2:time)
anova(model3,model4)
#model3 is better
summary(model2)
anova(model2,type="marginal")

levels(iem2$time)[5:10]
contrast(model2,
         a=list(time=levels(iem2$time)[5:10],co2="amb"),
         b=list(time=levels(iem2$time)[5:10],co2="elev"))

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
ordered(as.numeric(iem2$time)))

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


#############################################################
#############################################################no contrast between pre and post-co2
detach(iem2)

length(time)
boxplot(no~co2*time)
boxplot(no~co2)
boxplot(no~co2*time)
boxplot(log(no)~co2*time)
boxplot(no^(1/3)~co2*time)

range(no)

boxplot(no~time)
plot(tapply(no,time,mean),tapply(no,time,var))
plot(tapply(log(no),time,mean),tapply(log(no),time,var))
plot(tapply(sqrt(no),time,mean),tapply(sqrt(no),time,var))
plot(tapply(no^(1/3),time,mean),tapply(no^(1/3),time,var))

par(mfrow=c(2,2))
plot(tapply(no,list(time,co2),mean),tapply(no,list(time,co2),var))
plot(tapply(log(no),list(time,co2),mean),tapply(log(no),list(time,co2),var))
plot(tapply(sqrt(no),list(time,co2),mean),tapply(sqrt(no),list(time,co2),var))
plot(tapply(no^(1/3),list(time,co2),mean),tapply(no^(1/3),list(time,co2),var))

####power(1/3) looks better
model1<-lme((no)^(1/3)~time*co2,random=~1|ring/plot,data=iem)
summary(model1)
anova(model1,type="marginal") 
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

#compare auto-correlation
model2<-lme(no^(1/3)~time*co2,random=~1|ring/plot,data=iem)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2.3 looks best
anova(model2.3)
mod3<-update(model2.3,method="ML")
mod4<-update(mod3,~.-time:co2)
anova(mod3,mod4)
mod5<-update(mod4,~.-co2)
anova(mod4,mod5)

mod6<-update(mod5,method="REML")
summary(mod6)

TukeyHSD(aov(mod6))

####################################pre co2
time2<-iem$time
levels(time2)[1:4]<-"pre"
levels(time2)[2:7]<-"post"
levels(time2)
model1<-lme((no)^(1/3)~time*co2,random=~1|ring/plot,subset=time2=="pre",data=iem)
anova(model1,type="marginal") 
##auto-correlation
model2<-lme((no)^(1/3)~co2*time,random=~1|ring/plot,subset=time2=="pre",data=iem)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2.3 looks best
anova(model2.3)
model3<-update(model2.3,method="ML")
model4<-update(model3,~.-co2:time)
model5<-update(model4,~.-co2)
anova(model3,model4,model5)
anova(model5)
model6<-update(model5,method="REML")
anova(model6)
####################################only post co2
model1<-lme((no)^(1/3)~time*co2,random=~1|ring/plot,subset=time2=="post",data=iem)
anova(model1,type="marginal")
##auto-correlation
model2<-lme((no)^(1/3)~co2*time,random=~1|ring/plot,subset=time2=="post",data=iem)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2.4 looks best
anova(model2.4)
model3<-update(model2.4,method="ML")
model4<-update(model3,~.-co2:time)
model5<-update(model4,~.-co2)
anova(model3,model4,model5)
#model5
anova(model5)
model6<-update(model5,method="REML")
anova(model5)

###################################################
###################################################nh contrast
par(mfrow=c(1,3))
range(nh)
plot(iem$nh)
postco2<-subset(iem,nh<0.8)###exclude outlires
par(mfrow=c(2,2))
boxplot(nh~co2*time,data=postco2)
boxplot(log(nh)~co2*time,data=postco2)
boxplot(sqrt(nh)~co2*time,data=postco2)
boxplot(nh^(1/3)~co2*time,data=postco2)

with(postco2,plot(tapply(nh,list(time,co2),mean),tapply(nh,list(time,co2),var)))
with(postco2,plot(tapply(log(nh),list(time,co2),mean),tapply(log(nh),list(time,co2),var)))
with(postco2,plot(tapply(sqrt(nh),list(time,co2),mean),tapply(sqrt(nh),list(time,co2),var)))
with(postco2,plot(tapply(nh^(1/3),list(time,co2),mean),tapply(nh^(1/3),list(time,co2),var)))

###########homogeneity of variance may not be met
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

#############################
####################################pre co2
time2<-iem$time
levels(time2)[1:4]<-"pre"
levels(time2)[2:7]<-"post"
levels(time2)
model1<-lme(log(nh)~time*co2,random=~1|ring/plot,subset=time2=="pre",data=iem)
anova(model1,type="marginal") 
##auto-correlation
model2<-lme(log(nh)~co2*time,random=~1|ring/plot,subset=time2=="pre",data=iem)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2.5 looks best
anova(model2.5)
model3<-update(model2.5,method="ML")
model4<-update(model3,~.-co2:time)
model5<-update(model4,~.-co2)
anova(model3,model4,model5)
anova(model5)
model6<-update(model5,method="REML")
anova(model6)
####################################only post co2
model1<-lme(log(nh)~time*co2,random=~1|ring/plot,subset=time2=="post",data=iem)
anova(model1,type="marginal")
##auto-correlation
model2<-lme(log(nh)~co2*time,random=~1|ring/plot,subset=time2=="post",data=iem)
model2.2<-update(model2,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model2,correlation=corARMA(q=2))
model2.4<-update(model2,correlation=corAR1()) 
model2.5<-update(model2,correlation=corARMA(q=1))
anova(model2,model2.2,model2.3,model2.4,model2.5)
#model2 looks best
anova(model2)
model3<-update(model2.2,method="ML")
model4<-update(model3,~.-co2:time)
model5<-update(model4,~.-co2)
anova(model3,model4,model5)
#model5
anova(model5)
model6<-update(model5,method="REML")
anova(model6)


##############################scatter plot
library(gmodels)
jpeg(file="IEM.scatterplot.time10.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,1),mar=c(3,9.5,2,1), oma=c(0,0,0,0))
#########no.graph
no.sum<-with(iem,aggregate(no,list(ring=ring,time=time,co2=co2),mean))
names(no.sum)[4]<-"no"
xv<-as.numeric(no.sum$time)
means<-with(no.sum,tapply(no,list(xv,co2),mean))
ses<-with(no.sum,tapply(no,list(xv,co2),function(x) ci(x)[4]))
plot(no~xv,type="n",ylim=c(0,max(pretty(means+ses))),
axes=F,xlab="",ylab="",no.sum)
points(means[,1]~unique(xv),pch=16,type="b",lwd=2,cex=2.5)
arrows(unique(xv),means[,1]-ses[,1],unique(xv),means[,1]+ses[,1],
code=3,angle=90,len=0.1)
points(means[,2]~unique(xv),pch=1,type="b",lwd=2,lty=2,cex=2.5)
arrows(unique(xv),means[,2]-ses[,2],unique(xv),means[,2]+ses[,2],
code=3,angle=90,len=0.1)
axis(2,las=1,cex.axis=2)
box(bty="o")
legend("topright",leg=c("Amb",expression(eCO[2])),lwd=2,lty=c(1,2),
pch=c(16,1),bty="n",cex=2)
mtext(2,text=expression(N-NO[3]^"-"~(mu*g~cm^-2~day^-1)),line=5.5,cex=1.5)
lines(c(4.5,4.5),c(-1,1.5),lty=2)
#########nh.graph
nh.sum<-with(iem,aggregate(nh,list(ring=ring,time=time,co2=co2),mean))
names(nh.sum)[4]<-"nh"
nh.sum
xv<-as.numeric(nh.sum$time)
means<-with(nh.sum,tapply(nh,list(xv,co2),mean))
ses<-with(nh.sum,tapply(nh,list(xv,co2),function(x) ci(x)[4]))
plot(nh~xv,type="n",ylim=c(0,max(pretty(means+ses))),
axes=F,xlab="",ylab="",nh.sum)
points(means[,1]~unique(xv),pch=16,type="b",lwd=2,cex=2.5)
arrows(unique(xv),means[,1]-ses[,1],unique(xv),means[,1]+ses[,1],
code=3,angle=90,len=0.1)
points(means[,2]~unique(xv),pch=1,type="b",lwd=2,lty=2,cex=2.5)
arrows(unique(xv),means[,2]-ses[,2],unique(xv),means[,2]+ses[,2],
code=3,angle=90,len=0.1)
axis(2,las=1,cex.axis=2)
box(bty="o")
mtext(2,text=expression(N-NH[4]^"+"~(mu*g~cm^-2~day^-1)),line=5.5,cex=1.5)
lines(c(4.5,4.5),c(-1,1.5),lty=2)
#########p.graph
iem2<-subset(iem,p<max(p))###exclude outlire
p.sum<-with(iem2,aggregate(p,list(ring=ring,time=time,co2=co2),mean))
names(p.sum)[4]<-"p"
xv<-as.numeric(p.sum$time)
means<-with(p.sum,tapply(p,list(xv,co2),mean))
ses<-with(p.sum,tapply(p,list(xv,co2),function(x) ci(x)[4]))
plot(p~xv,type="n",ylim=c(0,max(pretty(means+ses))),
axes=F,xlab="",ylab="",p.sum)
points(means[,1]~unique(xv),pch=16,type="b",lwd=2,cex=2.5)
arrows(unique(xv),means[,1]-ses[,1],unique(xv),means[,1]+ses[,1],
code=3,angle=90,len=0.1)
points(means[,2]~unique(xv),pch=1,type="b",lwd=2,lty=2,cex=2.5)
arrows(unique(xv),means[,2]-ses[,2],unique(xv),means[,2]+ses[,2],
code=3,angle=90,len=0.1)
axis(2,las=1,cex.axis=2)
box(bty="o")
axis(1,at=unique(iem2$time),lab=c("Jun","Jul","Aug","Sep","Oct","Nov","D&J","F&M","A&M","J&J"),
padj=1,mgp=c(0,0,0),cex.axis=2)
mtext(2,text=expression(P-PO[4]^"3-"~(mu*g~cm^-2~day^-1)),line=5.5,cex=1.5)
text(7,0.007,"*",cex=3)
lines(c(4.5,4.5),c(-1,1),lty=2)

dev.off()

#############graph.iem for each of nitrate, ammonium, phosphate
###############################no.graph
jpeg(file="NO.IEM.time7.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,2,1), oma=c(0,0,0,0))
for (i in 1:6)
{
xv<-as.numeric(time)
xv2<-as.numeric(levels(time))
means<-tapply(no,list(ring,xv),mean)
ses<-tapply(no,list(ring,xv),function(x) ci(x)[4])
plot(no~xv,type="n",ylim=c(0,max(pretty(means+ses))),axes=F,xlab="",ylab="")
points(means[i,],pch=1,type="b",lwd=1,cex=2.5)
arrows(xv2,means[i,]-ses[i,],xv2,means[i,]+ses[i,],code=3,angle=90,len=0.05)
axis(1,at=xv2,lab=F)
axis(2,las=1,at=seq(0,3,0.5),cex.axis=2)
lv<-seq(0,3,0.5)
sapply(1:6,function (x) abline(lv[x],0,col="gray"))
mtext(paste("Ring",i),3,line=0.5,cex=1.5)
box(bty="o")
lines(c(4.5,4.5),c(0,2.5),lty=2)
if(i==1)
      mtext(2,text=expression(N-NO[3]^"-"~mu*g/cm^2/day),line=4,cex=1.5)
if(i==6)
        axis(1,at=xv2,lab=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec.Jan"),cex.axis=2)
}
dev.off()
###############################nh.graph
#############

jpeg(file="NH.IEM.time7.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,2,1), oma=c(0,0,0,0))
for (i in 1:6)
{
xv<-as.numeric(time)
xv2<-as.numeric(levels(time))
means<-tapply(nh,list(ring,xv),mean)
ses<-tapply(nh,list(ring,xv),function(x) ci(x)[4])
plot(nh~xv,type="n",ylim=c(0,max(pretty(means+ses))),axes=F,xlab="",ylab="")
points(means[i,],pch=1,type="b",lwd=1, cex=2.5)
arrows(xv2,means[i,]-ses[i,],xv2,means[i,]+ses[i,],code=3,angle=90,len=0.05)
axis(1,at=xv2,lab=F,cex.axis=1)
axis(2,las=1,at=seq(0,0.5,0.1), cex.axis=2)
lv<-seq(0,0.5,0.1)
sapply(1:length(lv),function (x) abline(lv[x],0,col="gray"))
mtext(paste("Ring",i),3,line=0.5,cex=1.5)
box(bty="o")
if(i==1)
      mtext(2,text=expression(N-NH[4]^"+"~mu*g/cm^2/day),line=4,cex=1.5)
if(i==6)
        axis(1,at=xv2,lab=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec.Jan"),cex.axis=2)
}
dev.off()
######P
max(means+ses)
jpeg(file="P.IEM.time7.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,2,1), oma=c(0,0,0,0))
for (i in 1:6)
{
xv<-as.numeric(time)
xv2<-as.numeric(levels(time))
means<-tapply(p,list(ring,xv),mean)
ses<-tapply(p,list(ring,xv),function(x) ci(x)[4])
plot(p~xv,type="n",ylim=c(0,max(pretty(means+ses))+0.005),axes=F,xlab="",ylab="")
points(means[i,],pch=1,type="b",lwd=1,cex=2.5)
arrows(xv2,means[i,]-ses[i,],xv2,means[i,]+ses[i,],code=3,angle=90,len=0.05)
axis(1,at=1:length(xv2),lab=F)
axis(2,las=1,at=seq(0,0.025,0.01),labels=c(seq(0,0.025,0.01)),cex.axis=2)
lv<-seq(0,0.02,0.01)
sapply(1:length(lv),function (x) abline(lv[x],0,col="gray"))
abline(0.025,0,lty=2,cex=1.5)
mtext(paste("Ring",i),3,line=0.5,cex=1.5)
box(bty="o")
if(i==1)
      mtext(2,text=expression(P-PO[4]^"3-"~mu*g/cm^2/day),line=4.5,cex=1.5)
if(i==3)
        points(1,0.027,cex=2)
if(i==3)
        text(1,0.027,"0.37",adj=-0.2,cex=2)
if(i==6)
        axis(1,at=xv2,lab=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec.Jan","F,M","A,M","J,J"),cex.axis=2)
}
dev.off()


detach(iem2)

############################################## barplot
#######################################no.graph
detach(iem2)
attach(iem)
jpeg(file="IEM.CO2.time7.jpg",quality=100,height=300,width=1200)
par(mfrow=c(1,3),mar=c(3,8.5,2,1), oma=c(0,0,0,0))
xv<-as.numeric(iem$time)
means<-with(iem,t(tapply(no,list(xv,co2),mean)))
ses<-with(iem,t(tapply(no,list(xv,co2),function(x) ci(x)[4])))
xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),
beside=T,axes=F,axisnames=F,axis.lty=2,legend.text=F,col=c("gray80","gray30"))
arrows(xs,means-ses,xs,means+ses,code=3,angle=90,len=0.05)
axis(2,las=1,cex.axis=2)
box(bty="o")
axis(1,at=apply(xs,2,median),lab=c("Jun","Jul","Aug","Sep","Oct","Nov","D&J"),
padj=1,mgp=c(0,0,0),cex.axis=2)
legend("topright",leg=c("Amb","eCO2"),fill=c("gray80","gray30"),
col=c("gray80","gray30"),bty="n",cex=2)
mtext(2,text=expression(N-NO[3]^"-"~mu*g/cm^2/day),line=4.5,cex=1.5)
lines(c(12.5,12.5),c(0,1.4),lty=2)
##nh
means<-with(iem,t(tapply(nh,list(xv,co2),mean)))
ses<-with(iem,t(tapply(nh,list(xv,co2),function(x) ci(x)[4])))
xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),
beside=T,axes=F,axisnames=F,axis.lty=2,legend.text=F,col=c("gray80","gray30"))
arrows(xs,means-ses,xs,means+ses,code=3,angle=90,len=0.05)
axis(2,las=1,cex.axis=2)
box(bty="o")
axis(1,at=apply(xs,2,median),lab=c("Jun","Jul","Aug","Sep","Oct","Nov","D&J"),
padj=1,mgp=c(0,0,0),cex.axis=2)
mtext(2,text=expression(N-NH[4]^"+"~mu*g/cm^2/day),line=4.5,cex=1.5)
lines(c(12.5,12.5),c(0,1.4),lty=2)
##p
xv<-as.numeric(iem2$time)
means<-with(iem2,t(tapply(p,list(xv,co2),mean)))
ses<-with(iem2,t(tapply(p,list(xv,co2),function(x) ci(x)[4])))
xs<-barplot(means,ylim=c(0,max(pretty(means+ses))+0.003),
beside=T,axes=F,axisnames=F,axis.lty=2,legend.text=F,col=c("gray80","gray30"))
arrows(xs,means-ses,xs,means+ses,code=3,angle=90,len=0.05)
axis(2,las=1,at=c(0,0.01),lab=c(0,0.01),cex.axis=2)
box(bty="o")
axis(1,at=apply(xs,2,median),lab=c("Jun","Jul","Aug","Sep","Oct","Nov","D&J"),
padj=1,mgp=c(0,0,0),cex.axis=2)
mtext(2,text=expression(P-PO[4]^"3-"~mu*g/cm^2/day),line=4.5,cex=1.5)
abline(0.01,0,lty=2)
points(xs[1,1],0.012,cex=2)
text(xs[1,1],0.012,"0.37",adj=-0.2,cex=2)
lines(c(12.5,12.5),c(0,1.4),lty=2)
dev.off()

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

