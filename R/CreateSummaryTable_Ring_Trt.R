names(iem)
summary(iem)
boxplot(iem$p)

#remove P outlier
iem2<-iem
iem2$p[which(iem2$p==max(iem2$p))]<-NA
summary(iem2)

#ring mean
names(iem2)
ring.mean<-with(iem2,aggregate(iem2[,c(6:8)],list(time=time,date=date,ring=ring,co2=co2),mean,na.rm=TRUE))
summary(ring.mean)
contents(ring.mean)
head(ring.mean)
#write.table(ring.mean,"ring.mean.csv",sep=",")

#co2 mean
names(ring.mean)
summary(ring.mean)
str(ring.mean)
contents(ring.mean)
treat.mean<-summaryBy(no+nh+p~time+date+co2,FUN=c(mean,function(x) ci(x)[4],length),
                      fun.names=c("mean","SE","Sample_size"),data=ring.mean)
head(treat.mean)
contents(treat.mean)
#write.table(treat.mean,"IEM.co2.mean&SE.csv",sep=",")

#in excel (raw data + co2 mean)
# write.xlsx(iem2,"C:/Users/sh3410/Dropbox/Shun_Data/IEM/FACE_IEM_Summary.xlsx",sheetName="raw",append=FALSE) #store in deopbox
#write.xlsx(treat.mean,"C:/Users/sh3410/Dropbox/Shun_Data/IEM/FACE_IEM_Summary.xlsx",sheetName="CO2_mean",append=TRUE) 

#save summary table as an excel sheet
source("functions/summary_table_excel.R")
