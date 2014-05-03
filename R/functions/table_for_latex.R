#####
#summary table
#####
names(iem)
summary(iem)
boxplot(iem$p)

#remove P outlier
iem2<-iem
iem2$p[which(iem2$p==max(iem2$p))]<-NA
summary(iem2)

#ring mean
ring.mean<-with(iem2,aggregate(iem2[,c(6:8)],list(time=time,date=date,ring=ring,co2=co2),mean,na.rm=TRUE))


#co2 mean
treat.mean<-summaryBy(no+nh+p~time+date+co2,FUN=c(mean,function(x) ci(x)[4],length),
                      fun.names=c("mean","SE","Sample_size"),data=ring.mean)

CreateMeanDataframe <- function(data, value){
  x <- data[[value]]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

ring.mean.ml <- melt(ring.mean, id = c("time", "date", "ring", "co2"))
head(ring.mean.ml)

trt.mean <- dlply(ring.mean.ml, .(date, co2, variable), function(x) CreateMeanDataframe(x, "value"))
trt.mean

CreateMeanTable(ring.mean, value = "no")

ring.mean

#create table
names(iem2)

#create table
ntr <- c("no", "nh", "p")
ring.mean.tables <- lapply(ntr, function(x) format(CreateTable(iem2, fac = "ring", nutrient = x),digits = 2))
trt.mean.table <- lapply(ntr, function(x) format(CreateTable(ring.mean, fac = "co2", nutrient = x),digits = 2))





