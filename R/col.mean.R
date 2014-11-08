bdf <- subset(postDF, block == "B")
head(bdf)
bdf.m <- ddply(bdf, .(time, ring, co2), summarise, no = mean(no, na.rm = TRUE))
plot(no ~ co2, data = bdf.m)

timeM <- ddply(postDF, .(date, time, ring, block, co2), function(x) colMeans(x[c("no", "nh", "p", "Moist", "Temp_Mean")]))

ratios <- ddply(timeM, .(time, date, block), function(x){
  data.frame(no.r = x$no[x$co2 == "elev"]/x$no[x$co2 == "amb"],
             nh.r = x$nh[x$co2 == "elev"]/x$nh[x$co2 == "amb"],
             p.r = x$p[x$co2 == "elev"]/x$p[x$co2 == "amb"],
             Moist = mean(x$Moist),
             Temp_Mean = mean(x$Temp_Mean))
})

rate.mean <- ddply(ratios, .(time, date), function(x) colMeans(x[, -3:-1]))

colMeans(rate.mean[,-1:-2])

mean(subset(rate.mean, Temp_Mean > 15 & Moist < 0.2)$p.r)
mean(subset(rate.mean, Temp_Mean > 15 & Moist < 0.14)$p.r)
mean(subset(rate.mean, Temp_Mean > 15 & Moist < 0.08)$p.r)
p.df <- subset(rate.mean, Temp_Mean > 15 & Moist < 0.14)

mean(subset(rate.mean, Moist > 0.1)$nh.r)



colMeans(p.df[, -1:-2])



rate.mlt <- melt(rate.mean, id = c("time", "date", "Moist", "Temp_Mean"))
p <- ggplot(data = rate.mlt, aes(x = date, y = value))
p + geom_point() + facet_grid(~ variable)


timeM2 <- ddply(postDF, .(time, co2), function(x)
  data.frame(no = mean(x$no, na.rm = TRUE),
             nh = mean(x$nh, na.rm = TRUE),
             p = mean(x$p, na.rm = TRUE)))

ratios2 <- ddply(timeM2, .(time), function(x){
  data.frame(no.r = x$no[x$co2 == "elev"]/x$no[x$co2 == "amb"],
             nh.r = x$nh[x$co2 == "elev"]/x$nh[x$co2 == "amb"],
             p.r = x$p[x$co2 == "elev"]/x$p[x$co2 == "amb"])
})

colMeans(ratios2[,2:4])

TrtSmmryTbl
