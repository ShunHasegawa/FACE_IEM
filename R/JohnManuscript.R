rmpDF <- subsetD(iem, time %in% c(3:8))
rmpDF <- rmpDF[c("date", "time", "block", "ring", "plot", "no", "nh", "p")]



# ring average for each date
MeanDF <- ddply(rmpDF, .(time, co2, block, ring), summarise, 
                M = mean(p, rm.na = TRUE))
# ratio e/a
MeanDFcst <- cast(MeanDF, time + block ~ co2, value = "M")

MeanDFcst <- within(MeanDFcst, {
  ratio  <- elev/amb
  co2 <- factor(ifelse(time %in% c(3, 4), "pre", "post"))
})

# block average for each of pre and post-co2
Mean_blockDf <- ddply(MeanDFcst, .(block, co2), summarise, meanR = mean(ratio))

# treatment average for each of pre and post-co2
MeanRDf <- ddply(Mean_blockDf, .(co2), summarise, MeanR = mean(meanR), SE = ci(meanR)[4])
MeanRDf$co2 <- relevel(MeanRDf$co2, "pre")

theme_set(theme_bw())
p <- ggplot(MeanRDf, aes(x = co2, y = MeanR, fill = co2))
p2 <- p + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = MeanR - SE, ymax = MeanR + SE), width = .5) +
  geom_hline(aes(yintercept = 1))

+ 


+ 
  geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = colfactor), width = 5) + 
  labs(x = "Time", y = ylab) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed", col = "black") +
  scale_x_date(breaks= date_breaks("2 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2012-7-1", "2014-4-2"))) +
  theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))