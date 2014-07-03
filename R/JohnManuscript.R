load("output//data/FACE_rampDF.RData")

library(plyr)
library(reshape)
library(ggplot2)
library(gmodels)

# process dataframe
head(rmpDF)
rmpDF$co2 <- factor(ifelse(rmpDF$ring %in% c(1, 4, 5), "elev", "amb"))
rmpDFMlt <- melt(rmpDF, id = c("date", "time", "block","ring", "plot", "co2"))

# ring average for each date
MeanDF <- ddply(rmpDFMlt, .(time, block, ring, co2, variable), summarise, 
                M = mean(value, rm.na = TRUE))
# ratio e/a
MeanDFcst <- cast(MeanDF, time + block + variable ~ co2, value = "M")

MeanDFcst <- within(MeanDFcst, {
  ratio  <- elev/amb
  co2 <- factor(ifelse(time %in% c(3, 4), "pre", "post"))
})

# block average for each of pre and post-co2
Mean_blockDf <- ddply(MeanDFcst, .(block, co2, variable), summarise, meanR = mean(ratio))

# treatment average for each of pre and post-co2
MeanRDf <- ddply(Mean_blockDf, .(co2, variable), summarise, MeanR = mean(meanR), SE = ci(meanR)[4])
MeanRDf$co2 <- relevel(MeanRDf$co2, "pre")

theme_set(theme_bw())
p <- ggplot(MeanRDf, aes(x = co2, y = MeanR, fill = co2))
p2 <- p + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = MeanR - SE, ymax = MeanR + SE), width = .5) +
  geom_hline(aes(yintercept = 1)) +
  facet_grid(~variable) +
  ylab("IEM_Ratio(Elev/Amb)")

ggsave(filename = "output//figs/FACE_rmp_barfigWithBlock.pdf", plot = p2, 
       width = 6, height = 3)
