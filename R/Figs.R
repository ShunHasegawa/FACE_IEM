theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(iemMlt, .(time, date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- ddply(RngMean, .(time, date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))

#################################
# plot each nutrient separately #
#################################
vars <- c("Nitrate", "Ammonium", "Phosphate")

RngFg <- dlply(RngMean, .(variable), PltMean)
fls <- paste("output//figs/FACE_IEM_Ring_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("output//figs/FACE_IEM_CO2Trt_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"-N),
  'nh' = expression(NH[4]^"+"-N),
  'po' = expression(PO[4]^"3-"-P))


ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltMean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsavePP(filename = "output//figs/FACE_IEM_CO2Trt", plot = pl, width = 6, height = 6)

########################################################
# plot soil moist and temp for each incubation periods #
########################################################

## raw (before taking two week mean) ##
load("Data/FACE_TDR_ProbeDF.RData")

# subset IEM tdr
iemTDR <- subsetD(FACE_TDR_ProbeDF, Sample == "IEM")

# add co2 and remove unnecessary columns
iemTDR <- within(iemTDR, {
  co2 <- factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
  plot <- factor(plot)
  Temp_Min <- NULL
  Temp_Max <- NULL
  Sample <- NULL
})
names(iemTDR)[5] <- "Temp"

iemTDRdf <- melt(iemTDR, id = c("Date", "co2", "ring", "plot"))
iemTDRdf$type <- iemTDRdf$variable

iemTDR_RngMean <- ddply(iemTDRdf, .(Date, co2, ring, variable, type), summarise, value = mean(value, na.rm = TRUE))
iemTDR_co2Mean <- ddply(iemTDR_RngMean, .(Date, co2, variable, type), summarise, value = mean(value, na.rm = TRUE))

## Make figs
SoilVarDF <- iem[, c("co2", "ring", "date", "Moist", "Temp_Mean", "Temp_Min", "Temp_Max")]

## co2 ##
pl <- PltSoilVar(data = SoilVarDF, var = "co2") +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~trt), 
                     labels = c("Ambient", expression(eCO[2]))) +
  geom_line(aes(x = Date, y = value, group = co2), data = iemTDR_co2Mean) +
  geom_vline(xintercept = as.numeric(unique(iem$insertion)), col = "green", size = .5)

ggsavePP(filename = "output//figs/FACE_IEM_SoilVarMonth_CO2", plot = pl, width = 6, height = 4)


## ring ##
pl <- PltSoilVar(data = SoilVarDF, var = "ring") +
  scale_color_manual(values = palette(), "Ring", labels = paste("Ring", c(1:6), sep = "_")) +
  geom_line(aes(x = Date, y = value, group = ring), data = iemTDR_RngMean) +
  geom_vline(xintercept = as.numeric(unique(iem$insertion)), col = "green", size = .5)

ggsavePP(filename = "output//figs/FACE_IEM_SoilVarMonth_Ring", plot = pl, width = 6, height = 4)

############################
# Plot Moist against  Temp #
############################

p <- ggplot(subsetD(iem, !pre), aes(x = Temp_Max, y = log(Moist), col = ring))
p2 <- p + geom_point(alpha = .5) 

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output/figs/FACE_IEM_SoilVar_Ring", plot = pl, width = 6, height = 6)

ggsavePP(file = "output/figs/FACE_IEM_SoilVar", plot = p2, width = 6, height = 6)

# Soil moisture and temperature seems to be correlated
# Temperature may regulate. 
