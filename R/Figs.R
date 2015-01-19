theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(iemMlt, .(time, date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- ddply(RngMean, .(time, date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))
save(TrtMean, file = "output//data/FACE_IEM_CO2Mean.RData")

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
  'no' = expression(NO[3]^"-"),
  'nh' = expression(NH[4]^"+"),
  'po' = expression(PO[4]^"3-"))


ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltMean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)

ggsavePP(filename = "output//figs/FACE_IEM_CO2Trt", plot = pl, width = 6, height = 6)

########################
# Plot for publication #
########################
# load stat table, note that if you want the most updated one, you need to run

# Stat.R first
load("output//data//CO2Time_Stat.RData")

# theme
theme_set(theme_bw())

# ymax value for each variable
ymaxDF <- ddply(TrtMean, .(variable), function(x) max(x$Mean + x$SE, na.rm = TRUE))

# load contrastDF to annotate stat result and combine with max values from
# TrtMean as y position
load("output//data/FACE_IEM_ContrastDF.RData")
Antt_CntrstDF <- merge(ContrastDF, 
                       ddply(TrtMean, .(date, variable), summarise, yval = max(Mean + SE)),
                       # this return maximum values
                       by = c("date", "variable"), all.x = TRUE)
Antt_CntrstDF$co2 <- "amb" # co2 column is required as it's used for mapping

p <- WBFig(data = TrtMean, ylab = expression(IEM*-adsorbed~nutrients~(ng~cm^"-2"~d^"-1")),
           StatRes = Stat_CO2Time, 
           StatY = ymaxDF[ , 2]) +
  geom_text(data = Antt_CntrstDF, aes(x = date, y = yval, label = stars), vjust = 0)

ggsavePP(filename = "output//figs/FACE_manuscript/FACE_IEM", plot = p, width = 6, height = 6)

################
## for poster ##
################
poster_theme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.text.x = element_text(angle=45, vjust= 1, hjust = 1, 
                                                 size = 13),
                      legend.position = "non",
                      axis.title.y = element_text(size = 15),
                      plot.title = element_text(size = 25, face = "bold"))

pl_Meeting  <- PltMean(subsetD(TrtMean, variable == "p")) +
  labs(x = NULL, y = expression(IEM*-adsorbed~PO[4]^"3-"~(ng~cm^"-2" ~ d^"-1")))+
  poster_theme +
  geom_text(data = subset(Antt_CntrstDF, variable == "p"),
            aes(x = date, y = yval, label = stars), 
            col = "black", vjust = 0, size = 7)
ggsavePP(filename = "output//figs/GroupMeeting//FACE_IEM_CO2_P", plot = pl_Meeting,
         width = 6, height = 4)

pl  <- pl_Meeting + ggtitle("Plant accessible P")
ggsavePP(filename = "output//figs/GSBI_Poster/FACE_IEM_CO2_P", plot = pl, width = 6, height = 4)


########################################################
# plot soil moist and temp for each incubation periods #
########################################################

##########################
## Process raw TDR data ##
##########################
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

iemTDRdf <- melt(iemTDR, id = c("Date", "co2", "ring", "plot"))
iemTDRdf$type <- factor(ifelse(iemTDRdf$variable == "Moist", "Moist", "Temp")) 
  # need "type" column for ggplot later

# compute mean
iemTDR_RngMean <- ddply(iemTDRdf, .(Date, co2, ring, variable, type), summarise, value = mean(value, na.rm = TRUE))
iemTDR_co2Mean <- ddply(iemTDR_RngMean, .(Date, co2, variable, type), summarise, value = mean(value, na.rm = TRUE))

##############################################
## Plot raw data and incubation-period mean ##
##############################################

## co2 ##
pl <- PltSoilVar(data = iem, var = "co2", tdrData = iemTDR_co2Mean, linealpha = .5) +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~trt), 
                     labels = c("Ambient", expression(eCO[2]))) +
  ggtitle("Mean soil moisture and temperature\nduring IEM incubation")
ggsavePP(filename = "output//figs/FACE_IEM_SoilVarMonth_CO2", plot = pl, width = 6, height = 4)

## ring ##
pl <- PltSoilVar(data = iem, var = "ring", tdrData = iemTDR_RngMean, linealpha = .3) +
  scale_color_manual(values = palette(), "Ring", labels = paste("Ring", c(1:6), sep = "_")) +
  ggtitle("Mean soil moisture and temperature\nduring IEM incubation")

ggsavePP(filename = "output//figs/FACE_IEM_SoilVarMonth_Ring", plot = pl, width = 6, height = 4)

############################
# Plot Moist against  Temp #
############################

p <- ggplot(subsetD(iem, !pre), aes(x = Temp_Max, y = log(Moist), col = ring))
p2 <- p + geom_point(alpha = .5) 

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output/figs/FACE_IEM_SoilVar_Ring", plot = pl, width = 6, height = 6)

ggsavePP(file = "output/figs/FACE_IEM_SoilVar", plot = p2, width = 6, height = 6)
