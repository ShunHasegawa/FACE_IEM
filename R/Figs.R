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
vars <- c("Nitrate", "Ammonium", "Phosphate", "NPratios")

RngFg <- dlply(RngMean, .(variable), PltMean)
fls <- paste("output//figs/FACE_IEM_Ring_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("output//figs/FACE_IEM_CO2Trt_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_wrap
ylabs <- c(expression(NO[3]^"-"*-N~(ng~cm^"-2"~d^"-1")),
           expression(NH[4]^"+"*-N~(ng~cm^"-2"~d^"-1")), 
           expression(PO[4]^"3-"*-P~(ng~cm^"-2"~d^"-1")),
           expression(log(N:P~ratios)))


pl <- PltMean(TrtMean) +
  facet_wrap(~ variable, scales= "free_y")
pl2 <- facet_wrap_labeller(pl, labels = ylabs)
pl2

ggsavePP(filename = "output//figs/FACE_IEM_CO2Trt", plot = pl, width = 6.5, height = 6)

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

# not show a dagger mark
ContrastDF$stars[!ContrastDF$stars %in% c("", "*", "**")] <- ""

Antt_CntrstDF <- merge(ContrastDF, 
                       ddply(TrtMean, .(date, variable), summarise, yval = max(Mean + SE)),
                       # this return maximum values
                       by = c("date", "variable"), all.x = TRUE)
Antt_CntrstDF$co2 <- "amb" # co2 column is required as it's used for mapping

#########################################################
# Plot each separately to add y axis title only for N:P #
#########################################################
subLabDF <- with(TrtMean, 
                 data.frame(xv = as.Date("2012-7-25"),
                            ddply(TrtMean, .(variable), summarise, yv = max(Mean + SE)),
                            labels = paste("(", letters[1:length(levels(variable))], ")", sep = ""),
                            co2 = "amb"))

ylengthDF <- ddply(TrtMean, 
                   .(variable), 
                   function(x) 
                     data.frame(ylength = max(x$Mean +x$SE, na.rm = TRUE) -
                                  min(x$Mean - x$SE, na.rm = TRUE)))

## create df
statDF <- StatPositionDF(StatRes = Stat_CO2Time, 
                         variable = levels(ylengthDF$variable), 
                         ytop = c(ymaxDF[1, 2] - 250,  ymaxDF[2:4 , 2]*1.07),
                         ylength = ylengthDF$ylength, 
                         gap = .08)


WBFig_sub <- function(data, figTheme = science_theme, plotlabel, xlab = "", ylab = "", legpos = "none"){
  data <- droplevels(data)
  vars <- levels(data$variable)
  
  # create a plot  
  p <- ggplot(data, aes(x = date, y = Mean, group = co2))
  
  p2 <- p + 
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", col = "black", 
               size = .6) +
    geom_line(aes(linetype = co2), position = position_dodge(20)) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                  width = 0,
                  position = position_dodge(20), 
                  size = .4) + 
    geom_point(aes(fill = co2),
               shape = 21, 
               position = position_dodge(20),
               size = 3) +
    scale_x_date(breaks= date_breaks("3 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-6-15", "2014-3-29"))) +
    scale_fill_manual(values = c("black", "white"), 
                      labels = c("Ambient", expression(eCO[2]))) +
    scale_linetype_manual(values = c("solid", "dashed"), 
                          labels = c("Ambient", expression(eCO[2]))) +
    geom_text(aes(x = xv, y = yv * 1.06, label = labels),
              fontface = "bold",
              hjust = 1,
              data = subset(subLabDF, variable == vars)) +
    facet_wrap(~variable, scales= "free_y", drop = TRUE) +
    figTheme +
    theme(legend.position = legpos) +
    geom_text(data = subset(statDF, predictor != "" & variable == vars), 
              aes(x = as.Date("2013-12-20"), y = yval, label = predictor),
              size = 3, hjust = 1, parse = TRUE) +
    # unless remove [" "] with predictor != "", labels will be messed up due to
    # this empty level
    geom_text(data = subset(statDF, variable == vars), 
              aes(x = as.Date("2014-2-20"), y = yval, label = p), 
              size = 3, parse = TRUE)+
    geom_text(data = subset(Antt_CntrstDF, variable == vars),
              aes(x = date, y = yval, label = stars), vjust = 0)+
    labs(x = xlab, y = ylab)
  return(p2)
}

# top panels
p_no <- WBFig_sub(data = subset(TrtMean, variable == "no")) +
  theme(legend.position = c(.7, .89),
        axis.text.x  = element_blank(),
        plot.margin = unit(c(1, -.2, 0, -.5), "line"))
p_no <- facet_wrap_labeller(p_no, labels = expression(NO[3]^'-'*-N))

p_nh <- WBFig_sub(data = subset(TrtMean, variable == "nh")) +
  theme(axis.text.x  = element_blank(),
        plot.margin = unit(c(1, 1, 0, 0), "line"))
p_nh <- facet_wrap_labeller(p_nh, labels = expression(NH[4]^'+'*-N))

# bottom panels
p_p <- WBFig_sub(data = subset(TrtMean, variable == "p")) +
  theme(plot.margin = unit(c(-1, -.2, 0, -.5), "line"))
p_p <- facet_wrap_labeller(p_p, labels = expression(PO[4]^'3-'*-P))

p_np <- WBFig_sub(data = subset(TrtMean, variable == "logNP"), ylab = "log(N:P ratios)") +
  theme(plot.margin = unit(c(-1, 1, 0, 0), "line"),
        axis.title.y = element_text(vjust = -0.5))
p_np <- facet_wrap_labeller(p_np, labels = "N:P ratios")

lp <- gtable:::rbind_gtable(p_no, p_p, "first")
rp <- gtable:::rbind_gtable(p_nh, p_np, "first")
ap <- gtable:::cbind_gtable(lp, rp, "first")

pp <- arrangeGrob(ap,
                  left = textGrob(expression(IEM*-adsorbed~nutrients~(ng~cm^"-2"~d^"-1")),
                                  rot = 90, vjust = 1),
                  sub = textGrob("Month", vjust = -1))
ggsavePP(filename = "output//figs/FACE_manuscript/FACE_IEM_withNP_postCO2_II", 
         plot = pp, width = 6.65, height = 5.65)

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
