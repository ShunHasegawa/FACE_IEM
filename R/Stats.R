########################################################
# plot soil moist and temp for each incubation periods #
########################################################

SoilVarDF <- iem[, c("co2", "ring", "time", "Moist", "Temp_Mean", "Temp_Min", "Temp_Max")]

# co2
SoilVarDF_CO2 <- ddply(SoilVarDF, .(time, co2),
                       function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")],na.rm = TRUE))
SoilVarMlt <- melt(SoilVarDF_CO2, id = c("co2", "time"))
SoilVarMlt$type <- factor(ifelse(SoilVarMlt$variable != "Moist", "Temp", "Moist"))

theme_set(theme_bw())
p <- ggplot(SoilVarMlt, aes(x = time, y = value, shape = variable, col = co2))
pl <- p + geom_point() +
  facet_grid(type ~., scale = "free_y")
ggsavePP(filename = "output//figs/FACE_IEM_SoilVarSummary_CO2", plot = pl, width = 6, height = 4)


# ring
SoilVarDF_ring <- ddply(SoilVarDF, .(time, ring),
                        function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")],na.rm = TRUE))
SoilVarMlt <- melt(SoilVarDF_ring, id = c("ring", "time"))
SoilVarMlt$type <- factor(ifelse(SoilVarMlt$variable != "Moist", "Temp", "Moist"))

p <- ggplot(SoilVarMlt, aes(x = time, y = value, shape = variable, col = ring))
pl <- p + geom_point() +
  facet_grid(type ~., scale = "free_y")
ggsavePP(filename = "output//figs/FACE_IEM_SoilVarSummary_Ring", plot = pl, width = 6, height = 4)

###########
# Nitrate #
###########
source("R/Stats_NO.R")

#############
# Ammonium  #
#############
source("R/Stats_NH.R")

#############
# Phosphate #
#############
source("R/Stats_P.R")


