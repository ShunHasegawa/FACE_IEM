##################
# Moist and Temp #
##################
theme_set(theme_bw())
iem$TrP <- (iem$p + 1.6)^(-1.1515)

p <- ggplot(subsetD(iem, !pre), aes(x = Temp_Max, y = log(Moist), size = TrP, col = TrP))
p2 <- p + geom_point(alpha = .5) + 
  scale_size(range = c(8, 1)) +
  scale_color_gradientn(colours = c("red", "yellow", "blue"))

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output/figs/FACE_IEM_P_withSoilVar_ring", plot = pl, width = 6, height = 6)

ggsavePP(file = "output/figs/FACE_IEM_P_withSoilVar", plot = p2, width = 6, height = 6)

# Soil moisture and temperature seems to be correlated
# Temperature may regulate soil moisture


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


