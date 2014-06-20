##################
# Moist and Temp #
##################
theme_set(theme_bw())

p <- ggplot(subsetD(iem, !pre), aes(x = Temp_Max, y = log(Moist), col = ring))
p2 <- p + geom_point(alpha = .5) 

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output/figs/FACE_IEM_SoilVa_Ringr", plot = pl, width = 6, height = 6)

ggsavePP(file = "output/figs/FACE_IEM_SoilVa", plot = p2, width = 6, height = 6)

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


