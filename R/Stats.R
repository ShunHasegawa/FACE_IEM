##################
# soil variables #
##################
load("Data/FACE_TDR_Probe.RData")
names(FACE_TDR_Probe)[grep("Plot", names(FACE_TDR_Probe))] <- "plot"
names(FACE_TDR_Probe)[grep("variable", names(FACE_TDR_Probe))] <- "probe"
FACE_TDR_Probe$type <- factor(ifelse(grepl("VWC",FACE_TDR_Probe$probe), "Moist", "Temp"))

FACE_TDR_Probe_Mlt <- melt(FACE_TDR_Probe, id = names(FACE_TDR_Probe)[which(!(names(FACE_TDR_Probe) %in% c("Mean", "Min", "Max")))])

FACE_TDR_ProbeDF <- cast(FACE_TDR_Probe_Mlt, Date + ring + plot + Sample ~ type + variable)

# remove moist min and max
FACE_TDR_ProbeDF <- FACE_TDR_ProbeDF[, -grep("Moist_Min|Moist_Max", names(FACE_TDR_ProbeDF))]
names(FACE_TDR_ProbeDF)[grep("Moist", names(FACE_TDR_ProbeDF))] <- "Moist"

# subset iem
TdrIem <- subsetD(FACE_TDR_ProbeDF, Sample == "IEM")

# compute mean of soil variable for given period


SoilVarDD <- function(data, rings, plots, Start, End){
  sDF <- subset(data, Date >= Start & Date >= End & ring == rings & plot == plots)
  ddply(sDF, .(ring, plot),function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE))
}

IEMSoil <- ddply(iem, .(insertion, sampling, ring, plot), 
           function(x) SoilVarDD(data = TdrIem, Start = x$insertion, End = x$sampling, rings = x$ring, plot = x$plot))

# merge
iem <- merge(iem, IEMSoil, by = c("insertion", "sampling", "ring", "plot"))

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


