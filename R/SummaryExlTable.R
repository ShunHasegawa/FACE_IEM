# remove outlier
iemRmOl <- iem

# Phosphate
boxplot(iem$p)
iemRmOl$p[which(iem$p == max(iem$p))] <- NA

# Ammonium
boxplot(iem$nh)
iemRmOl$nh[which(iem$nh > 800)] <- NA

# Nitrate
boxplot(log(iem$no))
iemRmOl$no[which(iem$no == min(iem$no))] <- NA

# NP
# remove the values calculated from the above values
iemRmOl$logNP[is.na(iemRmOl$nh)|is.na(iemRmOl$p)|is.na(iemRmOl$no)] <- NA

# melt dataset
iemMlt <- melt(iemRmOl, id = names(iem)[which(!(names(iem) %in% c("no", "nh", "p", "logNP")))])

# Ring summary table & mean
RngSmmryTbl <- dlply(iemMlt, .(variable), function(x) CreateTable(x, fac = "ring", digit = 1, nsmall = 2))
RngMean <- ddply(iemMlt, .(time, date, co2, ring, block, variable), summarise, 
                 value = mean(value, na.rm = TRUE)) 

# treat summary table & mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2",  digit = 1, nsmall =2))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rawdata
sheet <- createSheet(wb,sheetName="raw_data")
addDataFrame(iem, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheet without outlier
sheet <- createSheet(wb,sheetName="raw_data_withoutOutlier")
addDataFrame(iemRmOl, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate", "Ammonium","Phosphate", "logNPRatio", sep=""))
l_ply(1:4, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for co2 trt summary
shnames <- paste("CO2_mean.", c("Nitrate", "Ammonium","Phosphate", "logNPRatio"), sep = "")
l_ply(1:4, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/FACE_IEM.xlsx")
