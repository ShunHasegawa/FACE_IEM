# remove outlier
iemRmOl <- iem

# P
boxplot(iem$p)
iemRmOl$p[which(iemRmOl$p == max(iemRmOl$p))] <- NA

# nh
boxplot(iem$nh)
iemRmOl$nh[which(iemRmOl$nh > 800)] <- NA

# melt dataset
iemMlt <- melt(iemRmOl, id = names(iem)[which(!(names(iem) %in% c("no", "nh", "p")))])

# response ratio calculated for each block
BlockRatio(iemMlt)

# Ring summary table & mean
RngSmmryTbl <- dlply(iemMlt, .(variable), function(x) CreateTable(x, fac = "ring", digit = 1, nsmall = 2))
RngMean <- ddply(iemMlt, .(time, date, co2, ring, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2",  digit = 1, nsmall =2))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rawdata
sheet <- createSheet(wb,sheetName="raw_data")
addDataFrame(iem, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate", "Ammonium","Phosphate", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for co2 trt summary
shnames <- paste("CO2_mean.", c("Nitrate", "Ammonium","Phosphate"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/FACE_IEM.xlsx")

