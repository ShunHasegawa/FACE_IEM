# melt dataset
names(iem)
iemMlt <- melt(iem, id = c("time", "date", "ring", "plot", "co2", "id"))

# Ring summary table & mean
RngSmmryTbl <- dlply(iemMlt, .(variable), function(x) CreateTable(x, fac = "ring"))
RngMean <- ddply(iemMlt, .(time, date, co2, ring, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2"))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(iem, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate", "Ammonium","Phosphate", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrate", "Ammonium","Phosphate"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/FACE_IEM.xlsx")
