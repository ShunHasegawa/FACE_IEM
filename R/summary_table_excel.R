#########################
# creates summary table #
#########################
CreateTable <- function(dataset,fac,nutrient){
  #dataset=iem2 for ring summary, ring.mean for co2 summary, fac=ring or co2, nutrient=no/nh/p
  a <- dataset[c("date",fac,nutrient)] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a,date~variable,mean,na.rm=TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- merge(means,ses,by="date") #merge datasets
  mer <- mer[,c(1,order(names(mer)[-1])+1)] #re-order columns
  mer <- merge(mer,samples,by="date")
  mer
  return(mer)
}

#function which creates excel worksheets
crSheet <- function(sheetname,dataset,fac,nutrient){
  #create sheet
  sheet <- createSheet(wb,sheetName=sheetname)
  
  #add data to the sheet
  addDataFrame(CreateTable(dataset,fac,nutrient),sheet,showNA=TRUE,row.names=FALSE,startRow=2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname,"unit=ug cm^(-2) day^(-1))")),sheet,startRow=1,row.names=FALSE,col.names=FALSE)
}


#create xcel workbook
wb <- createWorkbook()

#worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(iem,sheet,showNA=TRUE,row.names=FALSE)

#worksheet for rowdata without outlier
sheet <- createSheet(wb,sheetName="row_data_without.outlier")
addDataFrame(iem2,sheet,showNA=TRUE,row.names=FALSE)

#crate excel workwheet for each varable
#worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate","Ammonium","Phosphate"),sep="")
nut <- c("no","nh","p")

for (i in 1:3){
  crSheet(sheetname=shnames[i],dataset=iem2,fac="ring",nutrient=nut[i])
}

#worksheets for co2 summary
shnames <- paste("CO2_mean.",c("Nitrate","Ammonium","Phosphate"),sep="")
for (i in 1:3){
  crSheet(sheetname=shnames[i],dataset=ring.mean,fac="co2",nutrient=nut[i])
}

#save file
saveWorkbook(wb,"table/FACE_IEM.xlsx")
