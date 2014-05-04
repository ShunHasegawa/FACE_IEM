rm(list=ls(all=TRUE))

library(xlsx)
library(plyr)
library(reshape)


Process.strp <- function(file, time){
  b <- read.table(file, row.names = NULL)
  b.df <- ddply(b, .(row.names), summarise, Area = sum(Area)) 
  b.df$time <- time
  b.df$ring <- gsub(".*_|[.].*", "", file) # bofore "/" and after "_" parts were removed from strings
  b.df$variable <- gsub(".*/|_.*", "", file)
  return(b.df)
}

# combine the processed results with their dates
cmbn.strp <- function(dates){
  files <- paste("Data//Strips//", dates, sep = "") # choose folder
  a <- dir(files, full.names = TRUE) # files in the folder to be read
  a.cmb <- ldply(a, function(x) Process.strp(x, time = gsub(".*//", "", files))) # process and combine them all
  return(a.cmb)
}

dates <- dir("Data/Strips/")

results <- ldply(dates, cmbn.strp)

ftable(xtabs(~ time + ring + variable + row.names, data = results))

# Dec2013 P_5.2 has two values so combine them
results <- ddply(results, .(time, ring, row.names, variable), summarise, Area = sum(Area))

# reshape table
names(results)[grep("Area", names(results))] <- "value"
names(results)[grep("row", names(results))] <- "plot" # cast does not work the column name "row.names".
rst.cst <- cast(results, time + ring + plot ~ variable)

write.csv(rst.cst, "Data//Strips/Result_IEM.SurfaceArea.csv", row.names = FALSE)
