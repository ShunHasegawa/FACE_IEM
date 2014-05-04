#######################
#model simplification #
#######################
MdlSmpl <- function(model){
  mod2 <- update(model, method = "ML") #change method from REML to ML
  stai <- stepAIC(mod2, trace = FALSE) #model simplification by AIC
  dr <- drop1(stai, test="Chisq") #test if removing a factor even more significantly lowers model
  model <- update(stai, method="REML")
  ifelse(all(dr[[4]] < 0.05, na.rm=TRUE), anr <- anova(model), anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic = stai$anova, drop1 = dr, anova.reml = anr, model.reml = model, model.ml = stai))
}

###################################
# Coorect AQ2 result based on CCV #
###################################

# subset dataset beween each ccv
Crrct.ccv <- function(x, data, ccval = 7.5){
  # subset dataset between given ccvs
  ccv.ps <- grep("V$", as.character(data$Sample.ID))
  
  ts <- data[c(ccv.ps[x] : ccv.ps[x + 1]),]
  trng <- range(ts$times)
  xv <- as.numeric(trng)
  yv <- ts$Result[ts$times == trng[1] | ts$times == trng[2]] 
  
  b <- ccval * (1 / yv[1] - 1 / yv[2]) / (xv[1] - xv[2])
  a <- ccval / yv[1] - b * xv[1]
  
  ts$Result <- ts$Result * (a + b * as.numeric(ts$times))
  ts$times <- NULL
  return(ts)
}

# applied the function above for each subsets of ccvs
Crrtct.ccv.df <- function(filename, ccval = 7.5){
  data <- read.csv(paste("Data/AQ2/NeedToBeCorrected/", filename, sep = ""), header = TRUE)
  
  # make time factor as numeric
  a <- sapply(as.character(data$Time), strsplit, " ")
  b <- ldply(a, function(x) paste(x[c(5, 2, 3, 4)], collapse = "/"))
  
  b$V1 <- ymd_hms(b$V1)
  
  names(b) <- c("Time", "times")
  
  # merge
  mrg.res <- merge(data, b, by = "Time")
  
  # reorder accoding to time
  mrg.res <- mrg.res[order(mrg.res$times), ]
  
  # add the latest ccv value at the end of data frame to make sure all measured values are placed between ccvs
  # if the last line is not ccv, vlues after last ccv will be turned into NA
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  lstTime <- mrg.res$times[nrow(mrg.res)]
  mrg.res[nrow(mrg.res) + 1, ] <- mrg.res[max(ccv.ps), ] 
  mrg.res$times[nrow(mrg.res)] <- lstTime + 1 # make the last time latest by adding 1 to the actual latest time
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID)) # update ccv.position
  
  # re-caluculate the results
  a <- ldply(1:(length(ccv.ps)-1), function(x) Crrct.ccv(x, data = mrg.res, ccval))
  
  # if there are negative values, add minimum value to remove any negative values
  if(any(a$Result < 0)) a$Result <- a$Result - min(a$Result, na.rm = TRUE)
  
  return(a)
}


######################################################
# process and combine aq 2 data, then create a table #
######################################################
prcsAQ2 <- function(data){
  # remove ccv, ccb, standard
  res <- data[-grep("^C|^STANDARD", as.character(data$Sample.ID)),]
  
  # remove dup, top, middle
  res <- res[-grep("dup$|top|middle", as.character(res$Sample.Details)),]
  
  # sample labels
  a <- strsplit(as.character(res$Sample.Details), "[.]")
  
  # turn this into data frame
  a.df <- ldply(a)
  names(a.df)[c(1, 4, 5)] <- c("Date", "ring", "plot")
  a.df$Date <- ymd(a.df$Date)
  res.df <- cbind(a.df, res)
  res.df <- res.df[c("Date", "ring", "plot", "Result")]
  return(res.df)
}

cmbn.fls <- function(file){
  # read files
  rd.fls <- lapply(file, function(x) read.csv(paste("Data/AQ2/ReadyToProcess/", x, sep = ""), header = TRUE))
  
  # process and make data frame for each test type
  pr.df <- ldply(rd.fls, function(x) ddply(x, .(Test.Name), prcsAQ2))
  
  # reshape
  names(pr.df)[5] <- "value"
  pr.cst <- cast(pr.df, Date + ring + plot ~ Test.Name)
  return(pr.cst)
}

##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac, ...){
  a <- dataset[c("date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(format(mer, ...))
}

#function which creates excel worksheets
crSheet <- function(sheetname, dataset){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  addDataFrame(dataset, sheet, showNA = TRUE, row.names = FALSE, startRow = 2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=ng cm^(-1) day^(-1)")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

####################
# plot mean and se #
####################
PltMean <- function(data){
  
  vars <- c(substitute(nutrients),
            substitute(NO[3]^"-"-N), 
            substitute(NH[4]^"+"-N),
            substitute(PO[4]^"3-"-P))
  # subsitute returens argument as it is without calculation (similar to expression())
  
  yvars <- lapply(vars, function(x) bquote(IEM-adsorbed~.(x)))
  # bquote allows one to call an object and return expression
  
  ylabs <- lapply(yvars, function(x) {
    c(expression(), 
      bquote(atop(paste(.(x)), paste((ng~cm^"-1"~d^"-1")))))         
  })
  
  # atop: put the 1st argument on top of the 2nd
  
  ylab <- ifelse(length(unique(data$variable)) > 1, ylabs[[1]],
                 ifelse(unique(data$variable) == "no", ylabs[[2]], 
                        ifelse(unique(data$variable) == "nh", ylabs[[3]],
                               ylabs[[4]])))
  
  colfactor <- ifelse(any(names(data) == "ring"), "ring", "co2")
  
  p <- ggplot(data, aes_string(x = "date", y = "Mean", col = colfactor))
  
  p2 <- p + geom_line(size = 1) + 
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = colfactor), width = 5) + 
    labs(x = "Time", y = ylab) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed", col = "black")
  
  # change colors, linetype and associated legend according to plotting groups (chamber or treatment)
  if(colfactor == "co2") 
    p3  <- p2 +  
    scale_color_manual(values = c("blue", "red"), expression(CO[2]~trt), labels = c("Ambient", expression(eCO[2]))) else
      p3 <- p2 + 
    scale_color_manual(values = palette(), "Ring", labels = paste("Ring", c(1:6), sep = "_")) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid", "solid", "dashed"), 
                          "Ring", labels = paste("Ring", c(1:6), sep = "_"))
  
  # add asterisk on P graphs at co2 treatments
  if(colfactor == "ring" | !any(unique(data$variable) == "po")) p3 else{
    newDF <- subset(data, time %in% c(2, 6)) # the times where "*" is placed
    ant_pos <- ddply(newDF, .(date, variable), summarise, Mean = max(Mean + SE)) #y position of "*"
    ant_pos <- subset(ant_pos, variable == "po") # only applied to PO data
    ant_pos$lab <- "*"
    ant_pos$co2 <- factor("amb", levels=c("amb", "elev")) 
    # the original data frame uses "co2", so it needs to have "temp" as well in ggplot2
    # but it doesn't really do anything    
    p3 +  geom_text(data = ant_pos, aes(x =date, y = Mean, label= lab), col = "black", vjust = 0)
  }
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

#############################################
# compare different auto-correlation models #
#############################################

atcr.cmpr <- function(model, rndmFac){
  if(rndmFac == "chamber/location"){
    model2 <- update(model,corr=corCompSymm(form=~1|chamber/location)) 
  } else {
    if(rndmFac == "chamber"){
      model2 <- update(model,corr=corCompSymm(form=~1|chamber))
    } else {
      model2 <- update(model,corr=corCompSymm(form=~1|id))
    }
  }
  
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
}

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse
bxplts <- function(value, ofst = 0, data){
  par(mfrow = c(2, 3))
  y <- data[[value]] + ofst #ofst is added to make y >0
  boxplot(y ~ temp*time, data)
  boxplot(log(y) ~ temp*time, main = "log", data)
  boxplot(sqrt(y) ~ temp*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ temp*time, main = "power(1/3)", data)
  boxplot(1/y ~ temp*time, main = "inverse", data)
  par(mfrow = c(1,1))
}
