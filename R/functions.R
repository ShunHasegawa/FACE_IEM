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
  data <- read.csv(paste("Data/AQ2/NO3_NeedToBeCorrected/", filename, sep = ""), header = TRUE)
  
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
  
  unt <- substitute((ng~cm^"-2" ~ d^"-1"))
  ylabs <- c(expression(),
            bquote(IEM*-adsorbed~nutrients~.(unt)),
            bquote(atop(paste(IEM *- adsorbed ~ NO[3]^"-"), 
                            paste(.(unt)))), 
            bquote(atop(paste(IEM *- adsorbed ~ NH[4]^"+"), 
                            paste(.(unt)))), 
            bquote(atop(paste(IEM *- adsorbed ~ PO[4]^"3-"), 
                            paste(.(unt))))
            )
  # subsitute returens argument as it is without calculation (similar to expression())
  
#   yvars <- lapply(vars, function(x) bquote(IEM-adsorbed~.(x)))
#   # bquote allows one to call an object and return expression
#   
#   ylabs <- lapply(yvars, function(x) {
#     c(expression(), 
#       bquote(atop(paste(.(x)), paste((ng~cm^"-2"~d^"-1")))))         
#   })
#   
  # atop: put the 1st argument on top of the 2nd
  
  ylab <- ifelse(length(unique(data$variable)) > 1, ylabs[1],
                 ifelse(unique(data$variable) == "no", ylabs[2], 
                        ifelse(unique(data$variable) == "nh", ylabs[3],
                               ylabs[4])))
  
  colfactor <- ifelse(any(names(data) == "ring"), "ring", "co2")
  
  p <- ggplot(data, aes_string(x = "date", y = "Mean", col = colfactor))
  
  p2 <- p + 
    geom_rect(xmin = -Inf, 
            xmax = as.numeric(as.Date("2012-09-18")),
            ymin = -Inf, ymax = Inf,
            fill = "grey90", col = "grey90") +
    geom_line(size = 1.5, position = position_dodge(10), alpha = .8) + 
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = colfactor), 
                  width = 20,
                  position = position_dodge(10),
                  , alpha = .8) + 
    labs(x = "Month", y = ylab) +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-6-15", "2014-4-2"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))
  
  # change colors, linetype and associated legend according to plotting groups (ring or treatment)
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
    # the original data frame uses "co2", so it needs to have "co2" as well in ggplot2
    # but it doesn't really do anything    
    p3 +  geom_text(data = ant_pos, aes(x =date, y = Mean, label= lab), col = "black", vjust = 0)
  }
}

############################################
# Create df to add a stat table to figures #
############################################
StatPositionDF <- function(StatRes, variable, ytop, ylength, gap = .07){
  d <- data.frame(variable, ytop, gap = gap * ylength) 
  # ytop is y coordinate for the top (i.e. CO2) of the table for each fig 
  # (variable), ylength is the difference of max and min value of the plot (i.e.
  # max(mean+SE) - min(mean-SE)). 0.1 * ylength is used to determine the gap between each row
  # of the table
  
  predictor <- levels(StatRes$predictor)
  
  # create df which contains variable, predictor and y coordinates for the other
  # predictors (i.e. Time, CO2xTime) which is ylength*0.1 (= gap) lower than one above
  d2 <- ddply(d, .(variable),
              function(x){
                data.frame(predictor, 
                           ldply(1:length(predictor), function(z) x$ytop - z * x$gap))
              })
  names(d2)[3] <- "yval"
  
  # mege every thing
  d3 <- merge(d2, StatRes, by = c("variable", "predictor"))
  d3$co2 <- "amb" # co2 column is required for ggplot
  return(d3)
}

#######################
# Fig for publication #
#######################
# define graphic background
science_theme <- theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = c(.5, .93), 
                       legend.title = element_blank())

# white-black figure
WBFig <- function(data, ylab, facetLab = ylab_label, figTheme = science_theme, StatRes, StatY){
  # StatRes is stats tables to put on the figs; StatY is y coordinate for that tables
    
  # df for sub-labels
  subLabDF <- with(data, 
                   data.frame(xv = as.Date("2012-6-15"),
                              ddply(data, .(variable), summarise, yv = max(Mean + SE)),
                              labels = LETTERS[1:length(levels(variable))],
                              co2 = "amb"))
    # co2 is required as group = co2 is used in the main plot mapping
  
  
  # df for stat table
  ## compute ylength for each variable
  ylengthDF <- ddply(data, 
                     .(variable), 
                     function(x) 
                       data.frame(ylength = max(x$Mean +x$SE, na.rm = TRUE) -
                                    min(x$Mean - x$SE, na.rm = TRUE)))
  # ylength is given as the difference between max and min
  
  
  ## create df
  statDF <- StatPositionDF(StatRes = StatRes, 
                           variable = levels(ylengthDF$variable), 
                           ytop = StatY,
                           ylength = ylengthDF$ylength)
  # create a plot  
  p <- ggplot(data, aes(x = date, y = Mean, group = co2))
  
  p2 <- p + geom_line(aes(linetype = co2), position = position_dodge(20)) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  width = 15, size = .3,
                  position = position_dodge(20)) + 
    geom_point(aes(shape = co2, fill = co2), position = position_dodge(20)) +
    labs(x = "Month", y = ylab) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-6-15", "2014-4-2"))) +
    scale_shape_manual(values = c(24, 21), labels = c("Ambient", expression(eCO[2]))) +
    scale_fill_manual(values = c("black", "white"), 
                      labels = c("Ambient", expression(eCO[2]))) +
    scale_linetype_manual(values = c("solid", "dashed"), 
                          labels = c("Ambient", expression(eCO[2]))) +
    geom_text(aes(x = xv, y = yv * .95, label = labels),
              fontface = "bold",
              hjust = 1,
              data = subLabDF) +
    facet_grid(variable~., scales= "free_y", labeller= facetLab) +
    figTheme +
    geom_text(data = subset(statDF, predictor != ""), 
              aes(x = as.Date("2014-1-20"), y = yval, label = predictor),
              size = 2, hjust = 1, parse = TRUE) +
    # unless remove [" "] with predictor != "", labels will be messed up due to
    # this empty level
    geom_text(data = statDF, 
              aes(x = as.Date("2014-2-20"), y = yval, label = p), 
              size = 2, parse = TRUE)
  return(p2)
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
atcr.cmpr <- function(model){
  model2 <- update(model,corr=corCompSymm(form = model$call$random))
  model3 <- update(model,correlation=corARMA(q = 2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q = 1))
  a <- anova(model, model2, model3, model4, model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
}

##############################################
# Compare different random factor structures #
##############################################
RndmComp <- function(model){
  m2 <- update(model, random = ~ 1|block/ring)
  m3 <- update(model, random = ~ 1|block/id)
  m4 <- update(model, random = ~ 1|ring/plot)
  m5 <- update(model, random = ~ 1|ring)
  m6 <- update(model, random = ~ 1|id)
  ms <- list(model, m2, m3, m4, m5, m6)
  a <- anova(model, m2, m3, m4, m5, m6)
  rownames(a) <- sapply(ms, function(x) as.character(x$call$random[2]))
  ms[[length(ms) + 1]] <- a
  names(ms)[length(ms)] <- 'anova'
  return(ms)
}

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ co2 * time, data = data)
  par(mfrow = c(2, 3))
  boxplot(y ~ co2*time, data, main = "raw")
  boxplot(log(y) ~ co2*time, main = "log", data)
  boxplot(sqrt(y) ~ co2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ co2*time, main = "power(1/3)", data)
  boxplot(1/y ~ co2*time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ co2*time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

# multiple box-cox power plot for different constant values
bxcxplts <- function(value, data, sval, fval){
  data$yval <- data[[value]]
  ranges <- seq(sval, fval, (fval - sval)/9)
  
  # store parameters given from box-cox plot
  par(mfrow = c(5, 2))
  BCmax <- vector()
  for (i in 1:10){
    data$y <- data$yval + ranges[i]
    a <- boxcox(y ~ co2 * time, data = data)
    BCmax[i] <- a$x[a$y == max(a$y)]
  }
  
  # plot box plot with poer given from box-box for 
  # each contstant value
  par(mfrow = c(5, 2))
  par(omi = c(0, 0, 0, 0), mai = c(0.4, 0.4, 0.4, 0))
  sapply(1:10, function(x) {
    boxplot((yval + ranges[x]) ^ BCmax[x] ~ co2 * time, 
            main = "", data = data)
    texcol <- ifelse(BCmax[x] < 0, "red", "black") 
    title(main = paste("constant=", round(ranges[x], 4), 
                       ", boxcox=", round(BCmax[x], 4)),
          col.main = texcol)
  })
  par(mfrow = c(1,1))
}

####################################
# create table of contrast results #
####################################
cntrstTbl <- function(cntrstRes, data, variable, ...){
  d <- unique(data[, c("date", "time")])
  Df <- data.frame(
    d,
    contrast  =  cntrstRes$Contrast,
    SE = cntrstRes$SE,
    t = cntrstRes$testStat,
    df = cntrstRes$df,
    P.value = cntrstRes$Pvalue,
    FormatPval(cntrstRes$Pvalue),
    variable = variable)
  return(Df)
}

###############
# Print table #
###############
printTbl <- function(tbl, caption, label, ...){
  print(xtable(tbl,
               caption = caption, 
               label = label, 
               align = rep("l", ncol(tbl) + 1)),
        caption.placement = "top", 
        include.rownames = FALSE,
        table.placement = "H", ...)
}

printRngTbl <- function(tbl, caption, label, ...){
  printTbl(tbl[, 1:7], 
           caption = caption,
           label = label,
           ...)
  printTbl(tbl[, c(1, 8:13)], 
           caption = NULL,
           label = NULL,
           ...)
  printTbl(tbl[, c(1, 14:19)], 
           caption = NULL,
           label = NULL,
           ...)
}

##############################
# subset data and droplevels #
##############################
subsetD <- function(...){
  droplevels(subset(...))
}

##########################
# Run lme for each month #
##########################
LmeMonth <- function(data){
  m1 <- lme((p + 1.6)^(-1.1515) ~ co2 + Temp_Max, 
            random = ~1|ring,  data = data)
  return(anova(m1))
}

###############################################
# Plot soil variable for each incubation time #
###############################################
PltSoilVar <- function(data, var, tdrData, linealpha = .5){
  df <- ddply(data, c("date", var),
              function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")],
                                   na.rm = TRUE))
  SoilVarMlt <- melt(df, id = c(var, "date"))
  SoilVarMlt$type <- factor(ifelse(SoilVarMlt$variable != "Moist", "Temp", "Moist"))
  p <- ggplot(SoilVarMlt, aes_string(x = "date", y = "value", shape = "variable", col = var))
  pl <- p + geom_point() +
    facet_grid(type ~., scale = "free_y") +
    labs(x = "Time", y = NULL) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed", col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-7-1", "2014-4-2"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1)) +
    geom_line(aes_string(x = "Date", y = "value", group = var), data = tdrData, alpha = linealpha) +
    geom_vline(xintercept = c(unique(ave(as.numeric(data$insertion), data$time)), 
                              max(as.numeric(data$sampling))),
               col = "gray30", size = .5,linetype = "dotted")
    pl
}

########################################
# adjust moisture range for each block #
########################################
BlkminMoist <- function(variable, data){
  a <- range(subset(iem, !pre & block == variable)$Moist)
  df <- subset(data, block == variable & 
                 Moist <= a[2] & 
                 Moist >= a[1])
  return(df)
}

########################################################
# confidence interval for estimated parameters by lmer #
########################################################
CIdf <- function(model, method = "boot"){
  CIs <- confint(model, method = method)
  CIs <- CIs[-grep("sd|sigma", row.names(CIs)), ] 
  # take out estimates for fixed factors
  coefs <- summary(model)$coefficients
  ciDF <- cbind(CIs, Estimate = coefs[, "Estimate"])
  return(ciDF)
}  

######################################
# Plot predicted values from a model #
######################################
PltPrdVal <- function(data, model, variable, ...){
  a <- visreg(model,
              xvar = variable,
              by = "co2",
              overlay = TRUE, 
              print.cond=TRUE, 
              line.par = list(col = c("blue", "red")),
              points.par = list(col = c("blue", "red")),
              ...)
  
  timePos <- seq(min(a[[1]][[2]][["r"]]), # min of predicted values
                 max(a[[1]][[2]][["r"]]), 
                 length.out = 10)
  times <- c(5:14)
  data$yval <- data[, variable]
  for (i in 1:10){
    lines(x = range(data$yval[data$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(data$yval[data$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
  legend("topright", lty = 1, leg = paste(variable, "range"), bty = "n")
}

###########################
# step deletion with lmer #
###########################
stepLmer <- function(model, red.rndm = FALSE, ddf = "Kenward-Roger", ...){
  require(lmerTest)
  update(step(model, reduce.random = red.rndm, ddf = ddf,...)$model, 
         contrasts = NULL)
}
# use "Kenward-Roger" for approximation for denominator degrees of freedom. This
# is the same as the default DF given by Anova(model, test.statistic = "F). The
# default of step gives me a warning message for IEM-NO3 for some reasons (not
# sure why.. so changed it.)

###########################################
# Predict values for given temp and moist #
###########################################

# compute predicted values from the model using bootstrap. This process takes
# time
BtsCI <- function(model, MoistVal, TempVal, variable){
  expDF <- data.frame(co2 = c("amb", "elev"),
                      Moist = rep(MoistVal, 2),
                      Temp_Mean = rep(TempVal, 2))
  bb <- bootMer(model,
                FUN=function(x) predict(x, expDF, re.form = NA),
                nsim=500)
  lci <- apply(bb$t, 2, quantile, 0.025)
  uci <- apply(bb$t, 2, quantile, 0.975)
  PredVal <- bb$t0
  df <- cbind(lci, uci, PredVal, expDF, variable)
  return(df)
} 

#############################################
# Reshape estimated value and creat a table #
#############################################
ANCV_Tbl <- function(df, digits = 2, nsmall = 2){
  Est.val <- within(data.frame(df), {
    pred <- row.names(Est.val)
    co2 <- factor(ifelse(grepl("elev", pred), "elev", "amb"))
    predictor <- factor(ifelse(grepl("Temp", pred), "Temp",
                               ifelse(grepl("Moist", pred), "Moist", 
                                      "co2")))
  })
  names(Est.val)[c(1,2)] <- c("bCI", "tCI") 
  
  Est.val.mlt <- melt(Est.val, id = c("co2", "pred", "predictor"))
  Est.val.Cst <- cast(Est.val.mlt, predictor + co2~ variable)
  # format disimal numbers for each predictor
  formatDF <- ddply(Est.val.Cst, .(predictor), 
                    function(x) format(x, digits = digits, nsmall = nsmall))
  # concatenate CI and estimated values
  formatDF$val <- apply(formatDF, 1, function(x) 
    paste(x["Estimate"], "(", x["bCI"], ", ",x["tCI"], ")", sep = ""))
  
  tbl <- cast(formatDF, predictor ~ co2, value =  "val")
  return(tbl)
}

#######################################
# Excel sheet for Summary Stats table #
#######################################

CrSheetAnvTbl <- function(workbook, sheetName, smmaryLst){
  sheet <- createSheet(workbook, sheetName = sheetName)
  addDataFrame(data.frame("ANOVA_F"), sheet, col.names = FALSE, row.names = FALSE)
  addDataFrame(smmaryLst[[sheetName]][[1]], sheet, showNA = FALSE, 
               row.names = TRUE, characterNA="NA",
               startRow = 2)
  addDataFrame(data.frame("Coef"), sheet, 
               col.names = FALSE, row.names = FALSE,
               startRow = 10)
  addDataFrame(smmaryLst[[sheetName]][[2]], sheet, showNA = FALSE, 
               row.names = FALSE, characterNA="NA", startRow = 11)
}

##########################
# Plot predicated values #
##########################

## Scatter plot of predicated values and CI ##
ScatterPlot <- function(df, xval, breakn = 5, xlab, gridval){
  # breakn: spacing between xaxis ticks
  
  df$Moist <- df$Moist * 100
  postDF_Mlt$Moist <- postDF_Mlt$Moist * 100
  df$gridval <- df[, gridval]
  postDF_Mlt$gridval <- postDF_Mlt[, gridval]
  
  scatter <- ggplot(df, 
                    aes_string(x = xval, y = "PredVal", col = "co2",
                               fill = "co2", group = "co2")) +
    geom_line() +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .4, color = NA) +
    # color = NA removes the ribbon edge
    geom_point(data = postDF_Mlt, aes_string(x = xval, y = "log(value)"), 
               alpha = .6, size = 1) +
    scale_color_manual(values = c("blue", "red"), 
                       labels =c("Ambient", expression(eCO[2]))) +
    scale_fill_manual(values = c("blue", "red"), 
                      labels = c("Ambient", expression(eCO[2]))) +
    scale_x_continuous(breaks = pretty(df[, xval], n = breakn)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(.12, .96), 
          legend.title = element_blank(),
          legend.key.size = unit(.2, "inch"),
          legend.background = element_rect(fill = alpha('white', 0)),
          axis.title = element_text(face = "plain"),
          plot.margin=unit(c(0, 0.5, 0, 0), "lines")) +
    facet_grid(variable ~ gridval, scales = "free_y", labeller = label_parsed) +
    labs(x = xlab, y = expression(log(IEM*-adsorbed~nutrients~(ng~cm^"-2"~d^"-1"))))
  return(scatter)
} 


## Boxplot for environmental vairalbe (moisture and temperature) ##
envPlot <- function(val, ylab){
  postDF$Moist <- postDF$Moist * 100
  
  M <- seq(min(postDF[, val]), max(postDF[, val]), length.out = 4)
  DF <- data.frame(x = c(1:3), ymin = M[1:3], ymax = M[2:4])
  pl <- ggplot(DF, aes(xmin = x - .3, xmax = x + .3,
                       ymin = ymin, ymax = ymax)) +
    geom_rect(fill = "gray30") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none", 
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin=unit(c(0, 0.5, 0, 0), "lines")) +
    coord_flip() +
    labs(x = "", y = ylab)
  return(pl)
}

#######################
# Compute R2 for GLMM #
#######################
source("R/rsquaredglmm.R")

################################
# Return star based on P value #
################################
FormatPval <- function(Pval) {
  stars <- ifelse(Pval > .1, "",
                  ifelse(Pval > .05, ".",
                         ifelse(Pval > .01, "*",
                                ifelse(Pval > .001, "**",
                                       c("***")))))
  
  p <- as.character(ifelse(Pval > .1, round(Pval, 3),
                           ifelse(Pval < .001, "bold('<0.001')", 
                                  # shown with bold font. Note that inside of
                                  # bold needs to be in ''
                                  paste("bold(", round(Pval, 3), ")", sep = "'"))))
  return(data.frame(stars, p))
} 

########################################
# Create summary stat table from anova #
########################################
StatTable <- function(x, variable) { # x is anova result
  df <- data.frame(predictor = c(row.names(x)),
                   rbind(FormatPval(x$Pr)))
  
  # add a row for column name of the table in the fig 
  df <- rbind(df, data.frame(predictor = "", 
                             stars = "italic('P>F')", 
                             p = "italic('P>F')"))
  
  result <- merge(df, data.frame(predictor = c("co2", "time", "co2:time")), all = TRUE)
  
  # replace NA with ns
  result <- within(result, {
    p <- ifelse(is.na(p), "ns", as.character(p)) 
      # ifelse tries to return factor, so use as.character
    stars <- ifelse(is.na(stars), "ns", as.character(stars))
    })
    
  # relabel for plotting
  result$predictor <- factor(result$predictor, 
                             labels = c("", "CO[2]", "Time", "CO[2]*~x~Time"), 
                             levels = c("", "co2", "time", "co2:time"))
  result$variable <- variable
  result <- result[order(result$predictor), ]
  return(result)
}

###############################
# Compute block ratio (e-a)/a #
###############################
BlockRatio <- function(data){
  Rmean <- ddply(data, .(date, time, block, ring, co2, variable), summarise, Mean = mean(value, na.rm = TRUE))
  blockR <- ddply(Rmean, .(date, time, block, variable), summarise, R = Mean[co2 == "elev"]/Mean[co2 == "amb"]-1)
  blockRMean <- ddply(blockR, .(date, time, variable), summarise, value = mean(R, na.rm = TRUE))
  dcast(blockRMean, date+time~variable)
}
