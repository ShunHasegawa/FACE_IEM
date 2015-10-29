library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(plyr)
library(reshape2)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(scales)
library(xtable)
library(visreg)
library(lmerTest)
library(gridExtra)
# library(foreach)
# library(doParallel) # set up parallel backend for foreach
# library(snow) # require to difine makeCluster for foreach
# library(doSNOW)
library(grid)
library(quantmod) # compute % change
library(boot)

# install.packages("devtools")
# require(devtools)
devtools::install_github("hadley/ggplot2") 
# devtools::install_github("jrnold/ggthemes")
# # library(ggthemes)
# install.packages("https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_1.0.0.tar.gz",
#                  repos=NULL, type="source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/gridExtra/gridExtra_0.9.tar.gz",
#                  repos=NULL, type="source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/scales/scales_0.2.4.tar.gz",
#                  repos=NULL, type="source")
# install.packages("gridExtra")
