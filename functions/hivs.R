#install the devtools package, which is required to install the HIEv package if necessary
#####
install.packages("devtools")
#load the devtools package, so that it can be used. 
library(devtools)
#install the HIEv package, and then load it so that it can be used
install_bitbucket("HIEv","remkoduursma",,ref="dev")

?install_bitbucket

library(HIEv)
#####
