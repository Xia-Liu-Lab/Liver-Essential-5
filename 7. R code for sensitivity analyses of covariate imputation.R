#mice imputation
SCC<-read.csv()
mice<-SCC[,c(...)] 
library(mice)
set.seed(55555)
imp<-mice(mice,m=5)
mice<-complete(imp)