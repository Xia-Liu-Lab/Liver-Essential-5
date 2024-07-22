
#Basic characteristics from ISSCC
library(tableone)
data<-read.csv() #ISSCC data set
catVars<-c() #define categorical variables
tab1 <- CreateTableOne(data = data, factorVars = catVars)
tab1Mat<-print(tab1,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE)
write.csv(tab1Mat,"SCC tableone.csv") #overall

tab2<-CreateTableOne(strata="pattern",data=data,factorVars=catVars)
tab2Mat <- print(tab2,quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE)
write.csv(tab2Mat,"SCC tableone-according to sleep.csv") #according to sleep quality