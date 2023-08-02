#######################################
# Cross validation framework
# Mathieu Fortin - March 2020
#######################################

source("./DonneesPEP20200131/staticFunctions.R")
source("utilityFunctions.R")

load(getFilepath("plotIndex.RData"))

group <- ceiling(runif(length(plotIndex[,1]), min=0.000000001, max=10))
crossValidationFrame <- data.frame(plotIndex$newID_PE, group)
colnames(crossValidationFrame)<- c("newID_PE", "group")
table(crossValidationFrame$group)
save(crossValidationFrame, file = "./ProcessedData/GrowthModel/0_Database/CrossValidationFrame.RData")
