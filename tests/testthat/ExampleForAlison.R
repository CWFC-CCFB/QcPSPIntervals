library(QcPSPIntervals)
restoreQcPSPIntervalsData()
selectedSpecies <- c("CHR", "PRU", "THO")
treeRem <- merge(QcTreeIndex[,c("j", "ESSENCE")], QcTreeRemeasurements, by="j")
selectedTreeRemeasurements <- treeRem[which(treeRem$ESSENCE %in% selectedSpecies),]

selectedTreeRemeasurements <- merge(QcNonoverlappingIntervals, selectedTreeRemeasurements, by="kk") # you get the plot identifier and the measurement dates
selectedTreeRemeasurements <- merge(selectedTreeRemeasurements, QcMeasurementIndex[,c("k", "N_TOT", "G_TOT")], by.x = "k.x", by.y = "k") # you get the initial per hectare stem density (N_TOT) and bsaal area (G_TOT)


