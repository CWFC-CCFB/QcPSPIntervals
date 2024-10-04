###########################################
# Extracting climate variables
###########################################

rm(list=ls())

if (!require(QcPSP)) {
  library(remotes)
  remotes::install_github("CWFC-CCFB/QcPSP")
}

QcPSP::restoreQcPSPData()
QcNonoverlappingIntervals <- readRDS(file = file.path(getwd(), "inst", "extdata", "QcNonoverlappingIntervals.Rds"))

tmp <- merge(QcPlotIndex[,c("newID_PE","latitudeDeg","longitudeDeg", "elevationM")],
             QcNonoverlappingIntervals,
             by="newID_PE")

tmp <- tmp[order(tmp$year.x, tmp$year.y),]

possibleIntervals <- as.data.frame(table(tmp$year.x, tmp$year.y))
possibleIntervals <- possibleIntervals[which(possibleIntervals$Freq > 0),]
colnames(possibleIntervals) <- c("year.x", "year.y", "Freq")

if (!require(BioSIM)) {
  require(remotes)
  install_github("RNCan/BioSimClient_R")
  require(BioSIM)
}

additionalParmsDegreeDays <- c("LowerThreshold"=5)

##### Retrieving degree-days and growing season length from BIOSIM #####
nbPossibleIntervals <- length(possibleIntervals[,1])
models <- c("DegreeDay_Annual",
            "GrowingSeason",
            "Climatic_Annual",
            "Climatic_Monthly",
            "Climate_Mosture_Index_Annual",
            "Climate_Moisture_Index_Monthly",
            "VaporPressureDeficit_Monthly")
additionalParms <- list(additionalParmsDegreeDays, NULL, NULL, NULL, NULL, NULL, NULL)

models <- c("Soil_Moisture_Index_Monthly")
additionalParms <- list(NULL)

#i <- 1
output <- NULL
for (i in 1:nbPossibleIntervals) {
  subset <- tmp[which(tmp$year.x == possibleIntervals[i,"year.x"] &
                      tmp$year.y == possibleIntervals[i,"year.y"]),]
  print(paste("Interval", i ,"of", nbPossibleIntervals, paste0("(", possibleIntervals[i,"year.x"],",", possibleIntervals[i,"year.y"],")"), nrow(subset), "plot(s)"))
  nbRuns <- ceiling(nrow(subset) / 1000)
  for (j in 1:nbRuns) {
    j.min <- (j-1)*1000 + 1
    j.max <- j*1000
    if (j.max > nrow(subset)) {
      j.max = nrow(subset)
    }
    subset.j <- subset[j.min:j.max,]
    weather <- generateWeather(models,
                               as.integer(subset.j[1,"year.y"] - 20),
                               as.integer(subset.j[1,"year.y"]),
                               subset.j$kk,
                               subset.j$latitudeDeg,
                               subset.j$longitudeDeg,
                               as.numeric(subset.j$elevationM),
                               additionalParms = additionalParms)
    if (is.null(output)) {
      output <- weather
    } else {
      for (name in names(output)) {
        output[[name]] <- rbind(output[[name]], weather[[name]])
      }
    }
  }
}

QcClimateVariables <- output

if (nrow(QcClimateVariables$DegreeDay_Annual) != nrow(QcNonoverlappingIntervals) * 21) {
  stop("The number of rows in DegreeDay_Annual is inconsistent!")
}

if (nrow(QcClimateVariables$GrowingSeason) != nrow(QcNonoverlappingIntervals) * 21) {
  stop("The number of rows in GrowingSeason is inconsistent!")
}

if (nrow(QcClimateVariables$Climatic_Annual) != nrow(QcNonoverlappingIntervals) * 21) {
  stop("The number of rows in Climatic_Annual is inconsistent!")
}

if (nrow(QcClimateVariables$Climatic_Monthly) != nrow(QcNonoverlappingIntervals) * 21 * 12) {
  stop("The number of rows in Climatic_Monthly is inconsistent!")
}

if (nrow(QcClimateVariables$Climate_Mosture_Index_Annual) != nrow(QcNonoverlappingIntervals) * 20) {
  stop("The number of rows in Climate_Mosture_Index_Annual is inconsistent!")
}

if (nrow(QcClimateVariables$Climate_Moisture_Index_Monthly) != nrow(QcNonoverlappingIntervals) * 21 * 12) {
  stop("The number of rows in Climate_Moisture_Index_Monthly is inconsistent!")
}

if (nrow(QcClimateVariables$VaporPressureDeficit_Monthly) != nrow(QcNonoverlappingIntervals) * 21 * 12) {
  stop("The number of rows in Climate_Moisture_Index_Monthly is inconsistent!")
}

if (nrow(QcClimateVariables$Soil_Moisture_Index_Monthly) != nrow(QcNonoverlappingIntervals) * 21 * 12) {
  stop("The number of rows in Soil_Moisture_Index_Monthly is inconsistent!")
}

for (n in names(QcClimateVariables)) {
  dataset <- QcClimateVariables[[n]]
  fieldsToKeep <- colnames(dataset)[which(!colnames(dataset) %in% c("Latitude", "Longitude", "Elevation", "Rep", "DataType"))]
  dataset <- dataset[,fieldsToKeep]
  colnames(dataset)[which(colnames(dataset) == "KeyID")] <- "kk"
  message(paste("Saving", n, "to file..."))
  saveRDS(dataset, file.path(getwd(), "inst", "extdata", paste0("QcClimateVariables",n,".Rds")), compress="xz")
  rm(dataset)
  rm(fieldsToKeep)
  message(paste("Done."))
}

shutdownClient()

#### Plot checkup ####

### TODO regenerate these plots

prcp <- readRDS(file.path(getwd(), "inst", "extdata", "QcClimateVariablesClimatic_Annual.Rds"))

prcp <- prcp[which(prcp$TotalPrcp > 2000),] ### 170 obs with precipitation over 2000 mm
prcp <- prcp[which(prcp$TotalPrcp > 2500),] ### 45 obs with precipitation over 2500 mm
prcp <- prcp[which(prcp$TotalPrcp > 3000),] ### 9 obs with precipitation over 3000 mm


