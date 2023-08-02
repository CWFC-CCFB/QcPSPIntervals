###########################################
# Extracting climate variables
###########################################

rm(list=ls())
#source("./DonneesPEP20200131/staticFunctions.R")
#source("utilityFunctions.R")

QcPlotIndex <- QcPSP::plotIndex
load(file = file.path(getwd(), "data", "nonoverlappingIntervals.RData"))

tmp <- merge(QcPlotIndex[,c("newID_PE","latitudeDeg","longitudeDeg", "elevationM")],
             nonoverlappingIntervals,
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
            "Climate_Mosture_Index_Annual")
additionalParms <- list(additionalParmsDegreeDays, NULL, NULL, NULL, NULL)

#i <- 1
output <- NULL
for (i in 1:nbPossibleIntervals) {
  print(paste("Possible intervals", i ,"of", nbPossibleIntervals))
  subset <- tmp[which(tmp$year.x == possibleIntervals[i,"year.x"] &
                      tmp$year.y == possibleIntervals[i,"year.y"]),]
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
    weather$janJuly <- weather$Climatic_Monthly[which(weather$Climatic_Monthly$Month %in% c(1,7)),]
    weather[["Climatic_Monthly"]] <- NULL
    if (is.null(output)) {
      output <- weather
    } else {
      output$DegreeDay_Annual <- rbind(output$DegreeDay_Annual, weather$DegreeDay_Annual)
      output$GrowingSeason <- rbind(output$GrowingSeason, weather$GrowingSeason)
      output$Climatic_Annual <- rbind(output$Climatic_Annual, weather$Climatic_Annual)
      output$janJuly <- rbind(output$janJuly, weather$janJuly)
      output$Climate_Mosture_Index_Annual <- rbind(output$Climate_Mosture_Index_Annual, weather$Climate_Mosture_Index_Annual)
    }
  }
}

climateVariables20221220 <- output
save(climateVariables20221220, file = "./ProcessedData/GrowthModel/0_Database/climateVariables20221220.RData")
shutdownJava()

