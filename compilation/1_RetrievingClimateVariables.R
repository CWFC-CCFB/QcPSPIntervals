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
            "Climate_Mosture_Index_Annual")
additionalParms <- list(additionalParmsDegreeDays, NULL, NULL, NULL, NULL)

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
      output$DegreeDay_Annual <- rbind(output$DegreeDay_Annual, weather$DegreeDay_Annual)
      output$GrowingSeason <- rbind(output$GrowingSeason, weather$GrowingSeason)
      output$Climatic_Annual <- rbind(output$Climatic_Annual, weather$Climatic_Annual)
      output$Climatic_Monthly <- rbind(output$Climatic_Monthly, weather$Climatic_Monthly)
      output$Climate_Mosture_Index_Annual <- rbind(output$Climate_Mosture_Index_Annual, weather$Climate_Mosture_Index_Annual)
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

saveRDS(QcClimateVariables, file.path(getwd(), "inst", "extdata", "QcClimateVariables.Rds"), compress="xz")
shutdownClient()

#### Plot checkup ####

### TODO regenerate these plots

QcClimateVariables <- readRDS(file.path(getwd(), "inst", "extdata", "QcClimateVariables.Rds"))

prcp <- QcClimateVariables$Climatic_Annual
prcp <- prcp[which(prcp$TotalPrcp > 2000),] ### 169 obs (81 plots) with precipitation over 2000 mm
nrow(aggregate(KeyID ~ Latitude + Longitude, prcp, FUN="length"))
prcp <- prcp[which(prcp$TotalPrcp > 2500),] ### 43 obs (25 plots) with precipitation over 2500 mm
nrow(aggregate(KeyID ~ Latitude + Longitude, prcp, FUN="length"))
prcp <- prcp[which(prcp$TotalPrcp > 3000),] ### 6 obs (4 plots) with precipitation over
nrow(aggregate(KeyID ~ Latitude + Longitude, prcp, FUN="length"))


