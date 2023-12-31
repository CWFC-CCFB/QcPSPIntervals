###########################################
# Formatting the database for the fitting
# of a climate-sensitive growth model
# Mathieu Fortin - December 2019
#   @output nonoverlappingIntervals.RData
#   @output dataBase.RData
#   @output speciesGrouping.RData
###########################################

rm(list=ls())

if (!require(QcPSP)) {
  library(remotes)
  remotes::install_github("CWFC-CCFB/QcPSP")
}
source("./compilation/utilityFunctions.R")

#### Creating non overlapping intervals ####

QcPSP::restoreQcPSPData()

output <- NULL
for (measurement in 2:max(QcMeasurementIndex$newNO_MES)) {
  mes1 <- QcMeasurementIndex[which(QcMeasurementIndex$newNO_MES == measurement - 1), c("newID_PE", "k", "year", "DATE_SOND", "ORIGINE", "PERTURB")]
  mes2 <- QcMeasurementIndex[which(QcMeasurementIndex$newNO_MES == measurement), c("newID_PE", "k", "year", "DATE_SOND", "ORIGINE", "PERTURB")]
  tmp <- merge(mes1, mes2, by="newID_PE")
  print(paste(measurement-1, "to",  measurement, "; Nb initial mesurements", length(mes1[,1]), "; Nb final measurements", length(mes2[,1]), "; Nb matches", length(tmp[,1])))
  output <- rbind(output, tmp)
}

nonoverlappingIntervals <- output ## 37 614 obs.

removeAllExcept(c("nonoverlappingIntervals"))

nonoverlappingIntervals$correctedYear.x <- getCorrectedYear(nonoverlappingIntervals, "DATE_SOND.x")
nonoverlappingIntervals$correctedYear.y <- getCorrectedYear(nonoverlappingIntervals, "DATE_SOND.y")

#length(which(abs(nonoverlappingIntervals$year.x - nonoverlappingIntervals$correctedYear.x) > .0001))
#length(which(abs(nonoverlappingIntervals$year.y - nonoverlappingIntervals$correctedYear.y) > .0001))

nonoverlappingIntervals$dt <- nonoverlappingIntervals$year.y - nonoverlappingIntervals$year.x
nonoverlappingIntervals$correctedDt <- nonoverlappingIntervals$correctedYear.y - nonoverlappingIntervals$correctedYear.x

nonoverlappingIntervals <- nonoverlappingIntervals[order(nonoverlappingIntervals$k.x, nonoverlappingIntervals$k.y),]
nonoverlappingIntervals$kk <- 1:length(nonoverlappingIntervals[,1])

QcNonoverlappingIntervals <- nonoverlappingIntervals
rm(nonoverlappingIntervals)

nonoverlappingIntervalsFilename <- file.path(getwd(), "inst", "extdata", "QcNonoverlappingIntervals.Rds")

if (file.exists(nonoverlappingIntervalsFilename)) {
  message(paste("Comparing file with reference version..."))
  newQcNonoverlappingIntervals <- QcNonoverlappingIntervals
  rm(QcNonoverlappingIntervals)
  QcNonoverlappingIntervals <- readRDS(file = nonoverlappingIntervalsFilename)
  print(compareTwoDataFrame(newQcNonoverlappingIntervals, QcNonoverlappingIntervals))
  QcNonoverlappingIntervals <- newQcNonoverlappingIntervals
} else {
  message(paste("File", nonoverlappingIntervalsFilename, "does not exist. It will be created."))
}

saveRDS(QcNonoverlappingIntervals, file = nonoverlappingIntervalsFilename, compress = "xz")

removeAllExcept(c("QcNonoverlappingIntervals"))

#### Adding trees ####

QcPSP::restoreQcPSPData()
commonFields <- c("kk", "k.x", "k.y", "year.x", "year.y")
treeInfo1 <- merge(QcNonoverlappingIntervals[, commonFields],
                   QcTreeMeasurements,
                   by.x=c("k.x"), by.y = c("k"), all.x = T) ## 1 470 781 obs
treeInfo2 <- merge(QcNonoverlappingIntervals[, commonFields],
                   QcTreeMeasurements[, c("j","k","ETAT","dbhCm","hauteurM")],
                   by.x=c("k.y"), by.y = c("k"), all.x = T) ## 1 650 696 obs
treeInfo <- merge(treeInfo1, treeInfo2, by=c(commonFields, "j"), all = T) ## 1 846 428 obs and 13 variable
treeInfo <- merge(treeInfo, QcTreeIndex[,c("j", "minYear", "maxYear", "intruder", "IN_1410")], by=c("j"), all.x = T)
rm(list = c("treeInfo1", "treeInfo2"))

table(treeInfo$intruder, useNA = "always") ### TODO deal with the 640 empty plot remeasured when intruder is missing
treeInfo <- treeInfo[which(treeInfo$intruder == F | is.na(treeInfo$intruder)),] ## dropping intruders and empty plots - 1 836 268 obs left.

dead <- c("14", "15", "16", "23", "24", "29", "34", "35", "36", "44", "45", "46", "54", "55", "56", "GM")
harvested <- c("26")
notAlreadyDead <- which(!(treeInfo$ETAT.x %in% c(dead,harvested)) | !(treeInfo$ETAT.y %in% c(dead,harvested)))
treeInfo <- treeInfo[notAlreadyDead,] ## dropping trees already dead or missing 1 763 571 obs left.
rm(list = c("notAlreadyDead"))

notAlreadyDead <- which(!(treeInfo$ETAT.x %in% c(dead, harvested)) | !is.na(treeInfo$ETAT.y))
treeInfo <- treeInfo[notAlreadyDead,] ## dropping trees already dead or missing 1 572 320 obs left.
rm(list = c("notAlreadyDead"))

notAbandoned <- which(treeInfo$ETAT.y != "GA")
treeInfo <- treeInfo[notAbandoned,] ## dropping abandoned saplings 1 496 211 obs left.
rm(list = c("notAbandoned"))
table(treeInfo$ETAT.x, useNA = "always")
table(treeInfo$ETAT.y, useNA = "always")

alive <- c("10","12","30","32","40","42","50","52")
dead <- c("14", "15", "16", "23", "24", "29", "34", "35", "36", "44", "45", "46", "54", "55", "56")
harvested <- c("26")
deadSapling <- c("GM")
aliveSapling <- c("GV")
treeInfo[which(treeInfo$ETAT.x %in% alive),"STATUT.x"] <- "alive"
treeInfo[which(treeInfo$ETAT.x %in% dead),"STATUT.x"] <- "dead"
treeInfo[which(treeInfo$ETAT.x %in% harvested),"STATUT.x"] <- "harvested"
treeInfo[which(treeInfo$ETAT.x %in% deadSapling),"STATUT.x"] <- "deadSapling"
treeInfo[which(treeInfo$ETAT.x %in% aliveSapling),"STATUT.x"] <- "aliveSapling"
table(treeInfo$ETAT.x, useNA="always")
table(treeInfo$STATUT.x, useNA="always")

treeInfo[which(treeInfo$ETAT.y %in% alive),"STATUT.y"] <- "alive"
treeInfo[which(treeInfo$ETAT.y %in% dead),"STATUT.y"] <- "dead"
treeInfo[which(treeInfo$ETAT.y %in% harvested),"STATUT.y"] <- "harvested"
treeInfo[which(treeInfo$ETAT.y %in% deadSapling),"STATUT.y"] <- "deadSapling"
treeInfo[which(treeInfo$ETAT.y %in% aliveSapling),"STATUT.y"] <- "aliveSapling"
table(treeInfo$ETAT.y, useNA="always")
table(treeInfo$STATUT.y, useNA="always")

treeInfo[which((is.na(treeInfo$ETAT.x) | treeInfo$ETAT.x == "") & treeInfo$dbhCm.x < 9.1), "STATUT.x"] <- "aliveSapling"
treeInfo[which((is.na(treeInfo$ETAT.y) | treeInfo$ETAT.y == "") & treeInfo$dbhCm.y < 9.1), "STATUT.y"] <- "aliveSapling"

deadOrHarvestedAtFirstMeasurement <- which(is.na(treeInfo$ETAT.x) & treeInfo$ETAT.y %in% c(dead,harvested) & treeInfo$year.y == treeInfo$minYear)
notDeadOrHarvestedArFirstMeasurement <- setdiff(1:length(treeInfo[,1]), deadOrHarvestedAtFirstMeasurement)
treeInfo <- treeInfo[notDeadOrHarvestedArFirstMeasurement, ] ### 1 479 491 obs left

aliveSaplingAtFirstMeasurement <- which(is.na(treeInfo$ETAT.x) & treeInfo$STATUT.y == "aliveSapling" & treeInfo$year.y == treeInfo$minYear)
notAliveSaplingAtFirstMeasurement <- setdiff(1:length(treeInfo[,1]), aliveSaplingAtFirstMeasurement)
treeInfo <- treeInfo[notAliveSaplingAtFirstMeasurement,] ### 1 479 491 obs
table(treeInfo$STATUT.x, treeInfo$STATUT.y, useNA = "always")

deadSaplingAtFirstMeasurement <- which(is.na(treeInfo$ETAT.x) & treeInfo$STATUT.y == "deadSapling" & treeInfo$year.y == treeInfo$minYear)
notAliveSaplingAtFirstMeasurement <- setdiff(1:length(treeInfo[,1]), deadSaplingAtFirstMeasurement)
treeInfo <- treeInfo[notAliveSaplingAtFirstMeasurement,] ### 1 479 487 obs
table(treeInfo$STATUT.x, treeInfo$STATUT.y, useNA = "always")

NAFollowedByDead <- which(is.na(treeInfo$ETAT.x) & treeInfo$STATUT.x != "aliveSapling" & treeInfo$STATUT.y == "dead" & treeInfo$year.y != treeInfo$minYear)
notNAFollowedByDead <- setdiff(1:length(treeInfo[,1]), NAFollowedByDead)
treeInfo <- treeInfo[notNAFollowedByDead,] ### 1 479 487 obs
table(treeInfo$STATUT.x, treeInfo$STATUT.y, useNA = "always")


## Identifying records where recruitment occurs
treeInfo$isRecruit <- F
monitoredSaplingsThatGrewUpToCommercialIndex <- which(treeInfo$STATUT.x == "aliveSapling" & treeInfo$STATUT.y == "alive") ### 15 922 occurrences
recruitIndex <- which(is.na(treeInfo$STATUT.x) & treeInfo$ETAT.y %in% c("10","12","40","42") & treeInfo$IN_1410 == "N") ### 298 495 occurrences
treeInfo[c(monitoredSaplingsThatGrewUpToCommercialIndex, recruitIndex),"isRecruit"] <- T

## Identifying records for survival analysis
treeInfo$isForSurv <- F
treeInfo[which(treeInfo$STATUT.x == "alive" & treeInfo$STATUT.y %in% c("alive", "aliveSapling", "dead", "deadSapling")), "isForSurv"] <- T
treeInfo[which(treeInfo$isForSurv & treeInfo$STATUT.y %in% c("alive","aliveSapling")), "hasSurvived"] <- T
treeInfo[which(treeInfo$isForSurv & treeInfo$STATUT.y %in% c("dead", "deadSapling")), "hasSurvived"] <- F
table(treeInfo$isForSurv, treeInfo$hasSurvived, useNA="always")

## Identifying records for survivor growth analysis
indexSurvivor <- which(treeInfo$hasSurvived)
treeInfo[indexSurvivor, "dDbhCm"] = treeInfo[indexSurvivor, "dbhCm.y"] - treeInfo[indexSurvivor, "dbhCm.x"]
treeInfo[indexSurvivor, "dHauteurM"] = treeInfo[indexSurvivor, "hauteurM.y"] - treeInfo[indexSurvivor, "hauteurM.x"]

## Identifying records for sapling survival
treeInfo$isForSaplingSurv <- F
treeInfo[which(treeInfo$STATUT.x == "aliveSapling" & treeInfo$STATUT.y %in% c("alive", "aliveSapling", "dead", "deadSapling")), "isForSaplingSurv"] <- T
treeInfo[which(treeInfo$isForSaplingSurv & treeInfo$STATUT.y %in% c("alive","aliveSapling")), "hasSaplingSurvived"] <- T
treeInfo[which(treeInfo$isForSaplingSurv & treeInfo$STATUT.y %in% c("dead", "deadSapling")), "hasSaplingSurvived"] <- F
table(treeInfo$isForSaplingSurv, treeInfo$hasSaplingSurvived, useNA="always")

## Identifying records for sapling growth
indexSaplingSurvivor <- which(treeInfo$hasSaplingSurvived)
treeInfo[indexSaplingSurvivor, "dDbhCm"] = treeInfo[indexSaplingSurvivor, "dbhCm.y"] - treeInfo[indexSaplingSurvivor, "dbhCm.x"]
treeInfo[indexSaplingSurvivor, "dHauteurM"] = treeInfo[indexSaplingSurvivor, "hauteurM.y"] - treeInfo[indexSaplingSurvivor, "hauteurM.x"]

## Identifying records for harvesting analysis
treeInfo$isForHarv <- F
treeInfo[which(treeInfo$STATUT.x == "alive" & treeInfo$STATUT.y %in% c("alive", "aliveSapling", "harvested")), "isForHarv"] <- T
treeInfo[which(treeInfo$isForHarv & treeInfo$STATUT.y %in% c("alive","aliveSapling")), "hasBeenHarvested"] <- F
treeInfo[which(treeInfo$isForHarv & treeInfo$STATUT.y %in% c("harvested")), "hasBeenHarvested"] <- T
table(treeInfo$isForHarv, treeInfo$hasBeenHarvested, useNA="always")


table(treeInfo$isRecruit | treeInfo$isForSurv | treeInfo$isForSaplingSurv)  ### should be 1 371 605 TRUE
table(treeInfo$isRecruit | treeInfo$isForSurv | treeInfo$isForSaplingSurv | treeInfo$isForHarv) ### should be 1 464 225 TRUE

notAssignedYet <- treeInfo[which(!treeInfo$isRecruit & !treeInfo$isForSurv & !treeInfo$isForSaplingSurv & !treeInfo$isForHarv & treeInfo$IN_1410 == "N"),] # 5 632 obs mainly forgotten or renumbered trees

QcTreeRemeasurements <- treeInfo

treeRemeasurementsFilename <- file.path(getwd(), "inst", "extdata", "QcTreeRemeasurements.Rds")
fieldsToKeep <- colnames(QcTreeRemeasurements)[!colnames(QcTreeRemeasurements) %in% c("k.x", "k.y", "year.x", "year.y", "ETAT.x", "ETAT.y", "minYear", "maxYear", "intruder", "IN_1410")]
QcTreeRemeasurements <- QcTreeRemeasurements[, fieldsToKeep]

if (file.exists(treeRemeasurementsFilename)) {
  message(paste("Comparing file with reference version..."))
  newQcTreeRemeasurements <- QcTreeRemeasurements
  rm(QcTreeRemeasurements)
  QcTreeRemeasurements <- readRDS(file = treeRemeasurementsFilename)
  print(compareTwoDataFrame(newQcTreeRemeasurements, QcTreeRemeasurements))
  QcTreeRemeasurements <- newQcTreeRemeasurements
} else {
  message(paste("File", treeRemeasurementsFilename, "does not exist yet. It will be created."))
}

saveRDS(QcTreeRemeasurements, file = treeRemeasurementsFilename, compress = "xz")

removeAllExcept(c("QcTreeRemeasurements"))

#### Recording intervals with harvesting ####

harvestedTrees <- QcTreeRemeasurements[which(QcTreeRemeasurements$isForHarv & QcTreeRemeasurements$hasBeenHarvested),] ### 92 620 harvested trees
nbHarvestedTreesByInterval <- aggregate(j ~ kk, harvestedTrees, FUN="length") ### 5074 intervals with harvesting
colnames(nbHarvestedTreesByInterval) <- c("kk", "nbHarvTrees")


QcNbHarvestedTreesByIntervals <- nbHarvestedTreesByInterval
rm(nbHarvestedTreesByInterval)

nbHarvestedTreesFilename <- file.path(getwd(),"inst", "extdata", "QcNbHarvestedTreesByIntervals.Rds")

if (file.exists(nbHarvestedTreesFilename)) {
  message(paste("Comparing file with reference version..."))
  newQcNbHarvestedTreesByIntervals <- QcNbHarvestedTreesByIntervals
  rm(QcNbHarvestedTreesByIntervals)
  QcNbHarvestedTreesByIntervals <- readRDS(file = nbHarvestedTreesFilename)
  print(compareTwoDataFrame(newQcNbHarvestedTreesByIntervals, QcNbHarvestedTreesByIntervals))
  QcNbHarvestedTreesByIntervals <- newQcNbHarvestedTreesByIntervals
} else {
  message(paste("File", nbHarvestedTreesFilename, "does not exist. It will be created."))
}

saveRDS(QcNbHarvestedTreesByIntervals, file = nbHarvestedTreesFilename, compress = "xz")

removeAllExcept(c("QcTreeRemeasurements"))

#### Defining species group ####

rm(list = ls())
source("./compilation/utilityFunctions.R")

QcPSP::restoreQcPSPData()
ESSENCE <- unique(QcTreeIndex$ESSENCE) # to have all the species in the database

QcTreeRemeasurements <- readRDS(file = file.path(getwd(), "inst", "extdata", "QcTreeRemeasurements.Rds"))
QcTreeRemeasurements <- merge(QcTreeIndex[,c("j", "ESSENCE")], QcTreeRemeasurements, by = "j")

getFrequencies <- function(dataSet, field) {
  tmp <- dataSet[which(dataSet[,field]),]
  tmp <- as.data.frame(table(tmp$ESSENCE, useNA = "always"))
  colnames(tmp) <- c("ESSENCE", field)
  return(tmp)
}

recruits <- getFrequencies(QcTreeRemeasurements, "isRecruit")
survival <- getFrequencies(QcTreeRemeasurements, "isForSurv")
growth <- getFrequencies(QcTreeRemeasurements, "hasSurvived")
frame <- data.frame(ESSENCE)
allFrequencies <- merge(frame, recruits, by="ESSENCE", all.x = T)
allFrequencies <- merge(allFrequencies, survival, by="ESSENCE", all.x = T)
allFrequencies <- merge(allFrequencies, growth, by="ESSENCE", all.x = T)
allFrequencies <- allFrequencies[which(allFrequencies$ESSENCE != ""),]
for (i in 1:ncol(allFrequencies)) {
  allFrequencies[which(is.na(allFrequencies[,i])),i] <- 0
}

print(allFrequencies)
allFrequencies$speciesGr <- allFrequencies$ESSENCE
allFrequencies[which(allFrequencies$ESSENCE %in% c("AME", "AUC", "AUR", "COA", "CRA", "MAS", "PRV", "RHS", "RHT")), "speciesGr"] <- "SmallTrees"
allFrequencies[which(allFrequencies$ESSENCE %in% c("SOA", "SOD")), "speciesGr"] <- "Sorbus"
allFrequencies[which(allFrequencies$ESSENCE %in% c("CAC", "CAF")), "speciesGr"] <- "Carya"
allFrequencies[which(allFrequencies$ESSENCE %in% c("CHB", "CHE", "CHG")), "speciesGr"] <- "Quercus"
allFrequencies[which(allFrequencies$ESSENCE %in% c("ERN", "ERG", "NOC", "ORR",
                                                   "ORT", "CAR", "CEO", "PED")), "speciesGr"] <- "SouthernSpecies"

allFrequenciesAggregatedSpecies <- merge(merge(aggregate(isRecruit ~ speciesGr, allFrequencies, FUN = "sum"),
      aggregate(isForSurv ~ speciesGr, allFrequencies, FUN = "sum"),
      by = "speciesGr"),
      aggregate(hasSurvived ~ speciesGr, allFrequencies, FUN = "sum"),
      by = "speciesGr")

allFrequenciesAggregatedSpecies <- allFrequenciesAggregatedSpecies[which(allFrequenciesAggregatedSpecies$isRecruit > 0 |
                                                                           allFrequenciesAggregatedSpecies$isForSurv > 0 |
                                                                           allFrequenciesAggregatedSpecies$hasSurvived > 0), ]

QcSpeciesGrouping <- allFrequencies

speciesGroupingFilename <- file.path(getwd(), "inst", "extdata", "QcSpeciesGrouping.Rds")

if (file.exists(speciesGroupingFilename)) {
  message(paste("Comparing with reference file."))
  newQcSpeciesGrouping <- QcSpeciesGrouping
  rm(QcSpeciesGrouping)
  QcSpeciesGrouping <- readRDS(file = speciesGroupingFilename)
  print(compareTwoDataFrame(newQcSpeciesGrouping, QcSpeciesGrouping))
  QcSpeciesGrouping <- newQcSpeciesGrouping
} else {
  message(paste("File", speciesGroupingFilename, "does not exist. It will be created."))
}

saveRDS(QcSpeciesGrouping, file = speciesGroupingFilename, compress = "xz")



