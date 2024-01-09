########################################################
# Clean database from PSP data.
#
# The tree remeasurements are reported by nonoverlapping
# intervals.
#
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: August 2023
########################################################

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to QcPSPIntervals!")
  packageStartupMessage("The QcPSPIntervals package provides a clean version of the PSP data from the Province of Quebec.")
  packageStartupMessage("These data can be used to fit growth models.")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
}

.onDetach <- function(libpath) {
}

.loadPackageData <- function(filename) {
  return(readRDS(system.file(paste0("extdata/",filename,".Rds"), package = "QcPSPIntervals")))
}

#'
#' Restore Quebec PSP Data by Intervals in the Global Environment.
#'
#' @description This function call creates five data.frame objects that contain
#' the tree remeasurements by nonoverlapping intervals.
#'
#' @details
#'
#' The five data.frame objects are: \cr
#' \itemize{
#' \item QcNbHarvestedTreesByInterval: the number of harvested trees by intervals \cr
#' \item QcNonoverlappingIntervals: the non overlapping intervals \cr
#' \item QcSpeciesGrouping: a suggested species grouping \cr
#' \item QcTreeRemeasurements: the tree remeasurements by nonoverlapping intervals \cr
#' \item QcClimateVariables: the climate variables by nonoverlapping intervals \cr
#' }
#'
#' @export
restoreQcPSPIntervalsData <- function() {
  QcPSP::restoreQcPSPData()
  assign("QcNbHarvestedTreesByIntervals", .loadPackageData("QcNbHarvestedTreesByIntervals"), envir = .GlobalEnv)
  assign("QcNonoverlappingIntervals", .loadPackageData("QcNonoverlappingIntervals"), envir = .GlobalEnv)
  assign("QcSpeciesGrouping", .loadPackageData("QcSpeciesGrouping"), envir = .GlobalEnv)
  assign("QcTreeRemeasurements", .loadPackageData("QcTreeRemeasurements"), envir = .GlobalEnv)
  assign("QcClimateVariables", .loadPackageData("QcClimateVariables"), envir = .GlobalEnv)
}


#'
#' Provide the metadata for any of the data.frame objects provided by
#' the QcPSPIntervals and QcPSP packages.
#'
#' @param dFrame a data.frame object. Any of these objects: QcNbHarvestedTreesByIntervals,
#' QcNonoverlappingIntervals, QcSpeciesGrouping, QcTreeRemeasurements, QcClimateVariables,
#' QcPlotIndex, QcTreeIndex, QcMeasurementIndex, or QcTreeMeasurements
#' @return a data.frame object containing the metadata.
#'
#' @export
getMetaData <- function(dFrame) {
  objectName <- deparse(substitute(dFrame))
  if (objectName == "QcNbHarvestedTreesByIntervals") {
    fieldNames <- c("kk", "nbHarvTrees")
    description <- c("Interval identifier.",
                     "Number of trees harvested in the plot during this interval.")
    return(data.frame(Field = fieldNames, Description = description))
  } else if (objectName == "QcNonoverlappingIntervals") {
    fieldNames <- c("newID_PE", "k.x", "year.x", "DATE_SOND.x", "ORIGINE.x", "PERTURB.x",
                    "k.y", "year.y", "DATE_SOND.y", "ORIGINE.y", "PERTURB.y", "correctedYear.x",
                    "correctedYear.y", "dt", "correctedDt", "kk")
    description <- c("PSP identifier after plot filtering and correction. Link to table QcPlotIndex.",
                     "Index of initial measurement of the interval. Link to table QcMeasurementIndex",
                     "Date (yr) of initial measurement.",
                     "Exact measurement date (YY-MM-DD) of initial measurement.",
                     "Stand-replacement disturbance code at initial measurement. See tab PERTURBATION in file DICTIONNAIRE_PLACETTE.xlsx.",
                     "Partial disturbance code at initial measurement. See tab PERTURBATION in file DICTIONNAIRE_PLACETTE.xlsx.",
                     "Index of final measurement of the interval. Link to table QcMeasurementIndex",
                     "Date (yr) of final measurement.",
                     "Exact measurement date (YY-MM-DD) of final measurement.",
                     "Stand-replacement disturbance code at final measurement. See tab PERTURBATION in file DICTIONNAIRE_PLACETTE.xlsx.",
                     "Partial disturbance code at final measurement. See tab PERTURBATION in file DICTIONNAIRE_PLACETTE.xlsx.",
                     "Corrected date (yr) of initial measurement. This correction is based on the growing season.",
                     "Corrected date (yr) of final measurement. This correction is based on the growing season.",
                     "Interval length (yr).",
                     "Corrected interval length (yr). This correction is based on the growing season.",
                     "Interval identifier")
    return(data.frame(Field = fieldNames, Description = description))
  } else if (objectName == "QcSpeciesGrouping") {
    fieldNames <- c("ESSENCE", "isRecruit", "isForSurv", "hasSurvived", "speciesGr")
    description <- c("Species code. See tab ESSENCES in file DICTIONNAIRE_PLACETTE.xlsx.",
                     "Number of observations that can be used to model recruitment.",
                     "Number of observations that can be used to model survival/mortality.",
                     "Number of observations that can be used to model diameter increment.",
                     "Assigned species group.")
    return(data.frame(Field = fieldNames, Description = description))
  } else if (objectName == "QcTreeRemeasurements") {
    fieldNames <- c("j", "kk", "dbhCm.x", "hauteurM.x", "BAL", "dbhCm.y",
                    "hauteurM.y", "STATUT.x", "STATUT.y", "isRecruit", "isForSurv", "hasSurvived", "dDbhCm",
                    "dHauteurM", "isForSaplingSurv", "hasSaplingSurvived", "isForHarv", "hasBeenHarvested")
    description <- c("Tree identifier. Refers to variable j in table QcTreeIndex.",
                     "Interval identifier. Link to table QcNonoverlappingIntervals.",
                     "DBH (cm) at initial measurement.",
                     "Height (m) at initial measurement.",
                     "Basal area (m2/ha) of trees with DBH larger than the subject at initial measurement.",
                     "DBH (cm) at final measurement.",
                     "Height (m) at final measurement.",
                     "Tree status at initial measurement (alive, dead, harvested, deadSapling, aliveSapling).",
                     "Tree status at final measurement (alive, dead, harvested, deadSapling, aliveSapling).",
                     "A binary variable that indicates whether the observation can be used to model recruitment.",
                     "A binary variable that indicates whether the observation can be used to model survival/mortality.",
                     "A binary variable that indicates whether the observation can be used to model diameter increment.",
                     "Diameter increment (cm) over the interval.",
                     "Height increment (m) over the interval.",
                     "A binary variable that indicates whether the observation can be used to model sapling survival/mortality.",
                     "A binary variable that indicates whether the observation can be used to model sapling diameter increment.",
                     "A binary variable that indicates whether the observation can be used to model harvesting.",
                     "A binary variable that indicates whether the tree has been harvested during the interval.")
    return(data.frame(Field = fieldNames, Description = description))
  } else if (objectName == "QcClimateVariables") {
    fieldNames <- c("IMPORTANT", "KeyID", "Latitude", "Longitude", "Elevation", "Year")
    description <- c("This object is a list that contains data.frame objects produced by BioSIM Web API.",
                     "Interval identifier. Refers to variable kk in table QcNonoverlappingIntervals.",
                     "Latitude (degrees) of the plot.",
                     "Longitude (degrees) of the plot.",
                     "Elevation (m) of the plot.",
                     "Date (yr) for which the climate variable is provided.")
    return(data.frame(Field = fieldNames, Description = description))
  } else {
    QcPSP::getMetaData(objectName)
  }
}


