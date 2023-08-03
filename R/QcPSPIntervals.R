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
