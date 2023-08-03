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


#'
#' A data.frame object containing the number of harvested trees
#' by intervals.
#'
#' @docType data
#'
#' @usage data(QcNbHarvestedTreesByIntervals)
#'
#' @keywords datasets
#'
#' @examples
#' data(QcNbHarvestedTreesByIntervals)
"QcNbHarvestedTreesByIntervals"

#'
#' A data.frame object containing the nonoverlapping intervals.
#'
#' @docType data
#'
#' @usage data(QcNonoverlappingIntervals)
#'
#' @keywords datasets
#'
#' @examples
#' data(QcNonoverlappingIntervals)
"QcNonoverlappingIntervals"

#'
#' A data.frame object containing the species grouping.
#'
#' @docType data
#'
#' @usage data(QcSpeciesGrouping)
#'
#' @keywords datasets
#'
#' @examples
#' data(QcSpeciesGrouping)
"QcSpeciesGrouping"

#'
#' A data.frame object containing the tree remeasurements.
#'
#' @docType data
#'
#' @usage data(QcTreeRemeasurements)
#'
#' @keywords datasets
#'
#' @examples
#' data(QcTreeRemeasurements)
"QcTreeRemeasurements"

#'
#' A data.frame object containing the climate variables associated
#' with the nonoverlapping intervals.
#'
#' @docType data
#'
#' @usage data(QcClimateVariables)
#'
#' @keywords datasets
#'
#' @examples
#' data(QcClimateVariables)
"QcClimateVariables"


#'
#' Restore Quebec PSP data by intervals in the global environment.
#'
#' @export
restoreQcPSPIntervalsData <- function() {
  QcPSP::restoreQcPSPData()
  assign("QcNbHarvestedTreesByInterval", QcPSPIntervals::QcNbHarvestedTreesByIntervals, envir = .GlobalEnv)
  assign("QcNonoverlappingIntervals", QcPSPIntervals::QcNonoverlappingIntervals, envir = .GlobalEnv)
  assign("QcSpeciesGrouping", QcPSPIntervals::QcSpeciesGrouping, envir = .GlobalEnv)
  assign("QcTreeRemeasurements", QcPSPIntervals::QcTreeRemeasurements, envir = .GlobalEnv)
  assign("QcClimateVariables", QcPSPIntervals::QcClimateVariables, envir = .GlobalEnv)
}
