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
#' @usage data(nbHarvestedTreesByInterval)
#'
#' @keywords datasets
#'
#' @examples
#' QcNbHarvestedTreesByInterval <- QcPSPIntervals::nbHarvestedTreesByInterval
"nbHarvestedTreesByInterval"

#'
#' A data.frame object containing the nonoverlapping intervals.
#'
#' @docType data
#'
#' @usage data(nonoverlappingIntervals)
#'
#' @keywords datasets
#'
#' @examples
#' QcNonoverlappingIntervals <- QcPSPIntervals::nonoverlappingIntervals
"nonoverlappingIntervals"

#'
#' A data.frame object containing the species grouping.
#'
#' @docType data
#'
#' @usage data(speciesGrouping)
#'
#' @keywords datasets
#'
#' @examples
#' QcSpecinesGrouping <- QcPSPIntervals::speciesGrouping
"speciesGrouping"

#'
#' A data.frame object containing the tree remeasurements.
#'
#' @docType data
#'
#' @usage data(treeRemeasurements)
#'
#' @keywords datasets
#'
#' @examples
#' QcTreeRemeasurements <- QcPSPIntervals::treeRemeasurements
"treeRemeasurements"


#'
#' Restore Quebec PSP data by intervals in the global environment.
#'
#' @export
restoreQcPSPIntervalsData <- function() {
  QcPSP::restoreQcPSPData()
  assign("QcNbHarvestedTreesByInterval", QcPSPIntervals::nbHarvestedTreesByInterval, envir = .GlobalEnv)
  assign("QcNonoverlappingIntervals", QcPSPIntervals::nonoverlappingIntervals, envir = .GlobalEnv)
  assign("QcSpeciesGrouping", QcPSPIntervals::speciesGrouping, envir = .GlobalEnv)
  assign("QcTreeRemeasurements", QcPSPIntervals::treeRemeasurements, envir = .GlobalEnv)
}
