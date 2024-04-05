#'
#' A series of simple tests
#'

library(QcPSPIntervals)

restoreQcPSPIntervalsData()

test_that("Testing nb rows in QcNonoverlappingIntervals", {expect_equal(nrow(QcNonoverlappingIntervals), 37614)})
test_that("Testing nb rows in QcNbHarvestedTreesByIntervals", {expect_equal(nrow(QcNbHarvestedTreesByIntervals), 5074)})
test_that("Testing nb rows in QcTreeRemeasurements", {expect_equal(nrow(QcTreeRemeasurements), 1479487)})
test_that("Testing nb rows in QcSpeciesGrouping", {expect_equal(nrow(QcSpeciesGrouping), 64)})
test_that("Testing nb of climate sets", {expect_equal(length(getListOfClimateVariables()), 7)})
restoreClimateDataSet("DegreeDay_Annual")
test_that("Testing nb rows in QcClimateVariablesDegreeDay_Annual", {expect_equal(nrow(QcClimateVariablesDegreeDay_Annual), 789894)})

test_that("Testing nb rows in metadata of QcNonoverlappingIntervals", {expect_equal(nrow(getMetaData(QcNonoverlappingIntervals)), ncol(QcNonoverlappingIntervals))})
test_that("Testing nb rows in metadata of QcNbHarvestedTreesByIntervals", {expect_equal(nrow(getMetaData(QcNbHarvestedTreesByIntervals)), ncol(QcNbHarvestedTreesByIntervals))})
test_that("Testing nb rows in metadata of QcTreeRemeasurements", {expect_equal(nrow(getMetaData(QcTreeRemeasurements)), ncol(QcTreeRemeasurements))})
test_that("Testing nb rows in metadata of QcSpeciesGrouping", {expect_equal(nrow(getMetaData(QcSpeciesGrouping)), ncol(QcSpeciesGrouping))})
test_that("Testing nb rows in metadata of QcClimateVariables", {expect_equal(nrow(getMetaData(QcClimateVariablesDegreeDay_Annual)), 3)})

test_that("Testing nb rows in metadata of QcClimateVariables", {expect_equal(nrow(getMetaData(QcTreeIndex)), 9)})
