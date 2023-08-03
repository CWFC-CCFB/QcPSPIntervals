#'
#' A series of simple tests
#'

restoreQcPSPIntervalsData()

test_that("Testing nb rows in QcNonoverlappingIntervals", {expect_equal(nrow(QcNonoverlappingIntervals), 37614)})
test_that("Testing nb rows in QcNbHarvestedTreesByIntervals", {expect_equal(nrow(QcNbHarvestedTreesByIntervals), 5074)})
test_that("Testing nb rows in QcTreeRemeasurements", {expect_equal(nrow(QcTreeRemeasurements), 1479487)})
test_that("Testing nb rows in QcSpeciesGrouping", {expect_equal(nrow(QcSpeciesGrouping), 64)})
test_that("Testing nb rows in QcClimateVariables", {expect_equal(nrow(QcClimateVariables$DegreeDay_Annual), 789894)})
