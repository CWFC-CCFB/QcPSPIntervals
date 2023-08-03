#'
#' A series of simple tests
#'

test_that("Testing nb rows in QcNonoverlappingIntervals", {expect_equal(nrow(QcPSPIntervals::QcNonoverlappingIntervals), 37614)})
test_that("Testing nb rows in QcNbHarvestedTreesByIntervals", {expect_equal(nrow(QcPSPIntervals::QcNbHarvestedTreesByIntervals), 5074)})
test_that("Testing nb rows in QcTreeRemeasurements", {expect_equal(nrow(QcPSPIntervals::QcTreeRemeasurements), 1479487)})
test_that("Testing nb rows in QcSpeciesGrouping", {expect_equal(nrow(QcPSPIntervals::QcSpeciesGrouping), 64)})
test_that("Testing nb rows in QcClimateVariables", {expect_equal(nrow(QcPSPIntervals::QcClimateVariables$DegreeDay_Annual), 789894)})
