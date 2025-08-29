# Read ESAS data from file
data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")
esas_table <- Read_ESAS_Tables(data_path) |>
  Create_ESAS_Table()

# Create a density cross table for testing
density_cross <- suppressMessages(
  Create_Seabird_Density_Cross_Table(
    esas_table = esas_table,
    probabilities =
      Calculate_Detection_P_Ship_Based_Surveys(esas_table, species_2_analyse = c(720, 6000)),
    species_selection = c(720, 6020)
  )
)


test_that("Create_Seabird_Density_Cross_Table() returns data.frame", {
  expect_s3_class(density_cross, "data.frame")
})

test_that("Create_Seabird_Density_Cross_Table() returns expected columns", {
  expect_named(
    density_cross,
    c(
      "PositionID",
      "Date",
      "Time",
      "Area",
      "Distance",
      "TransectWidth",
      "Latitude",
      "Longitude",
      "SamplingMethod",
      "TargetTaxa",
      "720",
      "6020"
    )
  )
})

test_that("Create_Seabird_Density_Cross_Table() returns error when selected species is absent", {

})
