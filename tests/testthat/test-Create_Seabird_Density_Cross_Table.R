# Read ESAS data from file
data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")
esas_table <- Read_ESAS_Tables(data_path) |>
  Create_ESAS_Table()

test_that("Create_Seabird_Density_Cross_Table() returns data.frame", {

})

test_that("Create_Seabird_Density_Cross_Table() returns expected columns", {

})
