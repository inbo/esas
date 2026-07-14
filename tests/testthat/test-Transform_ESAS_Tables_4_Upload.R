test_that("Transform_ESAS_Tables_4_Upload() returns data.frame", {
  # Read some ESAS data to transform
  data_path <- system.file("extdata", "ESAS_INBO_202401", package = "esas")
  esas_tables <- Read_ESAS_Tables(data_path)

  # Transform the tables to single data.frame
  transformed_esas_tables <-
    Transform_ESAS_Tables_4_Upload(
      campaigns_tbl = esas_tables$CAMPAIGNS,
      samples_tbl = esas_tables$SAMPLES,
      positions_tbl = esas_tables$POSITIONS,
      observations_tbl = esas_tables$OBSERVATIONS,
      data_provider = "202",
      country = "BE"
    )

  expect_s3_class(transformed_esas_tables, "data.frame")
})

test_that("Transform_ESAS_Tables_4_Upload() can transform data downloaded from ESAS", {
  skip("ISSUE#27: bug where only exactly 17 cols are supported.")
  # Read some ESAS data to transform
  data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")
  esas_tables <- Read_ESAS_Tables(data_path)

  transformed_esas_tables <-
    Transform_ESAS_Tables_4_Upload(
      campaigns_tbl = esas_tables$CAMPAIGNS,
      samples_tbl = esas_tables$SAMPLES,
      positions_tbl = esas_tables$POSITIONS,
      observations_tbl = esas_tables$OBSERVATIONS,
      data_provider = "202",
      country = "BE"
    )

  expect_s3_class(transformed_esas_tables, "data.frame")
})

test_that("Transform_ESAS_Tables_4_Upload() returns expected number of rows", {
  # Number of rows returned should be sum of rows of input tables + 1 for file
  # information
  # Read some ESAS data to transform
  data_path <- system.file("extdata", "ESAS_INBO_202401", package = "esas")
  esas_tables <- Read_ESAS_Tables(data_path)

  # Transform the tables to single data.frame
  transformed_esas_tables <-
    Transform_ESAS_Tables_4_Upload(
      campaigns_tbl = esas_tables$CAMPAIGNS,
      samples_tbl = esas_tables$SAMPLES,
      positions_tbl = esas_tables$POSITIONS,
      observations_tbl = esas_tables$OBSERVATIONS,
      data_provider = "202",
      country = "BE"
    )

  expect_identical(
    as.double(nrow(transformed_esas_tables)),
    sum(purrr::map_dbl(esas_tables, nrow)) + 1 # file information row
  )
})

test_that("Transform_ESAS_Tables_4_Upload() returns error on invalid country", {

})

test_that("Transform_ESAS_Tables_4_Upload() returns error on invalid data_provider", {

})

test_that("Transform_ESAS_Tables_4_Upload() returns error on missing input table", {

})

test_that("Transform_ESAS_Tables_4_Upload() returns error on invalid input table", {
  # Missing columns
})
