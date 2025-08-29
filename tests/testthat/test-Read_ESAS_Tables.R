test_that("Read_ESAS_Tables() reads 4 tables from filepath", {
  data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")
  # Check number of files
  expect_length(
    Read_ESAS_Tables(data_path),
    4L
  )
})

test_that("Read_ESAS_Tables() returns list of data.frames", {
  data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")

  esas_tables <- Read_ESAS_Tables(data_path)

  expect_type(
    esas_tables,
    "list"
  )

  # Only check for class, not names.
  expect_identical(
    purrr::map(unname(esas_tables), class),
    rep(list("data.frame"), 4)
  )

  # Check correctly named
  expect_named(
    esas_tables,
    c("CAMPAIGNS", "SAMPLES", "POSITIONS", "OBSERVATIONS")
  )

})

test_that("Read_ESAS_Tables() returns error on wrong filenames",{
  # Return an error when the files have non standard file names

})

test_that("Read_ESAS_Tables() applies correct column parsing", {
  # Columns should have the types that correspond to the ESAS Data Model
})
