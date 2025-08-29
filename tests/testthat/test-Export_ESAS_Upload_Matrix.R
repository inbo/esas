# Create a tempoary directory with withr so it'll be cleaned as the tests finish
testing_tempdir <- withr::local_tempdir(pattern = "esas_tests",
                     tmpdir = tempdir())

# stand in data vaguely in the form of the returned value from
# Transform_ESAS_Tables_4_Upload()
test_table_to_write <-
  dplyr::tibble(
    X1 = c("FI", "EC", "EC", "EC", "ES"),
    X2 = c(202, 110001081:110001084),
    X3 = c("BE", "Public", "Public", "Public", "110001081"),
    X4 = c(NA, seq(from = as.Date("2025-03-01"), to = as.Date("2025-03-04"), by = "day")),
    X5 = c(NA, seq(from = as.Date("2025-01-12"), to = as.Date("2025-01-14"), by = "day"), "11BU")
  )


test_that("Export_ESAS_Upload_Matrix() writes a tab delimited file", {
  exported_filepath <- Export_ESAS_Upload_Matrix(
    table = test_table_to_write,
    path = testing_tempdir,
    filename = "esas_test_export_for_upload.csv"
  )

  first_lines <- readr::read_lines(exported_filepath, n_max = 3)

  # The delimiter should be present on every line.
  purrr::map_lgl(first_lines, ~ grep("\t", .x)) |>
    purrr::walk(expect_true)
})

test_that("Export_ESAS_Upload_Matrix() doesn't use quoting", {

})

test_that("Export_ESAS_Upload_Matrix() does not include a header", {
  exported_filepath <- Export_ESAS_Upload_Matrix(
    table = test_table_to_write,
    path = testing_tempdir,
    filename = "esas_test_export_for_upload.csv"
  )

  first_line <- readr::read_lines(exported_filepath, n_max = 1)

  # Parse header: what would the header have looked like if it had been written?
  header <- test_table_to_write %>% dplyr::slice(0) %>%
    readr::format_delim(delim =  "\t")


  expect_false(first_line == header)

  # What should the first line look like?
  first_line_delim <- test_table_to_write %>% dplyr::slice(1) %>%
    readr::format_delim(delim =  "\t", col_names = FALSE, eol = "")

  expect_identical(first_line,
                   first_line_delim)
})

test_that("Export_ESAS_Upload_Matrix() writes to the provided path", {
  # Test case where path doesn't end in a backslash
})

test_that("Export_ESAS_Upload_Matrix() writes file with correct extension", {
  # Test for double extensions
})

test_that("Export_ESAS_Upload_Matrix() invisibly returns the out path", {

})

test_that("Export_ESAS_Upload_Matrix() handles non-existent paths", {

})

test_that("Export_ESAS_Upload_Matrix() handles invalid table input", {
  skip("argument testing not implemented yet ISSUE#20")
  expect_error(
    Export_ESAS_Upload_Matrix(table = as.character("This is not a data.frame"),
                              path = testing_tempdir,
                              filename = "invalid.csv"
                              )
  )
})
