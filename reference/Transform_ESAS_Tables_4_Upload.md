# Combine the 5 tables from the ESAS Data Model to a single data.frame as preparation for uploading

The resulting data.frame has 18 columns, where the first column is
"RecordType" which indicates the type of record (FI, EC, ES, EP, EO).
The remaining columns are filled with the respective data from the input
tables.

## Usage

``` r
Transform_ESAS_Tables_4_Upload(
  campaigns_tbl,
  samples_tbl,
  positions_tbl,
  observations_tbl,
  data_provider,
  country
)
```

## Arguments

- campaigns_tbl:

  (data.frame) of campaigns table as returned by
  [`Read_ESAS_Tables()`](https://inbo.github.io/esas/reference/Read_ESAS_Tables.md)`$CAMPAIGNS`

- samples_tbl:

  (data.frame) of samples table as returned by
  [`Read_ESAS_Tables()`](https://inbo.github.io/esas/reference/Read_ESAS_Tables.md)`$SAMPLES`

- positions_tbl:

  (data.frame) of positions table as returned by
  [`Read_ESAS_Tables()`](https://inbo.github.io/esas/reference/Read_ESAS_Tables.md)`$POSITIONS`

- observations_tbl:

  (data.frame) of observations table as returned by
  [`Read_ESAS_Tables()`](https://inbo.github.io/esas/reference/Read_ESAS_Tables.md)`$OBSERVATIONS`

- data_provider:

  (integer) code of the data provider as described by the ESAS Data
  Model [file
  information](https://esas-docs.ices.dk/tables/#file+information.DataRightsHolder)
  table.

- country:

  (character) ISO_3166 country code of the data provider. For example:
  "BE".

## Value

(data.frame) with 18 columns and a number of rows equal to the sum of
rows of the input tables plus one (for the file information row).

## See also

Other upload functions:
[`Export_ESAS_Upload_Matrix()`](https://inbo.github.io/esas/reference/Export_ESAS_Upload_Matrix.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ESAS_TABLES_LIST <-
  Read_ESAS_Tables(
    path = "local_folder_with_downloaded_files",
    file_encoding = "UTF-8"
  )

Transform_ESAS_Tables_4_Upload(
  campaigns_tbl = ESAS_TABLES_LIST$CAMPAIGNS,
  samples_tbl = ESAS_TABLES_LIST$SAMPLES,
  positions_tbl = ESAS_TABLES_LIST$POSITIONS,
  observations_tbl = ESAS_TABLES_LIST$OBSERVATIONS,
  data_provider = "202",
  country = "BE"
)
} # }
```
