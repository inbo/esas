# Write a tab delimited csv file as expected by the ESAS upload module.

This function exports a data frame to a tab-delimited file without row
names, column names, or quotes, using the specified file encoding.

## Usage

``` r
Export_ESAS_Upload_Matrix(table, path, filename)
```

## Arguments

- table:

  A data frame or matrix to be exported as returned by
  [`Transform_ESAS_Tables_4_Upload()`](https://inbo.github.io/esas/reference/Transform_ESAS_Tables_4_Upload.md)

- path:

  Directory path where the file will be saved.

- filename:

  Character string specifying the name of the output file without the
  extension.

## Value

(invisible) The path to the exported file.

## See also

Other upload functions:
[`Transform_ESAS_Tables_4_Upload()`](https://inbo.github.io/esas/reference/Transform_ESAS_Tables_4_Upload.md)

## Examples

``` r
#Read 4 ESAS tables:
data_path <- system.file("extdata", "ESAS_INBO_202401", package = "esas")
esas_tables_list <- Read_ESAS_Tables(path = data_path,
                                     file_encoding = "UTF-8")

#Convert tables to upload format:
esas_4_upload <- Transform_ESAS_Tables_4_Upload(campaigns_tbl = esas_tables_list$CAMPAIGNS,
                                                samples_tbl = esas_tables_list$SAMPLES,
                                                positions_tbl = esas_tables_list$POSITIONS,
                                                observations_tbl = esas_tables_list$OBSERVATIONS,
                                                data_provider = "202",
                                                country = "BE")

if (FALSE) { # \dontrun{
#Export upload table:
Export_ESAS_Upload_Matrix(table = esas_4_upload,
                          path = tempdir(),
                          filename = "esas_4_upload")
} # }
```
