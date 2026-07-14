# Execute distance analysis on ship-based survey results

This function calculates detection probabilities for specified species
from ship-based survey data using distance sampling methods. It filters
the input data for relevant observations, fits half-normal and
hazard-rate models, and selects the best model based on AIC values. The
output is a data frame containing species names, the selected model
function, and the average detection probability.

## Usage

``` r
Calculate_Detection_P_Ship_Based_Surveys(
  esas_table_2_analyse,
  species_2_analyse
)
```

## Arguments

- esas_table_2_analyse:

  A data frame containing survey data with columns for distance bins,
  platform class, transect type, observation distance, species code,
  behavior, and count. As returned by
  [`Create_ESAS_Table()`](https://inbo.github.io/esas/reference/Create_ESAS_Table.md).

- species_2_analyse:

  A vector of species codes as encoded in the species column of the
  Observations table in the ESAS Data Model. See the
  [Species](https://esas-docs.ices.dk/species/) page of the Data Model.

## Value

A data.frame with the following columns:

- Species: The species code.

- Function: The selected detection function ("HR" for hazard-rate or
  "HN" for half-normal).

- Detection_P_AVG: The average detection probability for the species.

## See also

Other analysis functions:
[`Create_ESAS_Table()`](https://inbo.github.io/esas/reference/Create_ESAS_Table.md),
[`Create_Seabird_Density_Cross_Table()`](https://inbo.github.io/esas/reference/Create_Seabird_Density_Cross_Table.md)

## Examples

``` r
# Read 4 ESAS tables:
path_to_read <- system.file("extdata", "ESAS_0827343782", package = "esas")
esas_tables_list <- Read_ESAS_Tables(
  path = path_to_read,
  file_encoding = "UTF-8"
)

# Create an ESAS master-table:
esas_table <- Create_ESAS_Table(esas_tables_list = esas_tables_list)
#> Joining with `by = join_by(CampaignID)`
#> Joining with `by = join_by(CampaignID, SampleID)`
#> Joining with `by = join_by(CampaignID, SampleID, PositionID)`


# Execute distance analysis on selection of species:
Calculate_Detection_P_Ship_Based_Surveys(
  esas_table_2_analyse = esas_table,
  species_2_analyse = c(720, 6020)
)
#> Fitting hazard-rate key function
#> AIC= 167.732
#> No survey area information supplied, only estimating detection function.
#> Fitting half-normal key function
#> AIC= 167.526
#> No survey area information supplied, only estimating detection function.
#> Fitting hazard-rate key function
#> Warning: Estimated hazard-rate scale parameter close to 0 (on log scale). Possible problem in data (e.g., spike near zero distance).
#> AIC= 343.464
#> Warning: Estimated hazard-rate scale parameter close to 0 (on log scale). Possible problem in data (e.g., spike near zero distance).
#> No survey area information supplied, only estimating detection function.
#> Fitting half-normal key function
#> AIC= 343.884
#> No survey area information supplied, only estimating detection function.
#>   Species Function Detection_P_AVG
#> 1     720       HN            0.81
#> 2    6020       HR            0.67
```
