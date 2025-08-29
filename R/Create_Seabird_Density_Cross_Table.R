#' Create a cross-table with distance-corrected bird densities
#'
#' This function processes seabird observation data to create a cross-table of
#' distance-corrected bird densities for selected species. It filters the data
#' based on specific criteria, applies detection probability corrections, and
#' calculates densities per unit area. The resulting table includes relevant
#' metadata such as date, time, area, and location.
#'
#' Requirements:
#' - `DistanceBins` must be `0|50|100|200|300`
#' - `PlatformClass` must be `30` (ship-based surveys)
#' - `Area` must be greater than `0`
#' - `ObservationDistance` must be `F` for flying birds and `A`, `B`, `C`, or `D` for swimming/diving birds
#' - `Transect` must be `True`
#' - `Behaviour` must not be `99`
#' - Species codes must be provided in `species_selection` and must exist in the `esas_table`
#'
#' @param esas_table A data frame containing seabird observation data as
#'   resulting from the [Create_ESAS_Table()] function.
#' @param probabilities A data frame containing detection probabilities for
#'   different species as returned by
#'   [Calculate_Detection_P_Ship_Based_Surveys()]
#' @param species_selection A vector of species codes to include in the analysis
#'   as encoded in the species column of the Observations table in the ESAS Data
#'   Model. See the [Species](https://esas-docs.ices.dk/species/) page of the
#'   Data Model.
#' @return A data frame containing distance-corrected bird densities for the
#'   selected species.
#' @export
#' @family analysis functions
#' @importFrom dplyr .data
#'
#' @examples
#'
#' path_to_read <- system.file("extdata", "ESAS_0827343782", package = "esas")
#' esas_table <- Create_ESAS_Table(Read_ESAS_Tables(path = path_to_read))
#' Create_Seabird_Density_Cross_Table(
#'   esas_table = esas_table,
#'   probabilities =
#'     Calculate_Detection_P_Ship_Based_Surveys(esas_table,
#'                                              species_2_analyse = c(720,6000)),
#'   species_selection = c(720, 6020)
#' )

Create_Seabird_Density_Cross_Table <- function(esas_table,
                                               probabilities,
                                               species_selection) {
  esas_table <- esas_table %>% dplyr::filter(
    .data$DistanceBins %in% c("0|50|100|200|300"),
    .data$PlatformClass %in% c(30),
    .data$Area > 0
  )

  observations_select_fly <- esas_table %>%
    dplyr::filter(
      .data$SpeciesCode %in% species_selection,
      .data$ObservationDistance %in% c("F"),
      .data$Transect %in% c("True"),
      !.data$Behaviour %in% c(99)
    )

  observations_select_swim <- esas_table %>%
    dplyr::filter(
      .data$SpeciesCode %in% species_selection,
      .data$ObservationDistance %in% c("A", "B", "C", "D"),
      .data$Transect %in% c("True"),
      !.data$Behaviour %in% c(99)
    )

  probabilities <- probabilities %>%
    dplyr::rename(SpeciesCode = dplyr::all_of("Species")) %>%
    dplyr::select(dplyr::all_of(c("SpeciesCode", "Detection_P_AVG")))

  observations_select_swim <-
    dplyr::left_join(observations_select_swim, probabilities) %>%
    dplyr::mutate(Count = .data$Count / .data$Detection_P_AVG)

  observations_select <- rbind(
    observations_select_fly,
    observations_select_swim %>%
      dplyr::select(-dplyr::any_of("Detection_P_AVG"))
  )

  base <- esas_table %>%
    tidyr::expand(.data$PositionID, species_selection) %>%
    dplyr::rename(SpeciesCode = dplyr::all_of("species_selection"))

  som <- observations_select %>%
    dplyr::group_by(.data$PositionID, .data$SpeciesCode) %>%
    dplyr::summarise(Count = sum(.data$Count))

  base_som <- dplyr::left_join(base, som) %>%
    tidyr::spread(key = "SpeciesCode",
                  value = "Count",
                  fill = 0) %>%
    dplyr::arrange(.data$PositionID)

  base_som <- as.data.frame(base_som)

  esas_densities_corr <- esas_table %>%
    dplyr::distinct(
      .data$PositionID,
      .data$Date,
      .data$Time,
      .data$Area,
      .data$Distance,
      .data$TransectWidth,
      .data$Latitude,
      .data$Longitude,
      .data$SamplingMethod,
      .data$TargetTaxa
    )

  esas_densities_corr <- dplyr::left_join(esas_densities_corr, base_som) %>%
    dplyr::arrange(.data$Date, .data$Time) %>%
    dplyr::mutate_at(paste(species_selection), ~ . / Area) %>%
    dplyr::mutate_at(paste(species_selection), ~ round(.x, digits = 2))

  return(esas_densities_corr)
}
