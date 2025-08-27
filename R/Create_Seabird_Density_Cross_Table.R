#' Create a cross-table with distance-corrected bird densities
#'
#' This function processes seabird observation data to create a cross-table of
#' distance-corrected bird densities for selected species. It filters the data
#' based on specific criteria, applies detection probability corrections, and
#' calculates densities per unit area. The resulting table includes relevant
#' metadata such as date, time, area, and location.
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
#'
#' @examples
Create_Seabird_Density_Cross_Table <- function(esas_table,
                                               probabilities,
                                               species_selection) {
  esas_table <- esas_table %>% dplyr::filter(
    DistanceBins %in% c("0|50|100|200|300"),
    PlatformClass %in% c(30),
    Area > 0
  )

  observations_select_fly <- esas_table %>%
    dplyr::filter(
      SpeciesCode %in% species_selection,
      ObservationDistance %in% c("F"),
      Transect %in% c("True"),
      !Behaviour %in% c(99)
    )

  observations_select_swim <- esas_table %>%
    dplyr::filter(
      SpeciesCode %in% species_selection,
      ObservationDistance %in% c("A", "B", "C", "D"),
      Transect %in% c("True"),
      !Behaviour %in% c(99)
    )

  probabilities <- probabilities %>%
    dplyr::rename(SpeciesCode = Species) %>%
    select(SpeciesCode, Detection_P_AVG)

  observations_select_swim <-
    left_join(observations_select_swim, probabilities) %>%
    dplyr::mutate(Count = Count / Detection_P_AVG)

  observations_select <- rbind(
    observations_select_fly,
    observations_select_swim %>% select(!Detection_P_AVG)
  )

  base <- esas_table %>%
    expand(PositionID, species_selection) %>%
    dplyr::rename(SpeciesCode = species_selection)

  som <- observations_select %>%
    dplyr::group_by(PositionID, SpeciesCode) %>%
    dplyr::summarise(Count = sum(Count))

  base_som <- left_join(base, som) %>%
    dplyr::spread(SpeciesCode, Count, fill = 0) %>%
    dplyr::arrange(PositionID)

  base_som <- as.data.frame(base_som)

  esas_densities_corr <- esas_table %>%
    dplyr::distinct(
      PositionID,
      Date,
      Time,
      Area,
      Distance,
      TransectWidth,
      Latitude,
      Longitude,
      SamplingMethod,
      TargetTaxa
    )

  round_number <- function(x) {
    round(x, digits = 2)
  }

  esas_densities_corr <- dplyr::left_join(esas_densities_corr, base_som) %>%
    dplyr::arrange(Date, Time) %>%
    dplyr::mutate_at(paste(species_selection), ~ . / Area) %>%
    dplyr::mutate_at(paste(species_selection), round_number)

  return(esas_densities_corr)
}
