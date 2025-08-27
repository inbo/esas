#' Execute distance analysis on ship-based survey results
#'
#' This function calculates detection probabilities for specified species from
#' ship-based survey data using distance sampling methods. It filters the input
#' data for relevant observations, fits half-normal and hazard-rate models, and
#' selects the best model based on AIC values. The output is a data frame
#' containing species names, the selected model function, and the average
#' detection probability.
#'
#' @param esas_table_2_analyse A data frame containing survey data with columns
#'   for distance bins, platform class, transect type, observation distance,
#'   species code, behavior, and count. As returned by [Create_ESAS_Table()].
#' @param species_2_analyse A vector of species codes as encoded in the species
#'   column of the Observations table in the ESAS Data Model. See the
#'   [Species](https://esas-docs.ices.dk/species/) page of the Data Model.
#'
#' @return A data.frame with the following columns:
#' - Species: The species code.
#' - Function: The selected detection function ("HR" for hazard-rate or "HN" for half-normal).
#' - Detection_P_AVG: The average detection probability for the species.
#' @export
#' @family analysis functions
#'
#' @examples
Calculate_Detection_P_Ship_Based_Surveys <- function(esas_table_2_analyse,
                                                     species_2_analyse) {
  # The Distance package is required to use this function
  rlang::check_installed("Distance")

  DISTANCE <- esas_table_2_analyse %>%
    dplyr::filter(
      DistanceBins %in% c("0|50|100|200|300"),
      PlatformClass %in% c(30),
      Transect %in% c("True"),
      ObservationDistance %in% c("A", "B", "C", "D"),
      SpeciesCode %in% species_2_analyse,
      !Behaviour %in% c("99")
    ) %>%
    dplyr::mutate(distance = recode(
      ObservationDistance,
      "A" = 0.025,
      "B" = 0.075,
      "C" = 0.150,
      "D" = 0.250
    )) %>%
    dplyr::rename(Size = Count) %>%
    dplyr::select(PositionID, SpeciesCode, Size, distance)

  distance_model_list_HR <- vector("list", length(species_2_analyse))
  distance_model_list_HN <- vector("list", length(species_2_analyse))

  for (i in c(1:length(species_2_analyse))) {
    Species2model <- species_2_analyse[i]

    distance_model_list_HR[[i]] <-
      Distance::ds(
        DISTANCE %>% dplyr::filter(SpeciesCode == Species2model),
        cutpoints = c(0, .05, .1, .2, .3),
        truncation = 0.3,
        key = "hr",
        adjustment = NULL,
        formula = ~1
      )

    distance_model_list_HN[[i]] <-
      Distance::ds(
        DISTANCE %>% dplyr::filter(SpeciesCode == Species2model),
        cutpoints = c(0, .05, .1, .2, .3),
        truncation = 0.3,
        key = "hn",
        adjustment = NULL,
        formula = ~1
      )
  }

  probabilities <-
    as.data.frame(matrix(nrow = length(species_2_analyse), ncol = 5))
  colnames(probabilities) <- c(
    "Species",
    "Detection_HR_P_AVG",
    "Detection_HR_AIC",
    "Detection_HN_P_AVG",
    "Detection_HN_AIC"
  )
  probabilities$Species <- species_2_analyse

  for (i in c(1:length(species_2_analyse))) {
    probabilities[i, 2] <-
      Distance::summary(distance_model_list_HR[[i]])$ds$average.p
    probabilities[i, 3] <-
      Distance::summary(distance_model_list_HR[[i]])$ds$aic

    probabilities[i, 4] <-
      Distance::summary(distance_model_list_HN[[i]])$ds$average.p
    probabilities[i, 5] <-
      Distance::summary(distance_model_list_HN[[i]])$ds$aic
  }

  probabilities <- probabilities %>%
    dplyr::mutate(
      Function = if_else(Detection_HR_AIC < Detection_HN_AIC, "HR", "HN"),
      Detection_P_AVG = if_else(
        Detection_HR_AIC < Detection_HN_AIC,
        Detection_HR_P_AVG,
        Detection_HN_P_AVG
      )
    ) %>%
    dplyr::mutate_at(c("Detection_P_AVG"), round_prob)

  return(probabilities %>% dplyr::select(Species, Function, Detection_P_AVG))
}
