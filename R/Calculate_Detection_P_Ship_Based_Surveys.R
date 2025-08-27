#Execute distance analysis on ship-based survey results:
Calculate_Detection_P_Ship_Based_Surveys <- function(esas_table_2_analyse, species_2_analyse)
{
  DISTANCE <- esas_table_2_analyse %>%
    filter(DistanceBins %in% c("0|50|100|200|300"),
           PlatformClass %in% c(30),
           Transect %in% c("True"),
           ObservationDistance %in% c("A","B","C","D"),
           SpeciesCode %in% species_2_analyse,
           !Behaviour %in% c("99")) %>%
    mutate(distance = recode(ObservationDistance,
                             "A" = 0.025,
                             "B" = 0.075,
                             "C" = 0.150,
                             "D" = 0.250)) %>%
    rename(Size = Count) %>%
    select(PositionID, SpeciesCode, Size, distance)

  distance_model_list_HR <- vector('list', length(species_2_analyse))
  distance_model_list_HN <- vector('list', length(species_2_analyse))

  for (i in c(1:length(species_2_analyse)))
  {
    Species2model <- species_2_analyse[i]

    distance_model_list_HR[[i]] <-
      ds(DISTANCE %>% filter(SpeciesCode == Species2model),
         cutpoints = c(0,.05,.1,.2,.3), truncation = 0.3, key = "hr", adjustment = NULL, formula = ~1)

    distance_model_list_HN[[i]] <-
      ds(DISTANCE %>% filter(SpeciesCode == Species2model),
         cutpoints = c(0,.05,.1,.2,.3), truncation = 0.3, key = "hn", adjustment = NULL, formula = ~1)
  }

  probabilities <- as.data.frame(matrix(nrow = length(species_2_analyse), ncol = 5))
  colnames(probabilities) <- c("Species","Detection_HR_P_AVG","Detection_HR_AIC","Detection_HN_P_AVG","Detection_HN_AIC")
  probabilities$Species <- species_2_analyse

  for (i in c(1:length(species_2_analyse)))
  {
    probabilities[i,2]  <- summary(distance_model_list_HR[[i]])$ds$average.p
    probabilities[i,3]  <- summary(distance_model_list_HR[[i]])$ds$aic

    probabilities[i,4]  <- summary(distance_model_list_HN[[i]])$ds$average.p
    probabilities[i,5]  <- summary(distance_model_list_HN[[i]])$ds$aic
  }

  round_prob <- function(x) round(x, digits = 2)

  probabilities <- probabilities %>%
    mutate(
      Function = if_else(Detection_HR_AIC < Detection_HN_AIC, "HR", "HN"),
      Detection_P_AVG = if_else(Detection_HR_AIC < Detection_HN_AIC, Detection_HR_P_AVG, Detection_HN_P_AVG)) %>%
    mutate_at(
      c("Detection_P_AVG"), round_prob)

  return(probabilities %>% select(Species, Function, Detection_P_AVG))
}
