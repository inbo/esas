#Create a cross-table with distance-corrected bird densities
Create_Seabird_Density_Cross_Table <- function(esas_table, probabilities, species_selection)
{
  esas_table <- esas_table %>% filter(DistanceBins %in% c("0|50|100|200|300"),
                                      PlatformClass %in% c(30),
                                      Area > 0)

  observations_select_fly <- esas_table %>%
    filter(SpeciesCode %in% species_selection,
           ObservationDistance %in% c("F"),
           Transect %in% c("True"),
           !Behaviour %in% c(99))

  observations_select_swim <- esas_table %>%
    filter(SpeciesCode %in% species_selection,
           ObservationDistance %in% c("A","B","C","D"),
           Transect %in% c("True"),
           !Behaviour %in% c(99))

  probabilities <- probabilities %>%
    rename(SpeciesCode = Species) %>%
    select(SpeciesCode, Detection_P_AVG)

  observations_select_swim <- left_join(observations_select_swim, probabilities) %>%
    mutate(Count = Count / Detection_P_AVG)

  observations_select <- rbind(observations_select_fly, observations_select_swim %>% select(!Detection_P_AVG))

  base <- esas_table %>% expand(PositionID, species_selection) %>%
    rename(SpeciesCode = species_selection)

  som <- observations_select %>%
    group_by(PositionID, SpeciesCode) %>%
    summarise(Count = sum(Count))

  base_som <- left_join(base, som) %>%
    spread(SpeciesCode, Count, fill = 0) %>%
    arrange(PositionID)

  base_som <- as.data.frame(base_som)

  esas_densities_corr <- esas_table %>%
    distinct(PositionID, Date, Time, Area, Distance, TransectWidth, Latitude, Longitude, SamplingMethod, TargetTaxa)

  round_number <- function(x) round(x, digits = 2)

  esas_densities_corr <- left_join(esas_densities_corr, base_som) %>%
    arrange(Date, Time) %>%
    mutate_at(paste(species_selection), ~ . / Area) %>%
    mutate_at(paste(species_selection), round_number)

  return(esas_densities_corr)
}
