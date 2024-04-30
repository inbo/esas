require(tidyverse)
require(httr)
require(jsonlite)
require(Distance)

#Read data & convert to ESAS 'mega-table':
Read_ESAS_Tables <- function(pathway, file_encoding)
{
  filenames <- list.files(pathway, full.names = TRUE)
  
  camp <- read.csv(filenames[grepl("camp", filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  samp <- read.csv(filenames[grepl("samp", filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  pos  <- read.csv(filenames[grepl("pos" , filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  obs  <- read.csv(filenames[grepl("obs" , filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  
  tables_list <- list(CAMPAIGNS = camp, SAMPLES = samp, POSITIONS = pos, OBSERVATIONS = obs)
  
  return(tables_list)
}  

Create_ESAS_Table <- function(esas_tables_list)   
{
  esas_table <- left_join(left_join(left_join(
    esas_tables_list$OBSERVATIONS, esas_tables_list$POSITIONS), 
    esas_tables_list$SAMPLES %>% rename(SampleNotes = Notes)), 
    esas_tables_list$CAMPAIGNS %>% rename(CampaignNotes = Notes)) 
  
  return(esas_table)
}
  
#Convert data to upload matrix:
Convert_ESAS_Tables_4_Upload <- function(campaigns_tbl, samples_tbl, positions_tbl, observations_tbl, data_provider, country)
{
  campaigns_tbl <- campaigns_tbl %>%
    mutate(RecordType = "EC") %>%
    relocate(RecordType)
  
  samples_tbl <- samples_tbl %>%
    mutate(RecordType = "ES") %>%
    relocate(RecordType)
  
  positions_tbl <- positions_tbl %>%
    mutate(RecordType = "EP") %>%
    relocate(RecordType)
  
  observations_tbl <- observations_tbl %>%
    mutate(RecordType = "EO") %>%
    relocate(RecordType)

  file_information <- matrix(nrow = 1, ncol = 18)
  file_information[1,1:3] <- c("FI", data_provider, country)
  
  #convert to matrices
  campaigns_matrix <- matrix(nrow = nrow(campaigns_tbl), ncol = 18)
  campaigns_matrix[,1:6] <- as.matrix(campaigns_tbl)
  
  samples_matrix <- matrix(nrow = nrow(samples_tbl), ncol = 18)
  samples_matrix[,1:16] <- as.matrix(samples_tbl)
  
  positions_matrix <- matrix(nrow = nrow(positions_tbl), ncol = 18)
  positions_matrix[,1:16] <- as.matrix(positions_tbl)
  
  observations_matrix <- matrix(nrow = nrow(observations_tbl), ncol = 18)
  observations_matrix[,1:18] <- as.matrix(observations_tbl)
  
  #bind matrices
  ESAS_2_ICES <- rbind(file_information,
                       campaigns_matrix,
                       samples_matrix,
                       positions_matrix,
                       observations_matrix)
  
  ESAS_2_ICES[is.na(ESAS_2_ICES)] <- ""
  ESAS_2_ICES <- as.data.frame(ESAS_2_ICES)
  
  return(ESAS_2_ICES)
}

#Export upload matrix:
Export_ESAS_Upload_Matrix <- function(table, pathway, export_name, file_encoding)
{
  write.table(table, paste(pathway, export_name, ".csv", sep = ""), 
              sep = "\t", 
              row.names = F, 
              col.names = F, 
              quote = F, 
              fileEncoding = file_encoding)
  
}

#Execute distance analysis on ship-based survey results:
Detection_Probabilities_Ship_Based_Surveys <- function(esas_table_2_analyse, species_2_analyse)
{
  DISTANCE <- esas_table_2_analyse %>%
    filter(DistanceBins == "0|50|100|200|300",
           PlatformClass == 30,
           Transect == "True",
           ObservationDistance != "F",
           SpeciesCode %in% species_2_analyse,
           Behaviour != "99",
           ObservationDistance %in% c("A","B","C","D")) %>%
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

# NOTE:
# Om de één of andere reden wordt het veld "Transect" teruggegeven als character variabele ("True"/"False") terwijl deze
# als boolean wordt opgeladen 
# ISSUE AANMAKEN!!


