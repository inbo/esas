require(tidyverse)
require(httr)
require(jsonlite)

#Read data:
Read_ESAS_tables <- function(pathway, file_encoding)
{
  filenames <- list.files(pathway, full.names = TRUE)
  
  camp <- read.csv(filenames[grepl("camp", filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  samp <- read.csv(filenames[grepl("samp", filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  pos  <- read.csv(filenames[grepl("pos" , filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  obs  <- read.csv(filenames[grepl("obs" , filenames, ignore.case = TRUE)], fileEncoding = file_encoding)
  
  ESAS_tables <- list(camp, samp, pos, obs)
  
  return(ESAS_tables)
}

#Convert data to upload matrix:
Convert_ESAS_tables_4_upload <- function(campaigns_tbl, samples_tbl, positions_tbl, observations_tbl, data_provider, country)
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
Export_ESAS_upload_matrix <- function(table, pathway, export_name, file_encoding)
{
  write.table(table, paste(pathway, export_name, ".csv", sep = ""), 
              sep = "\t", 
              row.names = F, 
              col.names = F, 
              quote = F, 
              fileEncoding = file_encoding)
  
}

#Download ESAS data per species:
Download_ESAS_data <- function(species)
{
  campaigns <- GET("https://esas.ices.dk/api/getCampaignRecords")
  samples <- GET("https://esas.ices.dk/api/getSampleRecords")
  positions <- GET("https://esas.ices.dk/api/getPositionRecords")
  observations <- GET(paste("https://esas.ices.dk/api/getObservationRecords?SpeciesCode=", as.character(species), sep = ""))
  
  campaigns = fromJSON(rawToChar(campaigns$content))
  campaigns = campaigns$results
  
  samples = fromJSON(rawToChar(samples$content))
  samples = samples$results
  
  positions = fromJSON(rawToChar(positions$content))
  positions = positions$results
  
  observations = fromJSON(rawToChar(observations$content))
  observations = observations$results
  
  Species_observations_tbl <- left_join(left_join(left_join(
    observations[,c("campaignID","sampleID","positionID","speciesScientificName","count","observationDistance")],
    positions[,c("campaignID","sampleID","positionID","latitude","longitude")]),
    samples[,c("campaignID","sampleID","date")]),
    campaigns[,c("campaignID","dataRightsHolder","country")])

  Species_observations_tbl <- Species_observations_tbl %>%
    select(date,latitude,longitude,speciesScientificName,count,observationDistance,dataRightsHolder,country) %>%
    arrange(date)
  
  return(Species_observations_tbl)
}
#I think the API functionality is not very elegant for the moment; 
#when selecting one year in campaigns for example, this same selection cannot be performed on the positions or observations table; 
#note that this is perfectly possible in the "less technical" download page!

#Functions to develop:
#Screen data at ICES DC (API?)
#Transform data from ICES DC to OBSALL / TOTALL type of tables
#Transform data from ICES DC to TOTALLcorr type of table with distance corrected densities

#Gesprek met Eric (18/04):
#Het is misschien interessant om generieke distance correctiefactoren te berekenen, enkel uitgesplitst per methodiek, bvb
#aerial & ship-based, in plaats van de distance analyse op zich mee op te nemen in de R pakket functies