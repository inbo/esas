require(tidyverse)
require(httr)
require(jsonlite)

#Read data:
Read_ESAS_table <- function(unique_part_of_table_name, pathway, fileEnc)
{
  filenames <- list.files(pathway, full.names = FALSE)
  
  ESAS_table <- read.csv(
    paste(pathway, filenames[grepl(unique_part_of_table_name, filenames, ignore.case = TRUE)], sep = ""), 
    fileEncoding = fileEnc)
  
  return(ESAS_table)
}

#Convert data to upload matrix:
Convert_ESAS_tables_4_upload <- function(CampaignsTable, SamplesTable, PositionsTable, ObservationsTable, Data_provider, Country)
{
  CampaignsTable <- CampaignsTable %>%
    mutate(RecordType = "EC") %>%
    relocate(RecordType)
  
  SamplesTable <- SamplesTable %>%
    mutate(RecordType = "ES") %>%
    relocate(RecordType)
  
  PositionsTable <- PositionsTable %>%
    mutate(RecordType = "EP") %>%
    relocate(RecordType)
  
  ObservationsTable <- ObservationsTable %>%
    mutate(RecordType = "EO") %>%
    relocate(RecordType)

  File_information <- matrix(nrow = 1, ncol = 18)
  File_information[1, 1:3] <- c("FI", Data_provider, Country)
  
  #convert to matrices
  Campaigns_matrix <- matrix(nrow = nrow(CampaignsTable), ncol = 18)
  Campaigns_matrix[, 1:6] <- as.matrix(CampaignsTable)
  
  Samples_matrix <- matrix(nrow = nrow(SamplesTable), ncol = 18)
  Samples_matrix[,1:16] <- as.matrix(SamplesTable)
  
  Positions_matrix <- matrix(nrow = nrow(PositionsTable),ncol = 18)
  Positions_matrix[, 1:16] <- as.matrix(PositionsTable)
  
  Observations_matrix <- matrix(nrow = nrow(ObservationsTable),ncol = 18)
  Observations_matrix[, 1:18] <- as.matrix(ObservationsTable)
  
  #bind matrices
  ESAS_2_ICES <- rbind(File_information,
                       Campaigns_matrix,
                       Samples_matrix,
                       Positions_matrix,
                       Observations_matrix)
  
  ESAS_2_ICES[is.na(ESAS_2_ICES)] <- ""
  ESAS_2_ICES <- as.data.frame(ESAS_2_ICES)
  
  return(ESAS_2_ICES)
}

#Export upload matrix:
Export_ESAS_upload_matrix <- function(table, pathway, export_name, fileEnc)
{
  write.table(table, paste(pathway, export_name, ".csv", sep = ""), 
              sep = "\t", 
              row.names = F, 
              col.names = F, 
              quote = F, 
              fileEncoding = fileEnc)
  
}

#Download ESAS data per species:
Download_ESAS_data <- function(Species)
{
  Campaigns <- GET("https://esas.ices.dk/api/getCampaignRecords")
  Samples <- GET("https://esas.ices.dk/api/getSampleRecords")
  Positions <- GET("https://esas.ices.dk/api/getPositionRecords")
  Observations <- GET(paste("https://esas.ices.dk/api/getObservationRecords?SpeciesCode=",as.character(Species),sep=""))
  
  Campaigns = fromJSON(rawToChar(Campaigns$content))
  Campaigns = Campaigns$results
  
  Samples = fromJSON(rawToChar(Samples$content))
  Samples = Samples$results
  
  Positions = fromJSON(rawToChar(Positions$content))
  Positions = Positions$results
  
  Observations = fromJSON(rawToChar(Observations$content))
  Observations = Observations$results
  
  ObservationsTable <- left_join(left_join(left_join(
    Observations[,c("campaignID","sampleID","positionID","observationID","transect","speciesCode","count","observationDistance")],
    Positions[,c("campaignID","sampleID","positionID","latitude","longitude","area")]),
    Samples[,c("campaignID","sampleID","date")]),
    Campaigns[,c("campaignID","dataRightsHolder","country")])

  return(ObservationsTable)
}
#I think the API functionality is not very elegant for the moment; 
#when selecting one year in campaigns for example, this same selection cannot be performed on the positions or observations table; 
#note that this is perfectly possible in the "less technical" download page!

#Functions to develop:
#Screen data at ICES DC (API?)
#Transform data from ICES DC to OBSALL / TOTALL type of tables
#Transform data from ICES DC to TOTALLcorr type of table with distance corrected densities
