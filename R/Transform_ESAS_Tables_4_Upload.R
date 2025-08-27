#Convert data to upload matrix:
Transform_ESAS_Tables_4_Upload <- function(campaigns_tbl, samples_tbl, positions_tbl, observations_tbl, data_provider, country)
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
