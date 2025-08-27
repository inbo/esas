#' Combine the 5 tables from the ESAS Data Model to a single data.frame as
#' preparation for uploading
#'
#' The resulting data.frame has 18 columns, where the first column is
#' "RecordType" which indicates the type of record (FI, EC, ES, EP, EO). The
#' remaining columns are filled with the respective data from the input tables.
#'
#' @param campaigns_tbl (data.frame) of campaigns table as returned by
#'   [Read_ESAS_Tables()]`$CAMPAIGNS`
#' @param samples_tbl (data.frame) of samples table as returned by
#'   [Read_ESAS_Tables()]`$SAMPLES`
#' @param positions_tbl (data.frame) of positions table as returned by
#'   [Read_ESAS_Tables()]`$POSITIONS`
#' @param observations_tbl (data.frame) of observations table as returned by
#'   [Read_ESAS_Tables()]`$OBSERVATIONS`
#' @param data_provider (integer) code of the data provider as described by the
#'   ESAS Data Model [file
#'   information](https://esas-docs.ices.dk/tables/#file+information.DataRightsHolder)
#'   table.
#' @param country (character) ISO_3166 country code of the data provider. For
#'   example: "BE".
#'
#' @return (data.frame) with 18 columns and a number of rows equal to the sum of
#'  rows of the input tables plus one (for the file information row).
#' @export
#' @family upload functions
#'
#' @examples
#' \dontrun{
#' ESAS_TABLES_LIST <-
#'   Read_ESAS_Tables(
#'     path = "local_folder_with_downloaded_files",
#'     file_encoding = "UTF-8"
#'   )
#'
#' Transform_ESAS_Tables_4_Upload(
#'   campaigns_tbl = ESAS_TABLES_LIST$CAMPAIGNS,
#'   samples_tbl = ESAS_TABLES_LIST$SAMPLES,
#'   positions_tbl = ESAS_TABLES_LIST$POSITIONS,
#'   observations_tbl = ESAS_TABLES_LIST$OBSERVATIONS,
#'   data_provider = "202",
#'   country = "BE"
#' )
#' }
Transform_ESAS_Tables_4_Upload <- function(campaigns_tbl,
                                           samples_tbl,
                                           positions_tbl,
                                           observations_tbl,
                                           data_provider,
                                           country) {
  campaigns_tbl <- campaigns_tbl %>%
    dplyr::mutate(RecordType = "EC") %>%
    dplyr::relocate(dplyr::all_of("RecordType"))

  samples_tbl <- samples_tbl %>%
    dplyr::mutate(RecordType = "ES") %>%
    dplyr::relocate(dplyr::all_of("RecordType"))

  positions_tbl <- positions_tbl %>%
    dplyr::mutate(RecordType = "EP") %>%
    dplyr::relocate(dplyr::all_of("RecordType"))

  observations_tbl <- observations_tbl %>%
    dplyr::mutate(RecordType = "EO") %>%
    dplyr::relocate(dplyr::all_of("RecordType"))

  file_information <- matrix(nrow = 1, ncol = 18)
  file_information[1, 1:3] <- c("FI", data_provider, country)

  # convert to matrices
  campaigns_matrix <- matrix(nrow = nrow(campaigns_tbl), ncol = 18)
  campaigns_matrix[, 1:6] <- as.matrix(campaigns_tbl)

  samples_matrix <- matrix(nrow = nrow(samples_tbl), ncol = 18)
  samples_matrix[, 1:16] <- as.matrix(samples_tbl)

  positions_matrix <- matrix(nrow = nrow(positions_tbl), ncol = 18)
  positions_matrix[, 1:16] <- as.matrix(positions_tbl)

  observations_matrix <- matrix(nrow = nrow(observations_tbl), ncol = 18)
  observations_matrix[, 1:18] <- as.matrix(observations_tbl)

  # bind matrices
  ESAS_2_ICES <- rbind(
    file_information,
    campaigns_matrix,
    samples_matrix,
    positions_matrix,
    observations_matrix
  )

  ESAS_2_ICES[is.na(ESAS_2_ICES)] <- ""
  ESAS_2_ICES <- as.data.frame(ESAS_2_ICES)

  return(ESAS_2_ICES)
}
