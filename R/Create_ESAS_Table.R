#' Combine the tables from the ESAS Data Model into a single data.frame
#'
#' The Analysis functions in this R package expect a single data.frame as input.
#' This function combines the tables from the ESAS Data Model into a single
#' data.frame. It performs left joins to ensure that all records from the
#' CAMPAIGNS, SAMPLES, POSITIONS, and OBSERVATIONS tables are included. It also
#' renames the Notes columns from the CAMPAIGNS and SAMPLES tables to avoid
#' naming conflicts.
#'
#' @param esas_tables_list A list containing the ESAS tables as data.frames. The
#'   list should include the following named elements: CAMPAIGNS, SAMPLES,
#'   POSITIONS, and OBSERVATIONS.
#'
#' @return A single data.frame that combines the information from the CAMPAIGNS,
#'   SAMPLES, POSITIONS, and OBSERVATIONS tables.
#' @export
#' @family analysis functions
#'
#' @examples
#' # Read 4 ESAS tables:
#' path_to_read <- system.file("extdata", "ESAS_0827343782", package = "esas")
#' esas_tables_list <- Read_ESAS_Tables(
#'   path = path_to_read,
#'   file_encoding = "UTF-8"
#' )
#'
#' # Create an ESAS master-table:
#' esas_table <- Create_ESAS_Table(esas_tables_list = esas_tables_list)
#' esas_table

Create_ESAS_Table <- function(esas_tables_list) {

  campaigns <- esas_tables_list$CAMPAIGNS %>%
    dplyr::rename_with(~ paste0("Campaign", .x, recycle0 = TRUE), .cols = dplyr::any_of("Notes"))
  samples <- esas_tables_list$SAMPLES %>%
    dplyr::rename_with(~ paste0("Sample", .x, recycle0 = TRUE), .cols = dplyr::any_of("Notes"))
  positions <- esas_tables_list$POSITIONS %>%
    dplyr::rename_with(~ paste0("Position", .x, recycle0 = TRUE), .cols = dplyr::any_of("Notes"))
  observations <- esas_tables_list$OBSERVATIONS %>%
    dplyr::rename_with(~ paste0("Observation", .x, recycle0 = TRUE), .cols = dplyr::any_of("Notes"))

  # Join by `by = join_by(CampaignID)`, then `by = join_by(CampaignID,
  # SampleID)` and finally `by = join_by(CampaignID, SampleID, PositionID)`
  esas_table <-
    purrr::reduce(
      list(campaigns, samples, positions, observations),
      dplyr::left_join
    )

 # Return the joined data.frame
 esas_table
}
