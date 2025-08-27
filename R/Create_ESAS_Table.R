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
Create_ESAS_Table <- function(esas_tables_list) {
  esas_table <- dplyr::left_join(
    dplyr::left_join(
      dplyr::left_join(
        esas_tables_list$CAMPAIGNS %>% dplyr::rename(CampaignNotes = Notes),
        esas_tables_list$SAMPLES %>% dplyr::rename(SampleNotes = Notes)
      ),
      esas_tables_list$POSITIONS
    ),
    esas_tables_list$OBSERVATIONS
  )

  return(esas_table)
}
