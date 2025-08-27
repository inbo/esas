Create_ESAS_Table <- function(esas_tables_list) {
  esas_table <- left_join(
    left_join(
      left_join(
        esas_tables_list$CAMPAIGNS %>% rename(CampaignNotes = Notes),
        esas_tables_list$SAMPLES %>% rename(SampleNotes = Notes)
      ),
      esas_tables_list$POSITIONS
    ),
    esas_tables_list$OBSERVATIONS
  )

  return(esas_table)
}
