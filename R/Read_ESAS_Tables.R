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
