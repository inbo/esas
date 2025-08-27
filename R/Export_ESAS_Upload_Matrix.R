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
