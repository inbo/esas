#' Write a tab delimited csv file as expected by the ESAS upload module.
#'
#' This function exports a data frame to a tab-delimited file without row names,
#' column names, or quotes, using the specified file encoding.
#'
#' @param table A data frame or matrix to be exported as returned by
#'   [Transform_ESAS_Tables_4_Upload()]
#' @param path Directory path where the file will be saved.
#' @param filename Character string specifying the name of the output file
#'   without the extension.
#' @param file_encoding Character string specifying the file encoding (default
#'   is "UTF-8"). See [base::file()] for details on supported encodings.
#'
#' @return (invisible) The path to the exported file.
#' @export
#' @family upload functions
#'
#' @examples
#' \dontrun{
#' #Read 4 ESAS tables:
#' ESAS_TABLES_LIST <- Read_ESAS_Tables(path = "./Data/",
#'                                      file_encoding = "UTF-8")
#'
#' #Convert tables to upload format:
#' ESAS_4_UPLOAD <- Transform_ESAS_Tables_4_Upload(campaigns_tbl = ESAS_TABLES_LIST$CAMPAIGNS,
#'                                                 samples_tbl = ESAS_TABLES_LIST$SAMPLES,
#'                                                 positions_tbl = ESAS_TABLES_LIST$POSITIONS,
#'                                                 observations_tbl = ESAS_TABLES_LIST$OBSERVATIONS,
#'                                                 data_provider = "202",
#'                                                 country = "BE")
#'
#' #Export upload table:
#' Export_ESAS_Upload_Matrix(table = ESAS_4_UPLOAD,
#'                           path = "./Output/",
#'                           export_name = "ESAS_4_upload",
#'                           file_encoding = "UTF-8")
#'}
Export_ESAS_Upload_Matrix <- function(table,
                                      path,
                                      filename,
                                      file_encoding = "UTF-8") {
  # Export upload matrix

  # Create output path
  out_path <- paste(path, filename, ".csv", sep = "")
  # Write table to file
  write.table(
    x = table,
    file = out_path,
    sep = "\t",
    row.names = F,
    col.names = F,
    quote = F,
    fileEncoding = file_encoding
  )
  # Return path to file
  invisible(out_path)
}
