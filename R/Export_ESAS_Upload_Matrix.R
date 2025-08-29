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
#'
#' @return (invisible) The path to the exported file.
#' @export
#' @family upload functions
#'
#' @examples
#' #Read 4 ESAS tables:
#' data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")
#' esas_tables_list <- Read_ESAS_Tables(path = data_path),
#'                                      file_encoding = "UTF-8")
#'
#' #Convert tables to upload format:
#' esas_4_upload <- Transform_ESAS_Tables_4_Upload(campaigns_tbl = esas_tables_list$CAMPAIGNS,
#'                                                 samples_tbl = esas_tables_list$SAMPLES,
#'                                                 positions_tbl = esas_tables_list$POSITIONS,
#'                                                 observations_tbl = esas_tables_list$OBSERVATIONS,
#'                                                 data_provider = "202",
#'                                                 country = "BE")
#'
#'\dontrun{
#' #Export upload table:
#' Export_ESAS_Upload_Matrix(table = esas_4_upload,
#'                           path = tempdir(),
#'                           filename = "esas_4_upload")
#'}
Export_ESAS_Upload_Matrix <- function(table,
                                      path,
                                      filename) {
  # Export upload matrix

  # Create output path
  out_path <- paste(path, filename, ".csv", sep = "")

  # Write table to file
  readr::write_delim(table,
                     file = out_path,
                     delim = "\t",
                     col_names = FALSE,
                     quote = "none")

  # Return path to file
  invisible(out_path)
}
