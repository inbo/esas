#' Read csv files and combine them into a single data.frame
#'
#' Read 4 csv files (CAMPAIGNS, SAMPLES, POSITIONS, OBSERVATIONS) and combine
#' them into a list of data.frames.
#'
#' @param path (Character.) Path to the directory containing 4 csv files as
#'   returned by the [ESAS Data inventory](https://esas.ices.dk/inventory).
#'   It should contain the files: "Campaigns.csv", "Samples.csv",
#'   "Positions.csv" and "Observations.csv".
#' @param file_encoding (Character.) File encoding. Default is "UTF-8".
#'
#' @return A list of 4 data.frames: CAMPAIGNS, SAMPLES, POSITIONS, OBSERVATIONS.
#' @export
#' @family read data from ESAS
#' @examples
#' data_path <- system.file("extdata", "ESAS_0827343782", package = "esas")
#' Read_ESAS_Tables(data_path)
#'
Read_ESAS_Tables <- function(path, file_encoding = "UTF-8") {
  camp <- utils::read.csv(file.path(path, "Campaigns.csv"),
    fileEncoding = file_encoding
  )
  samp <- utils::read.csv(file.path(path, "Samples.csv"),
    fileEncoding = file_encoding
  )
  pos <- utils::read.csv(file.path(path, "Positions.csv"),
    fileEncoding = file_encoding
  )
  obs <- utils::read.csv(file.path(path, "Observations.csv"),
    fileEncoding = file_encoding
  )

  tables_list <- list(
    CAMPAIGNS = camp,
    SAMPLES = samp,
    POSITIONS = pos,
    OBSERVATIONS = obs
  )

  return(tables_list)
}
