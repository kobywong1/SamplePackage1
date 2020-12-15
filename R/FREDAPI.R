#' Federal Reserve Economic Data(FRED) API Query
#'
#' @param APIkey Your FRED API Key
#' @param series_id Series ID of the data from FRED
#' @param observation_start Start of Observation ("yyyy-mm-dd")
#'
#' @return Data from Specified FRED Query
#' @export
#'
#' @examples fred_get_series("1234567890","UNRATE","1999-01-01")
fred_get_series <- function(APIkey, series_id, observation_start){
  library(jsonlite)
  URL = "https://api.stlouisfed.org/fred/series/observations"

  parameters = paste(
    "?series_id=",series_id,
    "&api_key=", APIkey,
    "&file_type=json",
    "&observation_start=",observation_start,
    sep = "")
  PATH = paste0(URL, parameters)

  initialquery = fromJSON(PATH)
  df = initialquery$observations
  rownames(df) <- df$date

  df = df[c("value")]
  df$value <- as.numeric(df$value)

  return(df)
}

