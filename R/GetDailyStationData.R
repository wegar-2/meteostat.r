# ------------------------------------------------------------------------------
# This script contains function for fetching of daily Meteostat's data - for the API
# description of the data cf.: https://dev.meteostat.net/api/point/daily.html
# ------------------------------------------------------------------------------


#' Fetch daily Meteostat data for a station
#'
#' Fetch daily Meteostat data for a station for at most yearly interval
#'
#' This function fetches data made available via Meteostat's API. Please note that
#' due to the cap on the size of the window for which the data can be fetched,
#' this function can only handle requests that ask for data for up to 370 days.
#' For generalized version of this function capable of handling arbitrary
#' time intervals cf. dtGetDailyStationData
#' @param iMeteostatStationId - integer scalar, ID of the station as given by
#' Meteostat's dictionary table of weather stations
#' @param cApiKey - character scalar, Meteostat's key to use to fetch the data
#' @param dateStartDate - Date class scalar, first date of the interval for which
#' the data is to be obtained
#' @param dateEndDate - Date class scalar, last date of the interval for which
#' the data is to be obtained
#' @return data.table with the data fetched; the colnames are self-explanatory,
#' in case of doubt cf. the implementation and API's documentation:
#' https://dev.meteostat.net/api/stations/daily.html#response
#' @export
dtGetDailyStationDataOverUpToOneYear <- function(iMeteostatStationId, cApiKey,
                                                 dateStartDate, dateEndDate) {

  # 1. parameters validation ---------------------------------------------------
  # 1.1. iMeteostatStationId
  if (!bIsScalarOfType(objIn = iMeteostatStationId, cTypeName = "integer")) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter iMeteostatStationId ",
         "is not an integer scalar! ")
  }
  # 1.2. cApiKey
  if (!bIsScalarOfType(objIn = cApiKey, cTypeName = "character")) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter cApiKey ",
         "is not a character scalar! ")
  }
  # 1.3. dateStartDate
  if (!lubridate::is.Date(x = dateStartDate) | length(x = dateStartDate) != 1) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter dateStartDate ",
         "is not a Date class scalar! ")
  }
  # 1.4. dateEndDate
  if (!lubridate::is.Date(x = dateEndDate) | length(x = dateEndDate) != 1) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter dateEndDate ",
         "is not a Date class scalar! ")
  }
  # 1.5. check consistency of the dateStartDate and dateEndDate
  if (dateStartDate > dateEndDate) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameters dateStartDate and  ",
         "dateEndDate are inconsistent: dateStartDate precedes dateEndDate!")
  }
  # 1.6. check if the period that the data is queried for is shorter than 370 day
  iPeriodLengthInDays <- (difftime(time1 = dateEndDate, time2 = dateStartDate, units = "days") %>% as.integer()) + 1L
  if (iPeriodLengthInDays > 370) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameters dateStartDate and  ",
         "dateEndDate indicate an interval that is longer than 370 days!!")
  }

  # 2. make query URL ----------------------------------------------------------
  # 2.1. base of the query
  cUrlBase <- "https://api.meteostat.net/v2/stations/daily?"
  # 2.2. station id clause
  cStationIdClause <- paste0("station=", iMeteostatStationId)
  # 2.3. time interval
  cStartDate <- format(dateStartDate, format = "%Y-%m-%d")
  cEndDate <- format(dateEndDate, format = "%Y-%m-%d")
  cTimeIntervalClause <- paste0("start=", cStartDate, "&end=", cEndDate)
  # 2.4. make the final Url
  cQueryUrl <- paste0(cUrlBase, cStationIdClause, "&", cTimeIntervalClause)

  # 3. run the query to fetch the data -----------------------------------------
  # 3.1. run the API request
  res <- tryCatch(expr = {
    message("Running GET on ", cQueryUrl)
    httr::GET(cQueryUrl, config = httr::add_headers("x-api-key" = cApiKey))
  }, error = function(er) {
    stop("Error occurred during call to fetch the daily Meteostat's data from ",
         cQueryUrl, "; specific error message: ", er)
  }, finally = {
    message("after fetching via GET call")
  })
  # 3.2. retrieve the status code and print info
  iStatusCode <- as.integer(res$status_code)
  cStatusMessage <- cGetMeteostatStatusCodeMessage(iStatusCode = iStatusCode)
  message("status message: ", cStatusMessage)

  # 4. process retrieved response ----------------------------------------------
  # 4.1. parse the JSON to data.frame
  lData <- jsonlite::fromJSON(txt = httr::content(res, "text"))
  # 4.2. retrieve data
  dtData <- lData$data %>% data.table::as.data.table()
  # 4.3. rename the columns
  cOldColnames <- c(
    "date", "tavg", "tmin", "tmax",
    "prcp", "snow",
    "wdir", "wspd", "wpgt",
    "pres", "tsun")
  cNewColnames <- c(
    "data_date", "temp_avg_deg_Cel", "temp_min_deg_Cel", "temp_max_deg_Cel",
    "total_precipitation_mm", "snow_depth_mm",
    "wind_direction_deg", "wind_speed_kmph", "wind_peak_gust_kmph",
    "pressure_avg_sea_level_hPa", "daily_sunshine_min")
  data.table::setnames(x = dtData, old = cOldColnames, new = cNewColnames)
  # 4.4. cast as correct types
  # 4.4.1. date of the observation
  data_date <- NULL
  dtData[, data_date := as.Date(data_date, format = "%Y-%m-%d")]
  # 4.4.2. all the rest as floats
  cFloatCols <- setdiff(x = colnames(dtData), y = c("data_date"))
  dtData[, (cFloatCols) := lapply(.SD, as.double), .SDcols = cFloatCols]
  # 4.5. add the column with the meteostat's station id
  dtData[, meteostat_station_id := iMeteostatStationId]
  # 4.6. set data_date as the key
  data.table::setkey(x = dtData, "data_date")
  return(dtData)
}



#'
#'
#'
# dtGetDailyStationData <- function(iMeteostatStationId, cApiKey,
#                                   dateStartDate, dateEndDate) {
#
#   # 1. parameters validation ---------------------------------------------------
#   # 1.1. iMeteostatStationId
#   if (!bIsScalarOfType(objIn = iMeteostatStationId, cTypeName = "integer")) {
#     stop("Error inside dtGetDailyStationData call: the parameter iMeteostatStationId ",
#          "is not an integer scalar! ")
#   }
#   # 1.2. cApiKey
#   if (!bIsScalarOfType(objIn = cApiKey, cTypeName = "character")) {
#     stop("Error inside dtGetDailyStationData call: the parameter cApiKey ",
#          "is not a character scalar! ")
#   }
#   # 1.3. dateStartDate
#   if (!lubridate::is.Date(x = dateStartDate) | length(x = dateStartDate) != 1) {
#     stop("Error inside dtGetDailyStationData call: the parameter dateStartDate ",
#          "is not a Date class scalar! ")
#   }
#   # 1.4. dateEndDate
#   if (!lubridate::is.Date(x = dateEndDate) | length(x = dateEndDate) != 1) {
#     stop("Error inside dtGetDailyStationData call: the parameter dateEndDate ",
#          "is not a Date class scalar! ")
#   }
#   # 1.5. check consistency of the dateStartDate and dateEndDate
#   if (dateStartDate > dateEndDate) {
#     stop("Error inside dtGetDailyStationData call: the parameters dateStartDate and  ",
#          "dateEndDate are inconsistent: dateStartDate precedes dateEndDate!")
#   }
#
#   # 2. split the time range into yearly intervals ------------------------------
#
#
#   # 3. fetch the data iteratively for the intervals ----------------------------
#   lData <- vector(mode = "list", length = length())
#
#   # 4. concatenate and process the data before returning -----------------------
#
#   return(dtData)
# }
