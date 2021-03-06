# ------------------------------------------------------------------------------
# This script contains function for fetching of daily Meteostat's data - for the API
# description of the data cf.: https://dev.meteostat.net/api/point/daily.html
# ------------------------------------------------------------------------------

# cStationCountryName <- 12120L
# dtPlStationsDict <- dtStationsDict[cStationCountryName == "PL", ]
iMeteostatStationId <- 12120L
dateStartDate <- as.Date("2019-01-01")
dateEndDate <- as.Date("2019-12-31")
cApiKey <- "y2fkAGRIDf3fc88pnDxbkCoJnygfnDbg"


#'
#'
#'
dtGetDailyStationData <- function(iMeteostatStationId, cApiKey,
                                  dateStartDate, dateEndDate) {

  # 1. parameters validation ---------------------------------------------------
  # 1.1. iMeteostatStationId
  if (!bIsScalarOfType(objIn = iMeteostatStationId, cTypeName = "integer")) {
    stop("Error inside dtGetDailyStationData call: the parameter iMeteostatStationId ",
         "is not an integer scalar! ")
  }
  # 1.2. cApiKey
  if (!bIsScalarOfType(objIn = cApiKey, cTypeName = "character")) {
    stop("Error inside dtGetDailyStationData call: the parameter cApiKey ",
         "is not a character scalar! ")
  }
  # 1.3. dateStartDate
  if (!lubridate::is.Date(x = dateStartDate) | length(x = dateStartDate) != 1) {
    stop("Error inside dtGetDailyStationData call: the parameter dateStartDate ",
         "is not a Date class scalar! ")
  }
  # 1.4. dateEndDate
  if (!lubridate::is.Date(x = dateEndDate) | length(x = dateEndDate) != 1) {
    stop("Error inside dtGetDailyStationData call: the parameter dateEndDate ",
         "is not a Date class scalar! ")
  }
  # 1.5. check consistency of the dateStartDate and dateEndDate
  if (dateStartDate > dateEndDate) {
    stop("Error inside dtGetDailyStationData call: the parameters dateStartDate and  ",
         "dateEndDate are inconsistent: dateStartDate precedes dateEndDate!")
  }
  # 1.6. check if the period that the data is queried for is shorter than 370 day
  iPeriodLengthInDays <- (difftime(time1 = dateEndDate, time2 = dateStartDate, units = "days") %>% as.integer()) + 1L
  if (iPeriodLengthInDays > 370) {
    stop()
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
  res <- tryCatch(expr = {
    message("Running GET on ", cQueryUrl)
    #
    httr::GET(cQueryUrl, config = httr::add_headers("x-api-key" = cApiKey))
  }, error = function(er) {
    stop("Error occurred during call to fetch the daily Meteostat's data from ",
         cQueryUrl, "; specific error message: ", er)
  }, finally = {
    message("after fetching via GET call")
  })

  # retrieve the status code and print info
  iStatusCode <- as.integer(res$status_code)


  # 4. process retrieved response ----------------------------------------------
  # 4.1. parse the JSON to data.frame
  dfData <- jsonlite::fromJSON(txt = httr::content(res, "text"))
  # 4.2.
  # 4.3.

  return(dtData)
}








