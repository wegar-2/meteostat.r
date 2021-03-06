
#' Download the full list set of stations
#'
#' Download the full list of Meteostat's stations (updated daily)
#'
#' This function downloads and parses the file with the full list of meteo stations
#' that feed data to Meteostat's API. Cf.:
#' https://dev.meteostat.net/bulk/stations.html#endpoints
#' @return data.table
#' @export
dtDownloadStationsFile <- function() {

  # 1. download the file -------------------------------------------------------
  cSourceFilePath <- "https://bulk.meteostat.net/stations/full.json.gz"
  cDestinationFilePath <- file.path(getwd(), "full.json.gz")
  download.file(url = cSourceFilePath, destfile = cDestinationFilePath)

  # 2. unpack the file ---------------------------------------------------------
  R.utils::gunzip(cDestinationFilePath)

  # 3. parse JSON to list ------------------------------------------------------
  res <- jsonlite::fromJSON(txt = file.path(getwd(), "full.json"))

  # 4. create dictionary table of weather stations from ------------------------
  #    fetched response
  dtStationsDict <- data.table::data.table(
    iMeteostatStationId = res$id,
    cStationName = res$name$en,
    cStationCountryName = res$country,
    cStationRegionName = res$region,
    iStationWmoId = res$identifiers$wmo,
    dStationLatitude = res$location$latitude,
    dStationLongitude = res$location$longitude,
    dStationElevation = res$location$elevation,
    cStationTimezone = res$timezone,
    cStationInventoryStartDate = res$inventory$daily$start,
    cStationInventoryEndDate = res$inventory$daily$end
  )

  return(dtStationsDict)
}

# library(data.table)
# library(jsonlite)
# library(magrittr)
