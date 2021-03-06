#' Download the full list set of stations
#'
#' Download the full list of Meteostat's stations (updated daily)
#'
#' This function downloads and parses the file with the full list of meteo stations
#' that feed data to Meteostat's API. Cf.:
#' https://dev.meteostat.net/bulk/stations.html#endpoints
#' @return data.table with the following columns:
#' @export
dtGetMeteostatWeatherStationsDict <- function() {

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
    meteostat_station_id = res$id,
    name = res$name$en,
    country = res$country,
    region_name = res$region,
    wmo_station_id = res$identifiers$wmo,
    station_latitude = res$location$latitude,
    station_longitude = res$location$longitude,
    station_elevation = res$location$elevation,
    station_timezone = res$timezone,
    inventory_start_date = res$inventory$daily$start,
    inventory_end_date = res$inventory$daily$end, stringsAsFactors = FALSE)

  # 5. get rid of redundant downloaded file ------------------------------------
  message("removing downloaded & unpacked full.json file...")
  file.remove(file.path(getwd(), "full.json"))
  message("successfully tidied up")

  # 6. parse stations dictionary further ---------------------------------------
  inventory_start_date <- inventory_end_date <- NULL
  dtStationsDict[, inventory_start_date := as.Date(inventory_start_date, "%Y-%m-%d")]
  dtStationsDict[, inventory_end_date := as.Date(inventory_end_date, "%Y-%m-%d")]

  return(dtStationsDict)
}


