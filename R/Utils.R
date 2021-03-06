
bIsScalarOfClass <- function(objIn, cClassName) {
  return(methods::is(object = objIn, class2 = cClassName) & length(objIn) == 1L)
}


bIsScalarOfType <- function(objIn, cTypeName) {
  return(typeof(x = objIn) == cTypeName & length(objIn) == 1L)
}


#'
#'
#'
cGetMeteostatStatusCodeMessage <- function(iStatusCode) {

  # 1. input validation --------------------------------------------------------
  if (!bIsScalarOfType(objIn = iStatusCode, cTypeName = "integer")) {
    stop("Error inside cGetMeteostatStatusCodeMessage - parameter iStatusCode ",
         "is not an integer scalar! ")
  }
  cStatusCode <- as.character(x = iStatusCode)

  # 2. dictionary of status codes ----------------------------------------------
  lStatusCodesDict <- list(
    "200" = "Success: All is fine",
    "400" = "Bad Request: Please check the request parameters",
    "401" = "Unauthorized: Make sure to send a valid x-api-key header",
    "403" = "Forbidden: You're not allowed to access this endpoint",
    "404" = "Not Found: This endpoint doesn't exist",
    "429" = "Too Many Requests: You've exceeded the quota",
    "503" = "Service Temporarily Unavailable: The API is currently down"
  )

  # 3. return the relevant status ----------------------------------------------
  if (cStatusCode %in% names(lStatusCodesDict)) {
    return(lStatusCodesDict[[cStatusCode]])
  } else {
    stop("The status code ", cStatusCode, " passed to the function cGetMeteostatStatusCodeMessage ",
         "is not available in the dictionary of the statuses! ")
  }
}
