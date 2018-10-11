#' @keywords internal
getDimension <- function(nc, dimension, verbose) {

  # determine possible names
  if(tolower(dimension) == "lat") possibles <- c("lat", "Lat", "latitude", "Latitude")
  if(tolower(dimension) == "lon") possibles <- c("lon", "Lon", "longitude", "Longitude")
  if(tolower(dimension) == "time") possibles <- c("time", "Time", "year", "Year")

  # get variables and return if present
  dims.present <- names(nc$dim)
  vars.present <- names(nc$var)

  for(possible in possibles) {

     if(possible %in% vars.present || possible %in% dims.present ) {

      # try to get dimension with exception handling
      this.result <- tryCatch({
        this.dim <- ncvar_get(nc, possible,verbose=verbose)
      }, warning = function(war) {
        return(NA)
      }, error = function(err) {
        return(NA)
      }, finally = {
      })

      # return only if not an NA (ie not an error or a warning)
      if(length(this.result) > 1) return(this.result)

    }

  }

  warning(paste0("Can't find ", dimension, " dimension.  Variables are: ", paste(vars.present, collapse = " "), ". Dimensions present are:",  paste(dims.present, collapse = " "), ". \n If possible, will now look up the gridcell file for this dimension."))

  return(NULL)

}
