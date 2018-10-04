#' @keywords internal
getDimension <- function(nc, dimension, verbose) {

  # determine possible names
  if(tolower(dimension) == "lat") possibles <- c("lat", "Lat", "latitude", "Latitude", "y", "Y")
  if(tolower(dimension) == "lon") possibles <- c("lon", "Lon", "longitude", "Longitude", "x", "X")
  if(tolower(dimension) == "time") possibles <- c("time", "Time")

  # get variables and return if present
  vars.present <- names(nc$var)
  for(possible in possibles) {
    if(possible %in% vars.present) return(ncvar_get(nc, possible,verbose=verbose))
  }

  # if time dimension only (for some reasons), get dimensions and return if present
  if(tolower(dimension) == "time") {
    dims.present <- names(nc$dim)
    for(possible in possibles) {
      if(possible %in% dims.present) return(ncvar_get(nc, possible,verbose=verbose))
    }
  }

  if(tolower(dimension) == "time") warning(paste0("Can't find ", dimension, " dimension.  Variables are: ", paste(vars.present, collapse = " "), ". Dimensions present are:",  paste(dims.present, collapse = " ")))
  else warning(paste0("Can't find ", dimension, " dimension.  Variables are: ", paste(vars.present, collapse = " ")))

  return(NULL)

}
