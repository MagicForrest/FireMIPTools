

standard.quantities.crosswalk <- list(
  list(FireMIP = "lai", Standard = "LAI_std"),
  list(FireMIP = "BA", Standard = "burntfraction_std", Factor = 1/100)
)

standardToFireMIPQuantity <- function(standard.quant){

  firemip.string <- NULL
  for(possible.quant in standard.quantities.crosswalk) {

    if(standard.quant == possible.quant$Standard) {
      firemip.string <- possible.quant$FireMIP
    }

  }

  return(firemip.string)

}

getConversionFactorToStandard <- function(standard.id){

  conversion.factor <- NULL
  for(standard.quant in standard.quantities.crosswalk) {

    if(standard.id == standard.quant$Standard) {
      conversion.factor <- standard.quant$Factor
    }

  }

  return(conversion.factor)

}

FireMIPtoStandardQuantity <- function(firemip.quant){

  standard.string <- NULL
  for(possible.quant in standard.quantities.crosswalk) {

    if(firemip.quant == possible.quant$FireMIP) {
      standard.string <- standard.quant$Standard
    }

  }

  return(standard.string)

}
