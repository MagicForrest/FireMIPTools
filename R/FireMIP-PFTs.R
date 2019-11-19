#' FireMIP PFTs
#'
#' A coarse but simple PFT set to which all the FireMIP models PFTs may be classified.
#'
#' @format An S4 class object with the slots as defined below.
#' @keywords datasets
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import DGVMTools
#' @importFrom methods new
#' @export

FireMIP.PFTs <- list(

  # BOREAL TREES

  # NE
  new("Layer",
      id = "NE",
      name = "Needleleaved Evergreen Tree",
      colour = "darkblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # NS
  new("Layer",
      id = "NS",
      name = "Needleleaved Summergreen Tree",
      colour = "cornflowerblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Summergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # BS
  new("Layer",
      id = "BS",
      name = "Broadleaved Summergreen Tree",
      colour = "cyan",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # BE
  new("Layer",
      id = "BE",
      name = "Broadleaved Evergreen Tree",
      colour = "darkgreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # BR
  new("Layer",
      id = "BR",
      name = "Broadleaved Raingreen Tree",
      colour = "maroon",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Raingreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # GRASSES

  # C3G
  new("Layer",
      id = "C3G",
      name = "Boreal/Temperate Grass",
      colour = "lightgoldenrod1",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # C4G
  new("Layer",
      id = "C4G",
      name = "Tropical Grass",
      colour = "sienna2",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # Shb
  new("Layer",
      id = "Shb",
      name = "Shrub",
      colour = "darkred",
      properties = list(type = "PFT",
                        growth.form = "Shrub",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # Crops
  new("Layer",
      id = "Crops",
      name = "Agricultural",
      colour = "black",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  )

)
