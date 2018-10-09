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
  new("PFT",
      id = "NE",
      name = "Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "NA",
      colour = "darkblue",
      shade.tolerance = "no"
  ),

  # NS
  new("PFT",
      id = "NS",
      name = "Needleleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Summergreen",
      climate.zone = "NA",
      colour = "cornflowerblue",
      shade.tolerance = "no"
  ),

  # BS
  new("PFT",
      id = "BS",
      name = "Broadleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "NA",
      colour = "cyan",
      shade.tolerance = "no"
  ),

  # BE
  new("PFT",
      id = "BE",
      name = "Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "NA",
      colour = "darkgreen",
      shade.tolerance = "no"
  ),

  # BR
  new("PFT",
      id = "BR",
      name = "Broadleaved Raingreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Raingreen",
      climate.zone = "NA",
      colour = "maroon",
      shade.tolerance = "no"
  ),

  # GRASSES

  # C3G
  new("PFT",
      id = "C3G",
      name = "Boreal/Temperate Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "lightgoldenrod1",
      shade.tolerance = "no"
  ),

  # C4G
  new("PFT",
      id = "C4G",
      name = "Tropical Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "sienna2",
      shade.tolerance = "no"
  ),

  # Shb
  new("PFT",
      id = "Shb",
      name = "Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "darkred",
      shade.tolerance = "no"
  ),

  # Crops
  new("PFT",
      id = "Crops",
      name = "Agricultural",
      growth.form = "Agricultural",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "black",
      shade.tolerance = "no"
  )

)
