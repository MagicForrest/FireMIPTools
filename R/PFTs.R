
#' @export
LPJ_GUESS_PFTs <- list(

  # BOREAL TREES

  # BNE
  new("PFT",
      id = "BNE",
      name = "Boreal Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Boreal",
      colour = "darkblue",
      shade.tolerance = "None"
  ),

  # BINE
  new("PFT",
      id = "BINE",
      name = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Boreal",
      colour = "dodgerblue3",
      shade.tolerance = "BNE"
  ),

  # BNS
  new("PFT",
      id = "BNS",
      name = "Boreal Needleleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Summergreen",
      climate.zone = "Boreal",
      colour = "cadetblue2",
      shade.tolerance = "None"
  ),

  # IBS
  new("PFT",
      id = "IBS",
      name = "Shade-intolerant B/leaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "Temperate",
      colour = "chartreuse",
      shade.tolerance = "None"
  ),

  # TEMPERATE TREES

  # TeBE
  new("PFT",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Temperate",
      colour = "darkgreen",
      shade.tolerance = "None"
  ),

  # TeNE
  new("PFT",
      id = "TeNE",
      name = "Temperate Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Temperate",
      colour = "lightseagreen",
      shade.tolerance = "None"
  ),

  # TeBS
  new("PFT",
      id = "TeBS",
      name = "Temperate Broadleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      colour = "darkolivegreen3",
      climate.zone = "Temperate",
      shade.tolerance = "None"
  ),


  # TROPICAL TREES

  # TrBE
  new("PFT",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Tropical",
      colour = "orchid4",
      shade.tolerance = "None"
  ),


  # TrIBE
  new("PFT",
      id = "TrIBE",
      name = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Tropical",
      colour = "orchid",
      shade.tolerance = "TrBE"
  ),

  # TrBR
  new("PFT",
      id = "TrBR",
      name = "Tropical Broadleaved Raingreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Raingreen",
      climate.zone = "Tropical",
      colour = "palevioletred",
      shade.tolerance = "None"
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
      shade.tolerance = "None"
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
      shade.tolerance = "None"
  )

)

#' @export
JULES_PFTs <- list(

  # TREES

  # NE
  new("PFT",
      id = "NE",
      name = "Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Extratropical",
      colour = "darkblue",
      shade.tolerance = "no"
  ),

  # ND
  new("PFT",
      id = "ND",
      name = "Needleleaved Deciduous Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Deciduous",
      climate.zone = "Extratropical",
      colour = "cornflowerblue",
      shade.tolerance = "no"
  ),

  # BS
  new("PFT",
      id = "BD",
      name = "Broadleaved Deciduous Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Deciduous",
      climate.zone = "Extratropical",
      colour = "chartreuse",
      shade.tolerance = "no"
  ),

  # TrBE
  new("PFT",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Tropical",
      colour = "darkgreen",
      shade.tolerance = "no"
  ),

  # TeBE
  new("PFT",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Extratropical",
      colour = "green",
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
      climate.zone = "Extratropical",
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
      climate.zone = "Tropical",
      colour = "sienna2",
      shade.tolerance = "no"
  ),

  # AGRICULTURAL

  # C3G Pasture
  new("PFT",
      id = "C3Pasture",
      name = "Boreal/Temperate Pasture",
      growth.form = "Pasture",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "Extratropical",
      colour = "lightgoldenrod1",
      shade.tolerance = "no"
  ),

  # C4G Pasture
  new("PFT",
      id = "C4Pasture",
      name = "Tropical Pasture",
      growth.form = "Pasture",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "Tropical",
      colour = "sienna2",
      shade.tolerance = "no"
  ),

  # C3G Crop
  new("PFT",
      id = "C3Crop",
      name = "Boreal/Temperate Crop",
      growth.form = "Crop",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "Extratropical",
      colour = "lightgoldenrod1",
      shade.tolerance = "no"
  ),

  # C4G Crop
  new("PFT",
      id = "C4Crop",
      name = "Tropical Crop",
      growth.form = "Crop",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "Tropical",
      colour = "sienna2",
      shade.tolerance = "no"
  ),




  # SHRUBS

  # Ev_Shb
  new("PFT",
      id = "Ev_Shb",
      name = "Evergreen Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "Evergreen",
      climate.zone = "NA",
      colour = "darkred",
      shade.tolerance = "no"
  ),

  # De_Shb
  new("PFT",
      id = "De_Shb",
      name = "Deciduous Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "Deciduous",
      climate.zone = "NA",
      colour = "palevioletred1",
      shade.tolerance = "no"
  ),

  # OTHER

  # Urban
  new("PFT",
      id = "Urban",
      name = "Urban",
      growth.form = "NA",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "grey50",
      shade.tolerance = "no"
  ),

  # Water
  new("PFT",
      id = "Lake",
      name = "Lake",
      growth.form = "NA",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "lightblue3",
      shade.tolerance = "no"
  ),

  # Bare
  new("PFT",
      id = "Bare",
      name = "Bare",
      growth.form = "NA",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "grey90",
      shade.tolerance = "no"
  ),

  # Ice
  new("PFT",
      id = "Ice",
      name = "Ice",
      growth.form = "NA",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "lightblue1",
      shade.tolerance = "no"
  )

)

#' @export
LPJmL_PFTs <- list(

  # BOREAL TREES

  # BNE
  new("PFT",
      id = "BNE",
      name = "Boreal Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Boreal",
      colour = "darkblue",
      shade.tolerance = "None"
  ),

  # BBS
  new("PFT",
      id = "BBS",
      name = " Boreal Broadleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "Temperate",
      colour = "chartreuse",
      shade.tolerance = "None"
  ),

  # BNS
  new("PFT",
      id = "BNS",
      name = "Boreal Needleleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Summergreen",
      climate.zone = "Boreal",
      colour = "cadetblue2",
      shade.tolerance = "None"
  ),



  # TEMPERATE TREES

  # TeBE
  new("PFT",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Temperate",
      colour = "darkgreen",
      shade.tolerance = "None"
  ),

  # TeNE
  new("PFT",
      id = "TeNE",
      name = "Temperate Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Temperate",
      colour = "lightseagreen",
      shade.tolerance = "None"
  ),

  # TeBS
  new("PFT",
      id = "TeBS",
      name = "Temperate Broadleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      colour = "darkolivegreen3",
      climate.zone = "Temperate",
      shade.tolerance = "None"
  ),


  # TROPICAL TREES

  # TrBE
  new("PFT",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Tropical",
      colour = "orchid4",
      shade.tolerance = "None"
  ),


  # TrBR
  new("PFT",
      id = "TrBR",
      name = "Tropical Broadleaved Raingreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Raingreen",
      climate.zone = "Tropical",
      colour = "palevioletred",
      shade.tolerance = "None"
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
      shade.tolerance = "None"
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
      shade.tolerance = "None"
  ),

  # C3Polar
  new("PFT",
      id = "C3Polar",
      name = "Polar Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "black",
      shade.tolerance = "None"
  )

)

#' @export
CTEM_PFTs <- list(

  # TREES

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


  new("PFT",
      id = "ND",
      name = "Needleleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Deciduous",
      climate.zone = "NA",
      colour = "cornflowerblue",
      shade.tolerance = "no"
  ),

  new("PFT",
      id = "BS",
      name = "Broadleaved Cold Deciduous Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "NA",
      colour = "cyan",
      shade.tolerance = "no"
  ),

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

  new("PFT",
      id = "BR",
      name = "Broadleaved Dry Deciduous Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Raingreen",
      climate.zone = "NA",
      colour = "maroon",
      shade.tolerance = "no"
  ),

  # GRASSES

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


  # CROPS

  new("PFT",
      id = "C3Crop",
      name = "C3 Crop",
      growth.form = "Crop",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "palegreen",
      shade.tolerance = "no"
  ),

  new("PFT",
      id = "C4Crop",
      name = "C4 Crop",
      growth.form = "Crop",
      leaf.form = "NA",
      phenology = "NA",
      climate.zone = "NA",
      colour = "palegoldenrod",
      shade.tolerance = "no"
  )



)

