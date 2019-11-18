

#' @format An S4 class object with the slots as defined below.
#' @keywords datasets
#' @export
LPJ_GUESS_PFTs  <- list(

  # TREES

  new("Layer",
      id = "BNE",
      name = "Boreal Needleleaved Evergreen Tree",
      colour = "darkblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "BINE",
      name = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
      colour = "dodgerblue3",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "BNE")
  ),

  BNS = new("Layer",
            id = "BNS",
            name = "Boreal Needleleaved Summergreen Tree",
            colour = "cadetblue2",
            properties = list(type = "PFT",
                              growth.form = "Tree",
                              leaf.form = "Needleleaved",
                              phenology = "Summergreen",
                              climate.zone = "Boreal",
                              shade.tolerance = "None")
  ),

  new("Layer",
      id = "IBS",
      name = "Shade-intolerant B/leaved Summergreen Tree",
      colour = "chartreuse",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      colour = "darkgreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "TeNE",
      name = "Temperate Needleleaved Evergreen Tree",
      colour = "lightseagreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Temperate",
                        colour = "lightseagreen",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "TeBS",
      name = "Temperate Broadleaved Summergreen Tree",
      colour = "darkolivegreen3",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      colour = "orchid4",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "TrIBE",
      name = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
      colour = "orchid",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "TrBE")
  ),

  new("Layer",
      id = "TrBR",
      name = "Tropical Broadleaved Raingreen Tree",
      colour = "palevioletred",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Raingreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "None")
  ),


  # GRASSES

  new("Layer",
      id = "C3G",
      name = "Boreal/Temperate Grass",
      colour = "lightgoldenrod1",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None")
  ),

  new("Layer",
      id = "C4G",
      name = "Tropical Grass",
      colour = "sienna2",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None")
  ),


  new("Layer",
      id = "C3G_pas",
      name = "C3 Pasture Grass",
      colour = "lightgoldenrod4",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "C4G_pas",
      name = "C4 Pasture Grass",
      colour = "sienna",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # CROPS

  new("Layer",
      id = "TeSW",
      name = "Temperate Summer Wheat",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "TeSWirr",
      name = "Irrigated Temperate Summer Wheat",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "TeWW",
      name = "Temperate Winter Wheat",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "TeWWirr",
      name = "Irrigated Temperate Winter Wheat",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "TeCo",
      name = "Temperate Corn",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "TeCoirr",
      name = "Irrigated Temperate Corn",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  )

)

#' @format An S4 class object with the slots as defined below.
#' @keywords datasets
#' @export
JULES_PFTs<- list(

  # TREES

  # NE
  new("Layer",
      id = "NE",
      name = "Needleleaved Evergreen Tree",
      colour = "darkblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Extratropical",
                        shade.tolerance = "no")
  ),

  # ND
  new("Layer",
      id = "ND",
      name = "Needleleaved Deciduous Tree",
      colour = "cornflowerblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Deciduous",
                        climate.zone = "Extratropical",
                        shade.tolerance = "no")
  ),

  # BS
  new("Layer",
      id = "BS",
      name = "Broadleaved Deciduous Tree",
      colour = "chartreuse",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Deciduous",
                        climate.zone = "Extratropical",
                        shade.tolerance = "no")
  ),

  # TrBE
  new("Layer",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      colour = "darkgreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "no")
  ),

  # TeBE
  new("Layer",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      colour = "green",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Extratropical",
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
                        climate.zone = "Extratropical",
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
                        climate.zone = "Tropical",
                        shade.tolerance = "no")
  ),

  # AGRICULTURAL

  # C3G Pasture
  new("Layer",
      id = "C3Pasture",
      name = "Boreal/Temperate Pasture",
      colour = "lightgoldenrod1",
      properties = list(type = "PFT",
                        growth.form = "Crop",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "Extratropical",
                        shade.tolerance = "no")
  ),

  # C4G Pasture
  new("Layer",
      id = "C4G",
      name = "Tropical Pasture",
      colour = "sienna2",
      properties = list(type = "PFT",
                        growth.form = "Crop",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "Tropical",
                        shade.tolerance = "no")
  ),

  # C3G Crop
  new("Layer",
      id = "C3Crop",
      name = "Boreal/Temperate Crop",
      colour = "lightgoldenrod1",
      properties = list(type = "PFT",
                        growth.form = "Crop",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "Extratropical",
                        shade.tolerance = "no")
  ),

  # C4G Crop
  new("Layer",
      id = "C4G",
      name = "Tropical Crop",
      colour = "sienna2",
      properties = list(type = "PFT",
                        growth.form = "Crop",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "Tropical",
                        shade.tolerance = "no")
  ),


  # SHRUBS

  # Ev_Shb
  new("Layer",
      id = "Ev_Shb",
      name = "Evergreen Shrub",
      colour = "darkred",
      properties = list(type = "PFT",
                        growth.form = "Shrub",
                        leaf.form = "NA",
                        phenology = "Evergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # De_Shb
  new("Layer",
      id = "De_Shb",
      name = "Deciduous Shrub",
      colour = "palevioletred1",
      properties = list(type = "PFT",
                        growth.form = "Shrub",
                        leaf.form = "NA",
                        phenology = "Deciduous",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # OTHER

  # Urban
  new("Layer",
      id = "Urban",
      name = "Urban",
      colour = "grey50",
      properties = list(type = "PFT",
                        growth.form = "NA",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # Water
  new("Layer",
      id = "Water",
      name = "Water",
      colour = "lightblue3",
      properties = list(type = "PFT",
                        growth.form = "NA",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # Bare
  new("Layer",
      id = "Bare",
      name = "Bare",
      colour = "grey90",
      properties = list(type = "PFT",
                        growth.form = "NA",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # Ice
  new("Layer",
      id = "Ice",
      name = "Ice",
      colour = "lightblue1",
      properties = list(type = "PFT",
                        growth.form = "NA",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  )

)



#' @export
LPJmL_PFTs <- list(

  # BOREAL TREES

  # BNE
  new("Layer",
      id = "BNE",
      name = "Boreal Needleleaved Evergreen Tree",
      colour = "darkblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None")
  ),

  # BBS
  new("Layer",
      id = "BBS",
      name = " Boreal Broadleaved Summergreen Tree",
      colour = "chartreuse",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),

  # BNS
  new("Layer",
      id = "BNS",
      name = "Boreal Needleleaved Summergreen Tree",
      colour = "cadetblue2",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Summergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None")
  ),



  # TEMPERATE TREES

  # TeBE
  new("Layer",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      colour = "darkgreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),

  # TeNE
  new("Layer",
      id = "TeNE",
      name = "Temperate Needleleaved Evergreen Tree",
      colour = "lightseagreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),

  # TeBS
  new("Layer",
      id = "TeBS",
      name = "Temperate Broadleaved Summergreen Tree",
      colour = "darkolivegreen3",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None")
  ),


  # TROPICAL TREES

  # TrBE
  new("Layer",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      colour = "orchid4",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "None")
  ),


  # TrBR
  new("Layer",
      id = "TrBR",
      name = "Tropical Broadleaved Raingreen Tree",
      colour = "palevioletred",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Raingreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "None")
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
                        shade.tolerance = "None")
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
                        shade.tolerance = "None")
  ),

  # C3Polar
  new("Layer",
      id = "C3Polar",
      name = "Polar Grass",
      colour = "black",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None")
  )

)



#' @format An S4 class object with the slots as defined below.
#' @keywords datasets
#' @export
CTEM_PFTs<- list(

  # TREES

  new("Layer",
      id = "NDL-EVG",
      name = "Needleleaved Evergreen Tree",
      colour = "darkblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),


  new("Layer",
      id = "NDL-DCD",
      name = "Needleleaved Summergreen Tree",
      colour = "cornflowerblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Deciduous",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "BDL-DCD-COLD",
      name = "Broadleaved Cold Deciduous Tree",
      colour = "cyan",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "BDL-EVG",
      name = "Broadleaved Evergreen Tree",
      colour = "darkgreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "BDL-DCD-DRY",
      name = "Broadleaved Dry Deciduous Tree",
      colour = "maroon",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Raingreen",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  # GRASSES

  new("Layer",
      id = "C3G-GRASS",
      name = "Boreal/Temperate Grass",
      colour = "lightgoldenrod1",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "C4G-GRASS",
      name = "Tropical Grass",
      colour = "sienna2",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),


  # CROPS

  new("Layer",
      id = "C3-CROP",
      name = "Agricultural",
      colour = "palegreen",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  ),

  new("Layer",
      id = "C4-CROP",
      name = "Agricultural",
      colour = "palegoldenrod",
      properties = list(type = "PFT",
                        growth.form = "Agricultural",
                        leaf.form = "NA",
                        phenology = "NA",
                        climate.zone = "NA",
                        shade.tolerance = "no")
  )

)


