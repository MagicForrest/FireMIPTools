######################################################################
########## FIREMIP QUANTITIES  ######################################
######################################################################
#'
#'
#' @format The \code{Quantity} class is an S4 class with the slots defined below.
#' @rdname Quantity-class
#' @keywords datasets
#' @import DGVMTools
#' @importFrom methods new
#'
FireMIP.quantities <- list(

  #### BURNT AREA AND EMISSIONS

  new("Quantity",
      id = "fFirePFT",
      name = "C emitted from fire (per PFT)",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "C emitted from fire (per PFT)"),

  new("Quantity",
      id = "fFire",
      name = "C emitted from fire",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "CO2 emitted from fire"),

  new("Quantity",
      id = "coFire",
      name = "CO emitted from fire",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "CO emitted from fire"),

  new("Quantity",
      id = "Cfire",
      name = "C emitted from fire",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "C emitted from fire"),

  new("Quantity",
      id = "burntArea",
      name = "Burnt Area Fraction (per PFT)",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Burnt Area Fraction (per PFT)"),

  new("Quantity",
      id = "BA",
      name = "Burnt Area Fraction (monthly)",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Burnt Area Fraction (monthly)"),

  new("Quantity",
      id = "fFirepft",
      name = "C emitted from fire (per PFT)",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "C emitted from fire (per PFT)"),



  ### FUEL LOADS

  new("Quantity",
      id = "cFuelLiveGrass",
      name = "Carbon in live grass fuel",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in live grass fuel"),

  new("Quantity",
      id = "cFuel",
      name = "Carbon in fuel fuel",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in fuel"),

  new("Quantity",
      id = "cFuel1hr",
      name = "Carbon in 1hr fuel",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in 1hr fuel"),

  new("Quantity",
      id = "cFuel10hr",
      name = "Carbon in 10hr fuel",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in 10hr fuel"),

  new("Quantity",
      id = "cFuel100hr",
      name = "Carbon in 100hr fuel",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in 100hr fuel"),

  new("Quantity",
      id = "cFuel1000hr",
      name = "Carbon in 1000hr fuel",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in 1000hr fuel"),


  ### COMBUSION COMPLETENESS

  new("Quantity",
      id = "ccFuelLiveGrass",
      name = "Combusion Completeness in live grass fuel",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Combusion Completeness live grass fuel"),

  new("Quantity",
      id = "ccFuel1hr",
      name = "Combustion Completeness in 1hr fuel",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Combustion Completenessin 1hr fuel"),

  new("Quantity",
      id = "ccFuel10hr",
      name = "Combustion Completenessin 10hr fuel",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Combustion Completeness in 10hr fuel"),

  new("Quantity",
      id = "ccFuel100hr",
      name = "Combustion Completenessin 100hr fuel",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Combustion Completeness in 100hr fuel"),

  new("Quantity",
      id = "ccFuel1000hr",
      name = "Combustion Completenessin 1000hr fuel",
      units = "%",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Combustion Completeness in 1000hr fuel"),


  ### FUEL MOISTURE

  new("Quantity",
      id = "mFuelDead",
      name = "Fuel moisture of dead fuel",
      units = "",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Fuel moisture of dead fuel"),

  new("Quantity",
      id = "mFuelLiveGrass",
      name = "Fuel moisture of live grass fuel",
      units = "",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Fuel moisture of live grass fuel"),


  ### FIRE PROPERTIES AND MORTALITY

  new("Quantity",
      id = "intensFire",
      name = "Fireline intensity",
      units = "kW m-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Fireline intensity"),

  new("Quantity",
      id = "nrfire",
      name = "Number of fires",
      units = "nr m-2 month-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Number of fires"),

  new("Quantity",
      id = "meanFire",
      name = "Mean fire size",
      units = "m2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Mean fire size"),

  new("Quantity",
      id = "cMortality",
      name = "Number of individuals killed",
      units = "indiv m-2 month-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Number of individuals killed"),

  new("Quantity",
      id = "RoS",
      name = "Mean rate of spread",
      units = "m/s",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Mean rate of spread"),

  new("Quantity",
      id = "durat",
      name = "Mean fire duration",
      units = "min",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Mean fire duration"),

  ### HYDROLOGICAL VARIABLES

  new("Quantity",
      id = "mrro",
      name = "Total Runoff",
      units = "kg m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Total Runoff"),

  new("Quantity",
      id = "evapotrans",
      name = "Total Evapo-Transpiration",
      units = "kg m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Total Evapo-Transpiration"),

  new("Quantity",
      id = "mrso",
      name = "Total Soil Moisture Content",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Total Soil Moisture Content"),

  new("Quantity",
      id = "mrsos",
      name = "Surface Layer (50cm) Soil Moisture Content",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Surface Layer (50cm) Soil Moisture Content"),

  new("Quantity",
      id = "evspsblveg",
      name = "Evaporation from Canopy",
      units = "kg m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Evaporation from Canopy"),

  new("Quantity",
      id = "evspsblsoi",
      name = "Evaporation from Soil",
      units = "kg m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Evaporation from Soil"),

  new("Quantity",
      id = "tran",
      name = "Transpiration",
      units = "kg m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Transpiration"),

  new("Quantity",
      id = "tsl",
      name = "Temperature of soil",
      units = "K",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Temperature of soil"),



  ### FLUXES

  new("Quantity",
      id = "gpp",
      name = "Gross Primary Production",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Gross Primary Production"),

  new("Quantity",
      id = "npp",
      name = "Net Primary Production",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Net Primary Production"),

  new("Quantity",
      id = "nbp",
      name = "Net Biospheric Production",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Net Biospheric Production"),

  new("Quantity",
      id = "ra",
      name = "Autotrophic (Plant) Respiration",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Autotrophic (Plant) Respiration"),

  new("Quantity",
      id = "rh",
      name = "Heterotrophic Respiration",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Heterotrophic Respiration"),

  new("Quantity",
      id = "gpppft",
      name = "Vegtype level GPP",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Vegtype level GPP"),

  new("Quantity",
      id = "npppft",
      name = "Vegtype level NPP",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Vegtype level NPP"),

  new("Quantity",
      id = "nbppft",
      name = "Vegtype level NBP",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP"),
      cf.name = "Vegtype level NBP"),

  new("Quantity",
      id = "fLuc",
      name = "CO2 Flux to Atmosphere from Land Use Change",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "CO2 Flux to Atmosphere from Land Use Change"),


  # POOLS

  new("Quantity",
      id = "cVegpft",
      name = "Vegtype level Carbon in Vegetation",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Vegtype level Carbon in Vegetation"),

  new("Quantity",
      id = "cVeg",
      name = "Carbon in Vegetation",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in Vegetation"),

  new("Quantity",
      id = "cLeaf",
      name = "Carbon in Leaves",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("CTEM_FireMIP"),
      cf.name = "Carbon_in_leaves"),

  new("Quantity",
      id = "cRoot",
      name = "Carbon in Roots",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("CTEM_FireMIP"),
      cf.name = "Carbon_in_roots"),

  new("Quantity",
      id = "cWood",
      name = "Carbon in Wood",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("CTEM_FireMIP"),
      cf.name = "Carbon_in_wood"),

  new("Quantity",
      id = "cLitter",
      name = "Carbon in Above-ground Litter Pool",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in Above-ground Litter Pool"),

  new("Quantity",
      id = "cSoil",
      name = "Carbon in Soil (including below-ground litter)",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in Soil (including below-ground litter)"),

  new("Quantity",
      id = "cSoilpft",
      name = "Carbon in Soil per PFT (including below-ground litter)",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Vegtype soil carbon"),

  new("Quantity",
      id = "fLitterSoil",
      name = "Total Carbon Flux from Litter to Soil",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP"),
      cf.name = "Total Carbon Flux from Litter to Soil"),

  new("Quantity",
      id = "fVegLitter",
      name = "Total Carbon Flux from Vegetation to Litter",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP"),
      cf.name = "Total Carbon Flux from Vegetation to Litter"),


  ###  STRUCTURE

  new("Quantity",
      id = "landCoverFrac",
      name = "Fractional Land Cover of PFT",
      units = "",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Fractional Land Cover of PFT"),

  new("Quantity",
      id = "lai",
      name = "Leaf Area Index",
      units = "",
      colours = reversed.viridis,
      format = "FireMIP",
      cf.name = "Leaf Area Index"),

  new("Quantity",
      id = "theightpft",
      name = "Vegtype level tree heights",
      units = "m",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Vegtype level tree heights"),

  ### LAND USE

  new("Quantity",
      id = "cProduct",
      name = "Carbon in Products of Land Use Change",
      units = "kg C m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Carbon in Products of Land Use Change"),

  new("Quantity",
      id = "fLuc",
      name = "CO2 Flux to Atmosphere from Land Use Change",
      units = "kg C m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "CO2 Flux to Atmosphere from Land Use Change"),

  ### CLIMATE

  new("Quantity",
      id = "pr",
      name = "Precipitation",
      units = "kg m-2 s-1",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Precipitation"),

  new("Quantity",
      id = "rsds",
      name = "Surface_Downwelling_Shortwave-Radiation",
      units = "W m-2",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Surface_Downwelling_Shortwave-Radiation"),

  new("Quantity",
      id = "tas",
      name = "Near-Surface_Air_Temperature",
      units = "K",
      colours = reversed.viridis,
      format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
      cf.name = "Near-Surface_Air_Temperature")



)
