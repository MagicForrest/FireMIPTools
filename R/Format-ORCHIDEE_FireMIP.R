############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################
#' Open a ORCHIDEE FireMIP output file
#'
#' Opens a .nc file from the ORCHIDEE FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.
#' Returns a data.table, because it is intended to be called by getField(), but of course the data.table could be used directly if you wish
#'
#'
#' @param run A Source object to define the run we want to open.
#' @param quantity A Quantity object to define which variable we want to look up
#' @param first.year The first year we want to read (numeric)
#' @param last.year The last year we want to read (numeric)
#' @param spatial.extent The spatial extent we want to read (as defined by as raster::extent or an object that can be cast to a raster::extent)
#' @param verbose Logical, if TRUE spew forth a lot of info.
#' @import data.table
#' @import DGVMTools
#' @import ncdf4
#' @importFrom stats na.omit
#'
#' @keywords internal
#'
#' @return A list containaing a data.table and an STAInfo object
#'
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export

openFireMIPOutputFile_ORCHIDEE <- function(run, quantity, sta.info, verbose = TRUE) {


  first.year = sta.info@first.year
  last.year = sta.info@last.year


  Year = Lon = landmask = NULL

  # get the name of the model
  print(run@format@id)

  # open the data file and the gridl file
  quantity.string <- quantity@id
  if(quantity.string == "ccFuel1000hr") quantity.string <- "ccFuel_1000h"
  else if(quantity.string == "ccFuel100hr") quantity.string <- "ccFuel_100h"
  else if(quantity.string == "ccFuel10hr") quantity.string <- "ccFuel_10h"
  else if(quantity.string == "ccFuel1hr") quantity.string <- "ccFuel_1h"
  else if(quantity.string == "cFuel1000hr") quantity.string <- "cFuel_1000h"
  else if(quantity.string == "cFuel100hr") quantity.string <- "cFuel_100h"
  else if(quantity.string == "cFuel10hr") quantity.string <- "cFuel_10h"
  else if(quantity.string == "cFuel1hr") quantity.string <- "cFuel_1h"


  # ORCHIDEE is very special, need to unzip a tar archive
  if(!file.exists(file.path(run@dir, quantity.string))) {
    untar.command <- paste("tar xzvf", file.path(run@dir, paste0(quantity.string, ".tar.gz")), "--directory", run@dir)
    print(untar.command)
    system(untar.command)
  }
  this.dir <-  file.path(run@dir, quantity.string)
  if(quantity@id == "evapotrans")  this.dir <- file.path(run@dir, paste0(quantity.string, "New"))
  if(quantity@id == "tran")  this.dir <- file.path(run@dir, paste0(quantity.string, "New"))

  # open the 2013 file to get dimensions etc
  file.string <- file.path(this.dir, paste0(quantity.string, "_", "2013", ".nc"))
  print(file.string)
  this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  # grid.file <- system.file("gridfiles", "ORCHIDEE_grid.nc", package = "FireMIPTools")
  # grid.nc <-  nc_open(grid.file, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )


  # get and print the dimensions and variables present
  dims.present <- names(this.nc$dim)
  print(paste("Dimensions present: ", paste(dims.present, collapse = " ")))
  vars.present <- names(this.nc$var)
  print(paste("Variables present: ", paste(vars.present , collapse = " ")))

  # PFTs - hard coded
  this.pfts <- c("Bare", "TrBE", "TrBR", "TeNE", "TeBE", "TeBS", "BNE", "BBS", "BNS", "C3G", "C4G", "C3_agr", "C4_agr" )


  # get dimensions
  this.lat <- getDimension(this.nc, "lat", verbose)
  # if(is.null(this.lat)) this.lat <- getDimension(grid.nc, "lat", verbose)
  this.lon <- getDimension(this.nc, "lon", verbose)
  # if(is.null(this.lon)) this.lon <- getDimension(grid.nc, "lon", verbose)
  this.time <- getDimension(this.nc, "time", verbose)

  # get the land mask
  # this.landmask <- ncvar_get(grid.nc, "masks", start = c(1,1), count = c(-1,-1))
  # dimnames(this.landmask) <- list(this.lon, this.lat)
  # this.landmask.dt <- as.data.table(melt(this.landmask))
  # setnames(this.landmask.dt, c("Lon", "Lat", "landmask"))
  # this.landmask.dt <- this.landmask.dt[landmask > 0,]
  # this.landmask.dt[, landmask:=NULL]

  # attempt to automagically determine time axis
  is.monthly <- FALSE
  print(length(this.time))

  # monthly starting in 1861 -- CTEM
  if(length(this.time) == 1836) {
    is.monthly <- TRUE
    all.years <- 1861:2013
  }
  # annual starting in 1950 -- CTEM
  else if(length(this.time) == 64) {
    all.years <- 1950:2013
  }
  # annual starting in 1950 -- CTEM
  else if(length(this.time) == 156) {
    all.years <- 1860:2013
  }
  # monthly starting in 1950 -- CTEM
  else if(length(this.time) == 768) {
    is.monthly <- TRUE
    all.years <- 1950:2013
  }
  # monthly -- ORCHIDEE
  else if(length(this.time) == 12) {
    print("monthly")
    print(this.dir)
    print(quantity.string)
    is.monthly <- TRUE

    # also get the number of years present

    # this ugliness is because the list.files command seems to fail occasionally
    repeat{
      all.files <- list.files(this.dir, ".nc")
      if(length(all.files) > 0){
        break
      }
    }

    all.files <- gsub(".nc", "", all.files)
    all.files <- gsub(paste0(quantity.string, "_"), "", all.files)
    all.years <- as.numeric(all.files)
    # Hackery
    if(quantity.string == "nbp") {
      all.years <- 1950:2013
    }
    if(all.years[length(all.years)] - all.years[1] + 1 != length(all.years)) stop("ORCHIDEE files missing")

  }
  else if(length(this.time) == 0) {

    # also get the number of years present

    # this ugliness is because the list.files command seems to fail occasionally
    repeat{
      all.files <- list.files(this.dir, ".nc")
      if(length(all.files) > 0){
        break
      }
    }

    all.files <- gsub(".nc", "", all.files)
    all.files <- gsub(paste0(quantity.string, "_"), "", all.files)
    all.years <- as.numeric(all.files)
    if(all.years[length(all.years)] - all.years[1] + 1 != length(all.years)) stop("ORCHIDEE files missing")
  }

  else {
    stop(paste("Guess time axis for time dimensions length", length(this.time)))
  }



  # also determine if it is perPFT
  is.perPFT <- FALSE
  if(length(vars.present) > 3 || length(dims.present) > 3) {
    if("vegtype" %in% vars.present || "vegtype" %in% dims.present){
      is.perPFT <- TRUE
      this.vegtype <- ncvar_get(this.nc,"vegtype",verbose=verbose)
      #if(length(this.vegtype) == 9) this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS")
      #else if(length(this.vegtype) == 10) this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS", "Bare")
    }
  }

  # close the file
  nc_close(this.nc)


  first.year.output <- all.years[1]
  last.year.output <- all.years[length(all.years)]

  # choose range of years (if specifed, else take the whole range)
  if(is.null(first.year) || length(first.year) == 0) first.year <- first.year.output
  if(is.null(last.year) || length(last.year) == 0) last.year <- last.year.output

  # What we do now depend on how we want the output to be
  full.dt <- data.table()

  t1 <- Sys.time()

  for(counter in first.year:last.year) {

    year.counter <- counter - first.year.output

    # diagnostics
    #this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
    #print(dim(this.slice))

    file.string <- file.path(this.dir, paste0(quantity.string, "_", counter, ".nc"))
    print(paste0("Reading file ", file.string))
    this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
    if(is.perPFT && is.monthly) {
      this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
      dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, 1:12)
    }
    else if(is.monthly){
      this.slice <- ncvar_get(this.nc, start = c(1,1,1), count = c(-1,-1,-1))
      dimnames(this.slice) <- list(this.lon, this.lat,  1:12)
    }
    else if(is.perPFT){
      this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1,-1), collapse_degen = FALSE)
      dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, counter)
    }
    else {
      this.slice <- ncvar_get(this.nc, start = c(1,1,1), count = c(-1,-1,-1), collapse_degen = FALSE)
      dimnames(this.slice) <- list(this.lon, this.lat,  counter)
    }
    nc_close(this.nc)

    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))

    # set names, chuck out the water and set NAs to 0
    if(is.perPFT && is.monthly) { setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Month", quantity@id)) }
    else if(is.monthly) { setnames(this.slice.dt, c("Lon", "Lat", "Month", quantity@id)) }
    else if(is.perPFT) { setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Year", quantity@id)) }
    else { setnames(this.slice.dt, c("Lon", "Lat", "Year", quantity@id)) }
    # use land mask
    #this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)

    # set any remaining NAs to zero
    #for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

    # alternatively, remove NAs
    this.slice.dt <- na.omit(this.slice.dt)


    # add Year dcast back to a column for every PFT
    if(is.monthly) {
      this.slice.dt[, Year := counter]
      new.order <- c("Lon", "Lat", "Year", "Month", quantity@id)
      setcolorder(this.slice.dt, new.order)
    }
    #else {
    #  new.order <- c("Lon", "Lat", "Year", quantity@id)
    #  setcolorder(this.slice.dt, new.order)
    #}
    if(is.perPFT) {
      if(is.monthly) this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year + Month ~ PFT, value.var = quantity@id, fill = 0)
      else this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year ~ PFT, value.var = quantity@id, fill = 0)
    }


    # add it on to the full data.table
    full.dt <- rbind(full.dt, this.slice.dt)
    rm(this.slice, this.slice.dt)

  }
  t2 <- Sys.time()
  print(t2-t1)



  # Tidy stuff
  full.dt <- stats::na.omit(full.dt)
  gc()

  # convert landCoverFrac to fraction
  if(quantity@id == "landCoverFrac") {
    layer.names <- names(full.dt)
    layer.names <- layer.names[!layer.names %in% getDimInfo(full.dt)]
    full.dt[, (layer.names) := .SD /100, .SDcols = layer.names]
  }




  all.years <- sort(unique(full.dt[["Year"]]))
  if(is.monthly) subannual <- "Month"
  else subannual <- "Annual"
  sta.info = new("STAInfo",
                 first.year = min(all.years),
                 last.year = max(all.years),
                 subannual.resolution = subannual,
                 subannual.original = subannual,
                 spatial.extent = extent(full.dt))


  return(list(dt = full.dt,
              sta.info = sta.info))


}


#' Detemine PFTs present in an FireMIP run source
#'
#' @param x  A Source objects describing a FireMIP source
#' @param variables Some variable to look for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.  Currently ignored.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_ORCHIDEE_FireMIP <- function(x, variables) {

  return(x@format@default.pfts)

}



#' List all quantities available for a FireMIP Source
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory.
#' Also ignores some common red herrings like "guess.out" and "*.out"
#'
#' @param source A path to a directory on the file system containing some .out files
#' @return A list of all the .out files present, with the ".out" removed.
#'
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


availableQuantities_ORCHIDEE_FireMIP <- function(source, names){

  # First get the list of *.out files present
  files.present <- list.files(source@dir, ".tar.gz")

  quantities.present <- list()
  for(file in files.present) {

    # remove the.nc
    var.str <- gsub(".tar.gz", "", file)

    #split.thing <- unlist(strsplit(var.str, "_"))
    #var.str <- split.thing[length(split.thing)]
    if(var.str == "alb_nir") var.str <- NULL #  not standard
    else if(var.str == "alb_vis") var.str <- NULL #  not standard
    else if(var.str == "ccFuel_1000h") var.str <- "ccFuel1000hr"
    else if(var.str == "ccFuel_100h") var.str <- "ccFuel100hr"
    else if(var.str == "ccFuel_10h") var.str <- "ccFuel10hr"
    else if(var.str == "ccFuel_1h") var.str <- "ccFuel1hr"
    else if(var.str == "cFuel_1000h") var.str <- "cFuel1000hr"
    else if(var.str == "cFuel_100h") var.str <- "cFuel100hr"
    else if(var.str == "cFuel_10h") var.str <- "cFuel10hr"
    else if(var.str == "cFuel_1h") var.str <- "cFuel1hr"
    else if(var.str == "LAI") var.str <- NULL # what is this?
    else if(var.str == "meanFire_MeanOverTime") var.str <- NULL # what is this?
    else if(var.str == "rnpft") var.str <- NULL # ignore
    else if(var.str == "transpft") var.str <- NULL # ignore
    else if(var.str == "snow_depth") var.str <- NULL # snow depth files are all NA
    else if(var.str == "tsl") var.str <- NULL # ignore multiple layers for now
    # else if(var.str == "intercept") var.str <- NULL # not standard - possibly should be evspslveg
    # else if(var.str == "landCoverFrac") var.str <- NULL # not ignore landCoverFrac
    # else if(var.str == "v2") var.str <- "landCoverFrac" # use landCoverFrac_v2
    # else if(var.str == "trans") var.str <- NULL # not standard
    # else if(var.str == "Cfire2") var.str <- NULL # not standard
    # else if(var.str == "cLitter2") var.str <- NULL # not standard

    if(!is.null(var.str)) {
      if(names) quantities.present <- append(quantities.present, var.str)
      else  quantities.present <- append(quantities.present, lookupQuantity(var.str, source@format@quantities))

    }

  }

  return(quantities.present)

}

########################################################
########### FireMIP Coarse PFTS ########################
########################################################

#' @format An S4 class object with the slots as defined below.
#' @keywords datasets
ORCHIDEE_FireMIP.PFTs <- list(

  # TREES

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


  BNS = new("PFT",
            id = "BNS",
            name = "Boreal Needleleaved Summergreen Tree",
            growth.form = "Tree",
            leaf.form = "Needleleaved",
            phenology = "Summergreen",
            climate.zone = "Boreal",
            colour = "cadetblue2",
            shade.tolerance = "None"
  ),

  new("PFT",
      id = "BBS",
      name = "Boreal B/leaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "Boreal",
      colour = "cyan",
      shade.tolerance = "None"
  ),

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


  new("PFT",
      id = "C3_agr",
      name = "C3 Agriculture",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "lightgoldenrod4",
      shade.tolerance = "no"
  ),

  new("PFT",
      id = "C4_agr",
      name = "C4 Agriculture",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "sienna",
      shade.tolerance = "no"
  ),

  # OTHER


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
  )

)



####################################################
########### ORCHIDEE_FireMIP FORMAT ########################
####################################################
#' ORCHIDEE-FireMIP Format objects
#'
#' @description \code{ORCHIDEE_FireMIP} - a Format for reading ORCHIDEE FireMIP model output
#'
#' @format A \code{Quantity} object is an S4 class.
#' @keywords datasets
#' @importClassesFrom DGVMTools Quantity Source Format Field PFT Period STAInfo
#' @import DGVMTools
#' @export
#'
ORCHIDEE_FireMIP<- new("Format",

                       # UNIQUE ID
                       id = "ORCHIDEE-FireMIP",

                       # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
                       determinePFTs = determinePFTs_ORCHIDEE_FireMIP,

                       # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                       availableQuantities = availableQuantities_ORCHIDEE_FireMIP,

                       # FUNCTION TO READ A FIELD
                       getField = openFireMIPOutputFile_ORCHIDEE,

                       # DEFAULT GLOBAL PFTS
                       default.pfts = ORCHIDEE_FireMIP.PFTs,

                       # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS
                       quantities = FireMIP.quantities

)
