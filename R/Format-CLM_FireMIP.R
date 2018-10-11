############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################
#' Open a CLM FireMIP output file
#'
#' Opens a .nc file from the CLM FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.
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

openFireMIPOutputFile_CLM <- function(run, quantity, sta.info, verbose = TRUE) {


  first.year = sta.info@first.year
  last.year = sta.info@last.year


  Year = Lon = LandSea = NULL

  # get the name of the model
  model.string <- gsub("-FireMIP", "", run@format@id)
  print(model.string)
  africa.centre <- FALSE

  # make the string and open the filex (note special cases)
  if(quantity@id == "BA") file.string <- file.path(run@dir, "BAF.nc")
  else if(quantity@id == "Cfire") file.string <- file.path(run@dir, "CFFIRE.nc")
  else file.string <- file.path(run@dir, paste0(quantity@id, ".nc"))
  this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )

  # because, frustratingly, CLM variables are often missing lon and lat, open another file
  # also used for masking out water areas which have 0 instead on NA
  grid.file <- system.file("gridfiles", "CLM-gridcell.nc", package = "FireMIPTools")
  grid.nc <-  nc_open(grid.file, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )

  # standard easy dimensions stuff
  this.pfts <- c("Bare", "TeNE", "BNE", "BNS", "TrBE", "TeBE", "TrBR", "TeBS", "BBS", "BE_Shb", "TeBS_Shb", "BBS_Shb", "C3G_arc", "C3G", "C4G", "Crop1", "Crop2")
  this.lat <- ncvar_get(grid.nc,"lat",verbose=verbose)
  this.lon <- ncvar_get(grid.nc,"lon",verbose=verbose)

  # also prepare a list of land only gridcells
  this.landmask <- ncvar_get(grid.nc, "landmask", start = c(1,1), count = c(-1,-1))
  dimnames(this.landmask) <- list(this.lat, this.lon)
  this.landmask.dt <- as.data.table(melt(this.landmask))
  setnames(this.landmask.dt, c("Lat", "Lon", "LandSea"))
  this.landmask.dt <- this.landmask.dt[LandSea > 0,]
  this.landmask.dt[, LandSea:=NULL]
  setcolorder(this.landmask.dt , c("Lon", "Lat"))


  # figure out the variable type
  # annual, per-PFT variables
  if(quantity@id == "landCoverFrac" ||
     quantity@id == "lai"           ||
     quantity@id == "theightpft") {

    dimensions.present <- "perPFT"

    if(quantity@id == "landCoverFrac") this.time <- 1850:2013
    if(quantity@id == "lai" || quantity@id == "theightpft") this.time <- 1950:2013


  }

  # monthly *and* perPFT variables
  else if(quantity@id == "burntArea" ||
          quantity@id == "cVegpft" ||
          quantity@id == "gpppft" ||
          quantity@id == "npppft" ||
          quantity@id == "fFirepft") {

    dimensions.present <- "perPFT_monthly"

    this.time <- 1850:2013
    if(quantity@id == "gpppft" ||
       quantity@id == "cVegpft" ||
       quantity@id == "npppft") {
      this.time <- 1950:2013
    }

  }

  # monthly variables
  else {
    # if(quantity@id == "BA" ||
    #       quantity@id == "gpp" ||
    #       quantity@id == "npp" ||
    #       quantity@id == "nbp") {

    dimensions.present <- "monthly"

    this.time <- 1850:2013
    if(quantity@id == "cProduct" ||
       quantity@id == "fLuc" ||
       quantity@id == "gpp" ||
       quantity@id == "npp" ||
       quantity@id == "nbp" ||
       quantity@id == "rh" ||
       quantity@id == "ra" ||
       quantity@id == "mrro" ||
       quantity@id == "evapotrans") {
      this.time <- 1950:2013
    }

  }


  print(dimensions.present)
  first.year.output <- this.time[1]
  last.year.output <- this.time[length(this.time)]

  # choose range of years (if specifed, else take the whole range)
  if(is.null(first.year) || length(first.year) == 0) first.year <- first.year.output
  if(is.null(last.year) || length(last.year) == 0) last.year <- last.year.output



  # What we do now depend on how we want the output to be


  # get each year and make it into a data.table
  if(dimensions.present == "perPFT") {
    t1 <- Sys.time()
    full.dt <- data.table()

    year.start.index <- first.year - first.year.output +1
    count.index <- last.year - first.year +1

    this.slice <- ncvar_get(this.nc, start = c(year.start.index,1,1,1), count = c(count.index,-1,-1, -1))
    dimnames(this.slice) <- list(first.year:last.year, this.pfts, this.lat, this.lon)

    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))

    # set names, chuck out the water and set NAs to 0
    setnames(this.slice.dt, c("Year", "PFT", "Lat", "Lon", quantity@id))
    this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
    for (j in seq_len(ncol(this.slice.dt))[5:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

    # dcast back to a column for every PFT
    this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year ~ PFT, value.var = quantity@id)

    # add it on to the full data.table
    full.dt <- rbind(full.dt, this.slice.dt)
    rm(this.slice, this.slice.dt)

    t2 <- Sys.time()
    print(t2-t1)

  } # END ANNUAL PER-PFT CASE


  if(dimensions.present == "monthly") {

    # get each year and make it into a data.table
    t1 <- Sys.time()
    full.dt <- data.table()
    for(counter in first.year:last.year) {

      # get the slice
      year.counter <- counter - first.year.output

      this.slice <-  ncvar_get(this.nc, start = c((year.counter*12)+1,1,1), count = c(12,-1,-1))

      dimnames(this.slice) <- list(paste(1:12), this.lat, this.lon)

      # if necessary multiply data by a constant
      # this.slice <- (1/0.00001157407407) * this.slice

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Month", "Lat", "Lon", quantity@id))
      setcolorder(this.slice.dt, c("Lon", "Lat","Month", quantity@id))
      this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
      for (j in seq_len(ncol(this.slice.dt))[3:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

      # add a column for "Year"
      this.slice.dt[, Year := counter]

      # reorder columns so that "Year" follows after "Lon" and "Lat"
      new.order <- c("Lon", "Lat", "Year", "Month", quantity@id)
      setcolorder(this.slice.dt, new.order)

      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)
      rm(this.slice, this.slice.dt)

    }
    t2 <- Sys.time()
    print(t2-t1)

  }

  # get each year and make it into a data.table
  else if(dimensions.present == "perPFT_monthly") {


    t1 <- Sys.time()
    full.dt <- data.table()
    for(counter in first.year:last.year) {

      year.counter <- counter - first.year.output
      this.slice <- ncvar_get(this.nc, start = c((year.counter*12)+1,1,1,1), count = c(12,-1,-1, -1))
      dimnames(this.slice) <- list(1:12, this.pfts, this.lat, this.lon)

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Month", "PFT", "Lat", "Lon", quantity@id))
      this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
      for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

      # dcast back to a column for every PFT
      this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Month ~ PFT, value.var = quantity@id)
      # add a column for "Year"
      this.slice.dt[, Year := counter]

      # reorder columns so that "Year" follows after "Lon" and "Lat"
      new.order <- c("Lon", "Lat", names(this.slice.dt)[length(this.slice.dt)], names(this.slice.dt)[3:(length(this.slice.dt)-1)])
      setcolorder(this.slice.dt, new.order)

      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)
      rm(this.slice, this.slice.dt)

    }
    t2 <- Sys.time()
    print(t2-t1)

  } # END ANNUAL PER-PFT CASE


  # Tidy stuff
  full.dt <- stats::na.omit(full.dt)
  print(full.dt)

  all.years <- sort(unique(full.dt[["Year"]]))
  subannual <- "Month"
  if(dimensions.present == "perPFT") subannual <- "Annual"
  sta.info = new("STAInfo",
                 first.year = min(all.years),
                 last.year = max(all.years),
                 subannual.resolution = subannual,
                 subannual.original = subannual,
                 spatial.extent = extent(full.dt))


  # close the file
  nc_close(this.nc)
  nc_close(grid.nc)
  gc()

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

determinePFTs_CLM_FireMIP <- function(x, variables) {

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


availableQuantities_CLM_FireMIP <- function(source, names){

  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")

  quantities.present <- list()
  for(file in files.present) {


    # CLM
    if(source@format@id == "CLM-FireMIP") {

      # remove the.nc
      var.str <- gsub(".nc", "", file)
      if(var.str == "BAF") var.str <- "BA"
      else if (var.str == "CFFIRE") var.str <- "Cfire"
      else if (var.str == "cLittert") var.str <- NULL # not consistent with other models
      else if (var.str == "cSoilt") var.str <- NULL # not consistent with other models
      else if (var.str == "peatfc") var.str <- NULL # not standard FireMIP
      else if (var.str == "convfc") var.str <- NULL # not standard FireMIP
      else if (var.str == "mrsol") var.str <- NULL # not standard FireMIP - too many layers
      else if (var.str == "CLM-gridcell") var.str <- NULL # not a quantity
      print(var.str)

      if(!is.null(var.str)) {
        print(lookupQuantity(var.str, source@format@quantities))
        if(names) quantities.present <- append(quantities.present, var.str)
        else   quantities.present <- append(quantities.present, lookupQuantity(var.str, source@format@quantities))

      }

    }



  }

  return(quantities.present)

}

########################################################
########### FireMIP Coarse PFTS ########################
########################################################

#' @format An S4 class object with the slots as defined below.
#' @keywords datasets
CLM_FireMIP.PFTs <- list(




  # BOREAL TREES
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


  # BNS
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

  # BBS
  new("PFT",
      id = "BBS",
      name = "Shade-intolerant B/leaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "Boreal",
      colour = "dodgerblue3",
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

  # C3G_arc
  new("PFT",
      id = "C3G_arc",
      name = "Arctic Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "Arctic",
      colour = "plum",
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


  # BE_Shb
  new("PFT",
      id = "BE_Shb",
      name = "Broadleaved Evergreen Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "Evergreen",
      climate.zone = "NA",
      colour = "darkred",
      shade.tolerance = "no"
  ),

  # TeBS_Shb
  new("PFT",
      id = "TeBS_Shb",
      name = "Temperate Summergreen Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "Deciduous",
      climate.zone = "NA",
      colour = "palevioletred1",
      shade.tolerance = "no"
  ),

  # BBS_Shb
  new("PFT",
      id = "BBS_Shb",
      name = "BorealSummergreen Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "Deciduous",
      climate.zone = "NA",
      colour = "salmon",
      shade.tolerance = "no"
  ),

  # TeBS_Shb
  new("PFT",
      id = "TeBS_Shb",
      name = "Temperate Summergreen Shrub",
      growth.form = "Shrub",
      leaf.form = "NA",
      phenology = "Deciduous",
      climate.zone = "NA",
      colour = "palevioletred1",
      shade.tolerance = "no"
  ),

  # Crop1
  new("PFT",
      id = "Crop1",
      name = "Crop 1",
      growth.form = "Crop",
      leaf.form = "NA",
      phenology = "Crop",
      climate.zone = "NA",
      colour = "palegreen",
      shade.tolerance = "no"
  ),

  # Crop2
  new("PFT",
      id = "Crop2",
      name = "Crop 2",
      growth.form = "Crop",
      leaf.form = "NA",
      phenology = "Crop",
      climate.zone = "NA",
      colour = "palegoldenrod",
      shade.tolerance = "no"
  )

)



####################################################
########### CLM_FireMIP FORMAT ########################
####################################################
#' CLM-FireMIP Format objects
#'
#' @description \code{CLM_FireMIP} - a Format for reading CLM FireMIP model output
#'
#' @format A \code{Quantity} object is an S4 class.
#' @keywords datasets
#' @importClassesFrom DGVMTools Quantity Source Format Field PFT Period STAInfo
#' @import DGVMTools
#' @export
#'
CLM_FireMIP<- new("Format",

              # UNIQUE ID
              id = "CLM-FireMIP",

              # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
              determinePFTs = determinePFTs_CLM_FireMIP,

              # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
              availableQuantities = availableQuantities_CLM_FireMIP,

              # FUNCTION TO READ A FIELD
              getField = openFireMIPOutputFile_CLM,

              # DEFAULT GLOBAL PFTS
              default.pfts = CLM_FireMIP.PFTs,

              # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS
              quantities = FireMIP.quantities

)
