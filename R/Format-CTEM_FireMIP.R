############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################
#' Open a CTEM FireMIP output file
#'
#' Opens a .nc file from the CTEM FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.
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
#'
#' @keywords internal
#'
#' @return A list containaing a data.table and an STAInfo object
#'
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export

openFireMIPOutputFile_CTEM <- function(run, quantity, sta.info, verbose = TRUE) {


  first.year = sta.info@first.year
  last.year = sta.info@last.year


  Year = Lon = LandSea = NULL

  # get the name of the model
  print(run@format@id)

  # make the string and open the file
  file.string <- file.path(run@dir, paste0(run@id, "_", quantity@id, ".nc"))
  this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )




  dims.present <- names(this.nc$dim)
  print(dims.present)

  vars.present <- names(this.nc$var)
  print(vars.present)

  #print(str(this.nc))


  # PFTs - hard code
  this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS")

  # get latitude dimension
  if("lat" %in% vars.present) this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
  else if("latitude" %in% vars.present) this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
  else if("Lat" %in% vars.present) this.lat <- ncvar_get(this.nc,"Lat",verbose=verbose)
  else if("Latitude" %in% vars.present) this.lat <- ncvar_get(this.nc,"Latitude",verbose=verbose)
  else (stop(paste0("Can't find latitude dimension.  Dims are: ", dims.present, " and variables are ", vars.present, collapse = " ")))

  # get longitude dimension
  if("lon" %in% vars.present) this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
  else if("longitude" %in% vars.present) this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
  else if("Lon" %in% vars.present) this.lon <- ncvar_get(this.nc,"Lon",verbose=verbose)
  else if("Longitude" %in% vars.present) this.lon <- ncvar_get(this.nc,"Longitude",verbose=verbose)
  else (stop(paste0("Can't find longitude dimension.  Dims are: ", dims.present, " and variables are ", vars.present, collapse = " ")))

  # get time dimension
  if("time" %in% vars.present) this.time <- ncvar_get(this.nc,"time",verbose=verbose)
  else if("Time" %in% vars.present) this.lon <- ncvar_get(this.nc,"Time",verbose=verbose)
  if("time" %in% dims.present) this.time <- ncvar_get(this.nc,"time",verbose=verbose)
  else if("Time" %in% dims.present) this.lon <- ncvar_get(this.nc,"Time",verbose=verbose)
  else (stop(paste0("Can't find time dimension.  Dims are: ", dims.present, " and variables are ", vars.present, collapse = " ")))


  # get the grid file and also prepare a list of land only gridcells
  grid.file <- file.path(run@dir, "CTEM_t63_landmask.nc")
  if(file.exists(grid.file)) grid.nc <-  nc_open(grid.file, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else stop("please ensure that your run directory contains the file CTEM_t63_landmask.nc")
  this.landmask <- ncvar_get(grid.nc, "landmask", start = c(1,1,1), count = c(-1,-1,-1))
  dimnames(this.landmask) <- list(this.lon, this.lat)
  this.landmask.dt <- as.data.table(melt(this.landmask))
  setnames(this.landmask.dt, c("Lon", "Lat", "landmask"))
  this.landmask.dt <- this.landmask.dt[landmask > 0,]
  this.landmask.dt[, landmask:=NULL]

  # attempt to automagically determine time axis
  print(this.time)
  is.monthly <- FALSE
  # monthly starting in 1861 -- CTEM
  if(length(this.time) == 1836) {
    is.monthly <- TRUE
    all.years <- 1861:2013
  }
  # annual starting in 1950 -- CTEM
  else if(length(this.time) == 64) {
    all.years <- 1950:2013
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
      if(length(this.vegtype) == 9) this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS")
      else if(length(this.vegtype) == 10) this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS", "Bare")
    }
  }

  if(is.monthly && is.perPFT) dimensions.present <- "perPFT_monthly"
  else if(is.monthly) dimensions.present <- "monthly"
  else if(is.perPFT) dimensions.present <- "perPFT"
  else dimensions.present <- "annual"



  print(dimensions.present)
  first.year.output <- all.years[1]
  last.year.output <- all.years[length(all.years)]

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

      this.slice <-  ncvar_get(this.nc, start = c(1,1,(year.counter*12)+1), count = c(-1,-1,12))

      print(dim(this.slice))
      dimnames(this.slice) <- list(this.lon, this.lat, paste(1:12))

      # if necessary multiply data by a constant
      # this.slice <- (1/0.00001157407407) * this.slice

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))
      print(this.slice.dt)

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Lon", "Lat", "Month", quantity@id))
      #setcolorder(this.slice.dt, c("Lon", "Lat","Month", quantity@id))
      this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
      for (j in seq_len(ncol(this.slice.dt))[3:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
      this.slice.dt <- na.omit(this.slice.dt)


      # add a column for "Year"
      this.slice.dt[, Year := counter]

      # reorder columns so that "Year" follows after "Lon" and "Lat"
      new.order <- c("Lon", "Lat", "Year", "Month", quantity@id)
      setcolorder(this.slice.dt, new.order)

      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)

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

      # diagnostics
      #this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
      #print(dim(this.slice))
      #stop()

      this.slice <- ncvar_get(this.nc, start = c(1,1,1,(year.counter*12)+1), count = c(-1,-1,-1, 12))
      print(dim(this.slice))
      dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, 1:12)

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))
      print(this.slice.dt)

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Month", quantity@id))
      this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
      for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
      this.slice.dt <- na.omit(this.slice.dt)
      print(this.slice.dt)

      # add Year dcast back to a column for every PFT
      this.slice.dt[, Year := counter]
      new.order <- c("Lon", "Lat", "Year", "Month", quantity@id)
      setcolorder(this.slice.dt, new.order)
      this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year + Month ~ PFT, value.var = quantity@id, fill = 0)
      print(this.slice.dt)
      # add a column for "Year"


      # reorder columns so that "Year" follows after "Lon" and "Lat"


      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)

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

determinePFTs_CTEM_FireMIP <- function(x, variables) {

  warning("determinePFTs_FireMIP not currently implmented.")
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


determineQuantities_CTEM_FireMIP <- function(source, names){

  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")

  quantities.present <- list()
  for(file in files.present) {

    # remove the.nc
    var.str <- gsub(".nc", "", file)
    if(var.str != "CTEM_t63_landmask"){

      split.thing <- unlist(strsplit(var.str, "_"))
      var.str <- split.thing[length(split.thing)]

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
CTEM_FireMIP.PFTs <- list(

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



####################################################
########### CTEM_FireMIP FORMAT ########################
####################################################
#' CTEM-FireMIP Format objects
#'
#' @description \code{CTEM_FireMIP} - a Format for reading CTEM FireMIP model output
#'
#' @format A \code{Quantity} object is an S4 class.
#' @keywords datasets
#' @importClassesFrom DGVMTools Quantity Source Format Field PFT Period STAInfo
#' @import DGVMTools
#' @export
#'
CTEM_FireMIP<- new("Format",

                   # UNIQUE ID
                   id = "CTEM-FireMIP",

                   # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
                   determinePFTs = determinePFTs_CTEM_FireMIP,

                   # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                   determineQuantities = determineQuantities_CTEM_FireMIP,

                   # FUNCTION TO READ A FIELD
                   getField = openFireMIPOutputFile_CTEM,

                   # DEFAULT GLOBAL PFTS
                   default.pfts = CTEM_FireMIP.PFTs,

                   # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS
                   quantities = FireMIP.quantities

)
