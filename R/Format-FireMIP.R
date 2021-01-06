############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################
#' Open a FireMIP output file
#'
#' Opens a .nc file from the FireMIP 2019 output and sorts out the meta-data and dimensions and all that messy stuff.
#' Returns a data.table, because it is intended to be called by getField(), but of course the data.table could be used directly if you wish
#'
#'
#' @param source A Source object to define the run we want to open.
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

openFireMIPOutputFile <- function(source, quantity, sta.info, file.name, verbose = TRUE, model, grid.file, debug = FALSE) {


  first.year = sta.info@first.year
  last.year = sta.info@last.year


  Year = Lon = Lat = Month = landmask = NULL

  ### DETERMINE WHICH FIREMIP MODEL WE ARE DEALING WITH
  if(missing(model) || is.null(model)) {
    stop('Please specify with FireMIP model you are using when calling getField, with model = "CLASS-CTEM"/"LPJmL4.0"/"JULES"/"LPJ-GUESS-SPITFIRE"')
  }


  ###  OPEN THE DATA FILE
  # if no file.name provided
  if(missing(file.name) || is.null(file.name)) {
    # generate it automatically
    file.name <- file.path(source@dir, paste0(source@id, "_", quantity@id, ".nc"))
    # check exists, if not fail
    if(!file.exists(file.name)) {
      stop(paste0("Couldn't find file ", file.name, " (automatically generated)."))
    }
  }
  # next check for the file in the source directory
  else if(file.exists(file.path(source@dir, file.name))) {
    file.name <- file.path(source@dir, file.name)
  }
  # else check for the full file
  else if(file.exists(file.name)) {
    file.name <- file.name
  }
  # else fail!
  else {
    stop(paste0("Couldn't find file ", file.name, " (or in directory ", source@dir, ")"))
  }
  this.nc <- nc_open(file.name, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )


  # open the grid file if it has been provided
  if(!missing(grid.file) && !is.null(grid.file)) {
    grid.nc <- nc_open(grid.file, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  }


  # get and print the dimensions and variables present
  dims.present <- names(this.nc$dim)
  if(debug) print(paste("Dimensions present: ", paste(dims.present, collapse = " ")))
  vars.present <- names(this.nc$var)
  if(debug) print(paste("Variables present: ", paste(vars.present , collapse = " ")))

  # get dimensions
  this.lat <- getDimension(this.nc, "lat", verbose)
  if(debug) print(this.lat)
  if(is.null(this.lat) && exists("grid.nc")) this.lat <- getDimension(grid.nc, "lat", verbose)
  this.lon <- getDimension(this.nc, "lon", verbose)
  if(debug) print(this.lon)
  if(is.null(this.lon) && exists("grid.nc")) this.lon <- getDimension(grid.nc, "lon", verbose)
  this.time <- getDimension(this.nc, "time", verbose)
  if(debug) print(this.time)



  # get the land mask
  if(exists("grid.nc")) {
    this.landmask <- ncvar_get(grid.nc, "landmask", start = c(1,1,1), count = c(-1,-1,-1))
    grid.lons <- getDimension(grid.nc, "lon", verbose)
    grid.lats <- getDimension(grid.nc, "lat", verbose)
    if(model == "CLASS-CTEM") grid.lats <- this.lat
    dimnames(this.landmask) <- list(grid.lons, grid.lats)
    this.landmask.dt <- as.data.table(melt(this.landmask))
    setnames(this.landmask.dt, c("Lon", "Lat", "landmask"))
    this.landmask.dt <- this.landmask.dt[landmask > 0,]
    this.landmask.dt[, landmask:=NULL]
    if(min(unique(this.lon)) < 0 ) {
      this.landmask.dt[, Lon := LondonCentre(Lon)]
    }
  }


  # attempt to automagically determine time axis
  is.monthly <- FALSE
  # nothing automagic here in fact
  if(length(this.time) == 3828) {
    is.monthly <- TRUE
    all.years <- 1700:2018
  }
  else if(length(this.time) == 319) {
    all.years <- 1700:2018
  }
  # for LGM runs
  else if(length(this.time) == 1080) {
    is.monthly <- TRUE
    all.years <- 1:90
  }
  else if(length(this.time) == 90) {
    all.years <- 1:90
  }
  else {
    stop(paste("Guess time axis for time dimensions length", length(this.time)))
  }



  # also determine if it is perPFT
  is.perPFT <- FALSE
  if(length(vars.present) > 3 || length(dims.present) > 3) {

    for(pft.axis.name in c("vegtype", "PFT", "pft", "npft")){

      if(pft.axis.name %in% vars.present || pft.axis.name %in% dims.present) {
        is.perPFT <- TRUE
        # LPJmL4.0 does not have npfts defined properly as a dimension
        #if(model == "LPJmL4.0") {
        #  this.vegtype <- 1:12
        #}
        #else {
        this.vegtype <- ncvar_get(this.nc, pft.axis.name, verbose=verbose)
        #}

        if(verbose) print(paste0("Got PFT axis " , pft.axis.name))

        if(model == "CLASS-CTEM") {
          if(length(this.vegtype) == 9) this.pfts <- c("NE", "ND", "BE", "BS", "BR", "C3Crop", "C4Crop", "C3G", "C4G")
          else if(length(this.vegtype) == 10) this.pfts <- c("NE", "ND", "BE", "BS", "BR", "C3Crop", "C4Crop", "C3G", "C4G", "Bare")
        }
        else if(model == "JULES") {
          if(length(this.vegtype) == 13) this.pfts <- c("BD", "TrBE", "TeBE", "ND", "NE", "C3G", "C3Crop", "C3Pasture",  "C4G", "C4Crop", "C4Pasture", "De_Shb", "Ev_Shb")
          else if(length(this.vegtype) == 17) this.pfts <- c("BD", "TrBE", "TeBE", "ND", "NE", "C3G", "C3Crop", "C3Pasture",  "C4G", "C4Crop", "C4Pasture", "De_Shb", "Ev_Shb", "Urban", "Lake", "Bare", "Ice" )
        }
        else if(model == "LPJ-GUESS-SPITFIRE") {
          if(length(this.vegtype) == 12) this.pfts <- c("BNE", "BINE", "BNS", "TeNE", "TeBS", "IBS", "TeBE", "TrBE",  "TrIBE", "TrBR", "C3G", "C4G")
          else stop("Unexpected length of vegtype dimension in FireMIP (LPJ-GUESS-SPITFRE) file.")
        }
        else if(model == "LPJmL4.0") {
          print("LPJmL4.0")
          print(this.vegtype)
          if(length(this.vegtype) == 12) this.pfts <- c("NaturalStandFraction", "TrBE", "TrBR", "TeNE", "TeBE", "TeBS", "BNE", "BBS",  "BNS", "C4G", "C3G", "C3Polar")
          if(length(this.vegtype) == 32) this.pfts <- c("temperate cereals",
                                                        "rice",
                                                        "maize",
                                                        "tropical cereals",
                                                        "pulses",
                                                        "temperate roots",
                                                        "tropical roots",
                                                        "oil crops sunflower",
                                                        "oil crops soybean",
                                                        "oil crops groundnut",
                                                        "oil crops rapeseed",
                                                        "sugarcane",
                                                        "others",
                                                        "pasture",
                                                        "biomass grass",
                                                        "biomass tree",
                                                        "irrigated temperate cereals",
                                                        "irrigated rice",
                                                        "irrigated maize",
                                                        "irrigated tropical cereals",
                                                        "irrigated pulses",
                                                        "irrigated temperate roots",
                                                        "irrigated tropical roots",
                                                        "irrigated oil crops sunflower",
                                                        "irrigated oil crops soybean",
                                                        "irrigated oil crops groundnut",
                                                        "irrigated oil crops rapeseed",
                                                        "irrigated sugarcane",
                                                        "irrigated others",
                                                        "irrigated pasture",
                                                        "irrigated biomass grass",
                                                        "irrigated biomass tree")
        }
        break()
      }

    }

  }


  first.year.output <- all.years[1]
  last.year.output <- all.years[length(all.years)]

  # choose range of years (if specifed, else take the whole range)
  if(is.null(first.year) || length(first.year) == 0) first.year <- first.year.output
  if(is.null(last.year) || length(last.year) == 0) last.year <- last.year.output

  ### Initiliase the 'control' for the reading code below

  # first do longitude and latitude names, counters etc
  start.vec <- c(1,1)
  counter.vec <- c(-1,-1)
  dimnames.list <- list(this.lon, this.lat)
  colnames.vec <-  c("Lon", "Lat")
  final.colnames.vec <- c("Lon", "Lat", "Year")

  # if is.perPFT add stuff to read the perPFT axis
  if(is.perPFT) {
    start.vec <- append(start.vec, 1)
    counter.vec <- append(counter.vec, -1)
    dimnames.list[[length(dimnames.list)+1]] <- this.pfts
    colnames.vec <- append(colnames.vec, "PFT")
    final.colnames.vec <- append(final.colnames.vec, "PFT")
  }

  # if monthly
  if(is.monthly) {
    # NOTE: start.vec and counter.vec are done in the loop below
    dimnames.list[[length(dimnames.list)+1]] <- paste(1:12)
    colnames.vec <- append(colnames.vec, "Month")
    final.colnames.vec <- append(final.colnames.vec, "Month")
  }

  # and the Quantity id
  colnames.vec <- append(colnames.vec, quantity@id)
  final.colnames.vec <- append(final.colnames.vec, quantity@id)

  if(debug) {
    print(start.vec)
    print(counter.vec)
    print(dimnames.list)
    print(colnames.vec)
    print(final.colnames.vec)
  }

  t1 <- Sys.time()

  # the final data.table
  full.dt <- data.table()

  # read slices year by year to minimise memory footprint
  # *but* this is slower than reading all the data at once (+200%), so some optimising could definitely be done here
  all.years.dts.list <- list()
  for(counter in first.year:last.year) {

    if(verbose || debug) print(paste0("Reading year ", counter))

    # year index (starts from 0 by design)
    year.index <- counter - first.year.output

    # if monthly we also need to get 12 monthly slices
    if(is.monthly) {
      this.start.vec <- append(start.vec, (year.index*12)+1)
      this.count.vec <- append(counter.vec, 12)
    }
    # else just take one
    else {
      this.start.vec <- append(start.vec, year.index+1)
      this.count.vec <- append(counter.vec, 1)
    }

    # read the data
    this.slice <- ncvar_get(this.nc, start = this.start.vec, count = this.count.vec)

    # set the names (this will be melted into the dimensiom columns on the next step)
    dimnames(this.slice) <- dimnames.list

    # melt to a data.table (via data.frame) set names and omit NAs
    this.slice.dt <- as.data.table(melt(this.slice))
    setnames(this.slice.dt, colnames.vec)
    this.slice.dt <- na.omit(this.slice.dt)

    # use land mask - might not be necessary
    if(exists("this.landmask.dt")) this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)

    #  OLD CODE!! - MIGHT BE NECESSARY FOR SOME MODELS?
    # set any remaining NAs to zero, not sure any more why this should be necessary?
    #start.column.index <- length(getDimInfo(this.slice.dt)) + 1 # start after the dimension columns
    #if(is.perPFT) start.column.index <- start.column.index + 1 # also after the PFT column if present
    #for (j in seq_len(ncol(this.slice.dt))[start.column.index:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

    # add Year and re-order columns
    this.slice.dt[, Year := counter]
    setcolorder(this.slice.dt, final.colnames.vec)

    # if perPFT dcast back to a column for every PFT if necessary
    if(is.perPFT){
      if(is.monthly) this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year + Month ~ PFT, value.var = quantity@id, fill = 0)
      else this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year ~ PFT, value.var = quantity@id, fill = 0)
    }

    all.years.dts.list[[length(all.years.dts.list)+1]] <- this.slice.dt
    rm(this.slice, this.slice.dt)
    gc()

  }

  # add it on to the full data.table
  full.dt <- rbindlist(all.years.dts.list)
  rm(all.years.dts.list)
  gc()



  ### MF - commented code below is legacy code that is faster than the general case above,
  ###      I believe this is because it reads everything in one go

  # if(is.perPFT) {
  #
  #   print("per PFT")
  #
  #   year.start.index <- first.year - first.year.output +1
  #   count.index <- last.year - first.year +1
  #
  #   # diagnostics
  #   # this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
  #   # print(dim(this.slice))
  #
  #   this.slice <- ncvar_get(this.nc, start = c(1,1,1,year.start.index), count = c(-1,-1,-1,count.index))
  #   dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, first.year:last.year)
  #
  #   # melt to a data.table, via data.frame
  #   this.slice.dt <- as.data.table(melt(this.slice))
  #   this.slice.dt <- stats::na.omit(this.slice.dt)
  #
  #   # set names, chuck out the water and set NAs to 0
  #   setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Year", quantity@id))
  #   if(exists("this.landmask.dt")) this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
  #   #for (j in seq_len(ncol(this.slice.dt))[5:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
  #
  #   # dcast back to a column for every PFT
  #   this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year ~ PFT, value.var = quantity@id)
  #
  #   # add it on to the full data.table
  #   full.dt <- rbind(full.dt, this.slice.dt)
  #   rm(this.slice, this.slice.dt)
  #
  #
  # } # END ANNUAL PER-PFT CASE
  #
  #
  # # simple single annual value case
  # else {
  #
  #   print("simple annual")
  #
  #   # diagnostics
  #   #this.slice <- ncvar_get(this.nc, start = c(1,1,1), count = c(-1,-1,-1))
  #   #print(dim(this.slice))
  #
  #   year.start.index <- first.year - first.year.output +1
  #   count.index <- last.year - first.year +1
  #
  #   this.slice <- ncvar_get(this.nc, start = c(1,1,year.start.index), count = c(-1, -1, count.index))
  #   dimnames(this.slice) <- list(this.lon, this.lat, first.year:last.year)
  #
  #   # melt to a data.table, via data.frame
  #   this.slice.dt <- as.data.table(melt(this.slice))
  #   this.slice.dt <- stats::na.omit(this.slice.dt)
  #
  #   # set names, chuck out the water and set NAs to 0
  #   setnames(this.slice.dt, c("Lon", "Lat",  "Year", quantity@id))
  #   if(exists("this.landmask.dt")) this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
  #   #for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
  #
  #   # add it on to the full data.table
  #   full.dt <- rbind(full.dt, this.slice.dt)
  #   rm(this.slice, this.slice.dt)
  #
  # } # END SIMPLE ANNUAL CASE


  t2 <- Sys.time()
  if(debug || verbose) {
    print("Read netCDF file, time taken:")
    print(t2-t1)
  }

  # set the keys
  t1 <- Sys.time()
  setKeyDGVM(full.dt)
  t2 <- Sys.time()
  if(debug || verbose) {
    print("Keys set, time taken:")
    print(t2-t1)
  }

  # if london.centre is requested, make sure all longitudes greater than 180 are shifted to negative
  if(source@london.centre){
    unique.lons <- sort(unique(full.dt[["Lon"]]))
    if(unique.lons[length(unique.lons)] > 180) {
      t1 <- Sys.time()
      full.dt[, Lon := LondonCentre(Lon)]
      t2 <- Sys.time()
      if(debug) {
        print("London centred, time taken:")
        print(t2-t1)
      }
    }
  }

  all.years <- sort(unique(full.dt[["Year"]]))
  if(is.monthly) subannual <- "Month"
  else subannual <- "Annual"
  sta.info = new("STAInfo",
                 first.year = min(all.years),
                 last.year = max(all.years),
                 subannual.resolution = subannual,
                 subannual.original = subannual,
                 spatial.extent = extent(full.dt),
                 spatial.extent.id = "Full")


  # close the file
  nc_close(this.nc)
  if(exists("grid.nc")) nc_close(grid.nc)
  gc()


  this.Field <- new("Field",
                    id = makeFieldID(source = source, var.string = quantity@id, sta.info = sta.info),
                    source = source,
                    quant = quantity,
                    data = full.dt,
                    sta.info)

  return(this.Field)


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


availableQuantities_FireMIP <- function(source, names){

  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")

  quantities.present <- list()
  for(file in files.present) {

    # remove the.nc
    var.str <- gsub(".nc", "", file)
    if(var.str != "CTEM_t63_landmask"){

      split.thing <- unlist(strsplit(var.str, "_"))
      var.str <- split.thing[length(split.thing)]

      if(var.str == "mrso") var.str <- NULL # mutiple layers, not sure how to handle...
      else if(var.str == "tsl") var.str <- NULL # mutiple layers, not sure how to handle...
      else if(var.str == "msl") var.str <- NULL # mutiple layers, not sure how to handle...
      else if(var.str == "mslpft") var.str <- NULL # mutiple layers, not sure how to handle...
      else if(var.str == "oceanCoverFrac") var.str <- NULL # mutiple layers, not sure how to handle...

      if(!is.null(var.str)) {
        if(names) quantities.present <- append(quantities.present, var.str)
        else  quantities.present <- append(quantities.present, lookupQuantity(var.str, source@format@quantities))

      }
    }

  }

  return(quantities.present)

}


####################################################
############ FireMIP FORMAT ########################
####################################################
#' FireMIP Format object
#'
#' @description \code{FireMIP} - a Format for reading 2019 FireMIP model output
#'
#' @format A \code{Format} object is an S4 class.
#' @keywords datasets
#' @importClassesFrom DGVMTools Quantity Source Format Field Layer Period STAInfo
#' @import DGVMTools
#' @include FireMIP-PFTs.R
#' @export
#'
FireMIP <- new("Format",

               # UNIQUE ID
               id = "FireMIP",

               # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
               availableQuantities = availableQuantities_FireMIP,

               # FUNCTION TO READ A FIELD
               getField = openFireMIPOutputFile,

               # DEFAULT GLOBAL PFTS
               predefined.layers = FireMIP.PFTs,

               # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS
               quantities = FireMIP.quantities

)
