############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################
#' Open a LPJ_GUESS_SIMFIRE_BLAZE FireMIP output file
#'
#' Opens a .nc file from the LPJ_GUESS_SIMFIRE_BLAZE FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.
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

openFireMIPOutputFile_LPJ_GUESS_SIMFIRE_BLAZE <- function(run, quantity, sta.info, verbose = TRUE) {


  first.year = sta.info@first.year
  last.year = sta.info@last.year


  Year = Lon = Lat = landmask = NULL

  # get the name of the model
  print(run@format@id)

  # open the data file and the gridl file
  quantity.string <- quantity@id
  if(quantity.string == "cFuel") quantity.string <- "cfuel"

  file.string <- file.path(run@dir, paste0(run@id, "_", quantity.string, ".nc"))
  print(file.string)
  this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  # grid.file <- system.file("gridfiles", "LPJ_GUESS_SIMFIRE_BLAZE_grid.nc", package = "FireMIPTools")
  # grid.nc <-  nc_open(grid.file, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )


  # get and print the dimensions and variables present
  dims.present <- names(this.nc$dim)
  print(paste("Dimensions present: ", paste(dims.present, collapse = " ")))
  vars.present <- names(this.nc$var)
  print(paste("Variables present: ", paste(vars.present , collapse = " ")))

  # PFTs - hard coded
  this.pfts <- c("C3G_pas", "C4G_pas", "BNE", "BINE", "BNS", "TeBS", "IBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "TeSW", "TeSWirr", "TeWW", "TeWWirr", "TeCo", "TeCoirr")


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
  # monthly starting in 1700 -- LPJ_GUESS_SIMFIRE_BLAZE
  else if(length(this.time) == 3768) {
    is.monthly <- TRUE
    all.years <- 1700:2013
  }
  # monthly starting in 1700 -- LPJ_GUESS_SIMFIRE_BLAZE
  else if(length(this.time) == 314) {
    all.years <- 1700:2013
  }
  else {
    stop(paste("Guess time axis for time dimensions length", length(this.time)))
  }



  # also determine if it is perPFT
  is.perPFT <- FALSE
  if(length(vars.present) > 3 || length(dims.present) > 3) {
    if("pft" %in% vars.present || "pft" %in% dims.present){
      is.perPFT <- TRUE
      this.vegtype <- ncvar_get(this.nc,"pft",verbose=verbose)
      #if(length(this.vegtype) == 9) this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS")
      #else if(length(this.vegtype) == 10) this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS", "Bare")
    }
  }


  first.year.output <- all.years[1]
  last.year.output <- all.years[length(all.years)]

  # choose range of years (if specifed, else take the whole range)
  if(is.null(first.year) || length(first.year) == 0) first.year <- first.year.output
  if(is.null(last.year) || length(last.year) == 0) last.year <- last.year.output

  # What we do now depend on how we want the output to be
  full.dt <- data.table()

  # get each year and make it into a data.table
  if(is.monthly && is.perPFT) {

    print("per month and per PFT")

    t1 <- Sys.time()

    for(counter in first.year:last.year) {

      year.counter <- counter - first.year.output

      # diagnostics
      #this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
      #print(dim(this.slice))

      this.slice <- ncvar_get(this.nc, start = c(1,1,1,(year.counter*12)+1), count = c(-1,-1,-1, 12))
      dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, 1:12)

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Month", quantity@id))

      # use land mask
      #this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)

      # set any remaining NAs to zero
      #for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

      # alternatively, remove NAs
      this.slice.dt <- na.omit(this.slice.dt)

      # add Year dcast back to a column for every PFT
      this.slice.dt[, Year := counter]
      new.order <- c("Lon", "Lat", "Year", "Month", quantity@id)
      setcolorder(this.slice.dt, new.order)
      this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year + Month ~ PFT, value.var = quantity@id, fill = 0)




      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)
      rm(this.slice, this.slice.dt)

    }
    t2 <- Sys.time()
    print(t2-t1)

  } # END ANNUAL PER-PFT CASE


  # get each year and make it into a data.table
  else if(is.perPFT) {

    print("per PFT")

    t1 <- Sys.time()

    year.start.index <- first.year - first.year.output +1
    count.index <- last.year - first.year +1

    # diagnostics
    #this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
    #print(dim(this.slice))

    this.slice <- ncvar_get(this.nc, start = c(1,1,1,year.start.index), count = c(-1,-1,-1,count.index))
    dimnames(this.slice) <- list(this.pfts, this.lon, this.lat, first.year:last.year)

    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))

    # set names, chuck out the water and set NAs to 0
    setnames(this.slice.dt, c("PFT", "Lon", "Lat", "Year", quantity@id))
    #this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
    #for (j in seq_len(ncol(this.slice.dt))[5:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
    this.slice.dt <- na.omit(this.slice.dt)

    # dcast back to a column for every PFT and set colum order
    this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year ~ PFT, value.var = quantity@id)

    # add it on to the full data.table
    full.dt <- rbind(full.dt, this.slice.dt)
    rm(this.slice, this.slice.dt)

    t2 <- Sys.time()
    print(t2-t1)

  } # END ANNUAL PER-PFT CASE


  else if(is.monthly) {

    print("per month")

    # get each year and make it into a data.table
    t1 <- Sys.time()

    for(counter in first.year:last.year) {

      # get the slice
      year.counter <- counter - first.year.output

      # diagnostics
      #this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
      #print(dim(this.slice))
      #stop()

      this.slice <-  ncvar_get(this.nc, start = c(1,1,(year.counter*12)+1), count = c(-1,-1,12))
      dimnames(this.slice) <- list(this.lon, this.lat, paste(1:12))

      # if necessary multiply data by a constant
      # this.slice <- (1/0.00001157407407) * this.slice

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Lon", "Lat", "Month", quantity@id))
      #setcolorder(this.slice.dt, c("Lon", "Lat","Month", quantity@id))
      #this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
      #for (j in seq_len(ncol(this.slice.dt))[3:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
      this.slice.dt <- na.omit(this.slice.dt)

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
  else {

    print("simple annual")

    t1 <- Sys.time()

    # diagnostics
    #this.slice <- ncvar_get(this.nc, start = c(1,1,1), count = c(-1,-1,-1))
    #print(dim(this.slice))

    year.start.index <- first.year - first.year.output +1
    count.index <- last.year - first.year +1

    this.slice <- ncvar_get(this.nc, start = c(1,1,year.start.index), count = c(-1, -1, count.index))
    dimnames(this.slice) <- list(this.lon, this.lat, first.year:last.year)

    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))

    # set names, chuck out the water and set NAs to 0
    setnames(this.slice.dt, c("Lon", "Lat",  "Year", quantity@id))
    #this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
    #for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)
    this.slice.dt <- na.omit(this.slice.dt)

    # add it on to the full data.table
    full.dt <- rbind(full.dt, this.slice.dt)
    rm(this.slice, this.slice.dt)

    t2 <- Sys.time()
    print(t2-t1)

  } # END ANNUAL PER-PFT CASE


  # Tidy stuff
  full.dt <- stats::na.omit(full.dt)

  # Correct lon and lats
  full.dt[, Lon := Lon + 0.25]
  full.dt[, Lat := Lat + 0.25]

  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(run@london.centre){ full.dt[, Lon := vapply(full.dt[,Lon], 1, FUN = LondonCentre)] }

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
  gc()


  this.Field <- new("Field",
                    id = makeFieldID(source = run, var.string = quantity@id, sta.info = sta.info),
                    source = run,
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


availableQuantities_LPJ_GUESS_SIMFIRE_BLAZE_FireMIP <- function(source, names){

  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")

  quantities.present <- list()
  for(file in files.present) {

    # remove the.nc
    var.str <- gsub(".nc", "", file)

      split.thing <- unlist(strsplit(var.str, "_"))
      var.str <- split.thing[length(split.thing)]
      if(var.str == "cfuel1") var.str <- NULL #  not standard
      else if(var.str == "cfuel2") var.str <- NULL
      else if(var.str == "cfuel") var.str <- "cFuel"
      else if(var.str == "clitter") var.str <- "cLitter"
      else if(var.str == "evap") var.str <- NULL # not standard - possibly should be evspslsoi
      else if(var.str == "intercept") var.str <- NULL # not standard - possibly should be evspslveg
      else if(var.str == "landCoverFrac") var.str <- NULL # not ignore landCoverFrac
      else if(var.str == "v2") var.str <- "landCoverFrac" # use landCoverFrac_v2
      else if(var.str == "trans") var.str <- NULL # not standard
      else if(var.str == "Cfire2") var.str <- NULL # not standard
      else if(var.str == "cLitter2") var.str <- NULL # not standard

      if(!is.null(var.str)) {
        if(names) quantities.present <- append(quantities.present, var.str)
        else  quantities.present <- append(quantities.present, lookupQuantity(var.str, source@format@quantities))

      }

  }

  return(quantities.present)

}





####################################################
########### LPJ_GUESS_SIMFIRE_BLAZE_FireMIP FORMAT ########################
####################################################
#' LPJ_GUESS_SIMFIRE_BLAZE-FireMIP Format objects
#'
#' @description \code{LPJ_GUESS_SIMFIRE_BLAZE_FireMIP} - a Format for reading LPJ_GUESS_SIMFIRE_BLAZE FireMIP model output
#'
#' @format A \code{Quantity} object is an S4 class.
#' @keywords datasets
#' @importClassesFrom DGVMTools Quantity Source Format Field Layer Period STAInfo
#' @import DGVMTools
#' @include PFTs.R
#' @export
#'
LPJ_GUESS_SIMFIRE_BLAZE_FireMIP<- new("Format",

                   # UNIQUE ID
                   id = "LPJ-GUESS-SIMFIRE-BLAZE-FireMIP",

                   # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                   availableQuantities = availableQuantities_LPJ_GUESS_SIMFIRE_BLAZE_FireMIP,

                   # FUNCTION TO READ A FIELD
                   getField = openFireMIPOutputFile_LPJ_GUESS_SIMFIRE_BLAZE,

                   # DEFAULT GLOBAL PFTS
                   defined.layers = LPJ_GUESS_PFTs,

                   # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS
                   quantities = FireMIP.quantities

)
