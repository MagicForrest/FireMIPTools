############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################
#' Open a Inferno FireMIP output file
#'
#' Opens a .nc file from the Inferno FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.
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

openFireMIPOutputFile_Inferno <- function(run, quantity, sta.info, file.name, verbose = TRUE) {


  first.year = sta.info@first.year
  last.year = sta.info@last.year


  Year = Lon = Lat = Month = landmask = NULL

  # get the name of the model
  print(run@format@id)

  file.quantity.str <- quantity@id
  if(quantity@id == "landCoverFrac") file.quantity.str <- "LandCoverFrac" # stupid naming
  else if(quantity@id == "cVegpft") file.quantity.str <- "cVegPFT" # stupid naming

  # open the data file and the gridl file
  file.string.nc4 <- file.path(run@dir, paste0(run@id, "_", file.quantity.str, ".nc4"))
  if(file.exists(file.string.nc4)) this.nc <- nc_open(file.string.nc4, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- nc_open(file.path(run@dir, paste0(run@id, "_", file.quantity.str, ".nc")), readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  grid.file <- system.file("gridfiles", "CRU-NCEP-LandMask.nc", package = "FireMIPTools")
  grid.nc <-  nc_open(grid.file, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )


  # get and print the dimensions and variables present
  dims.present <- names(this.nc$dim)
  print(paste("Dimensions present: ", paste(dims.present, collapse = " ")))
  vars.present <- names(this.nc$var)
  print(paste("Variables present: ", paste(vars.present , collapse = " ")))

  # PFTs - hard coded
  this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb", "Urban", "Water", "Bare", "Ice")

  # get dimensions
  this.lat <- getDimension(this.nc, "lat", verbose)
  if(is.null(this.lat)) this.lat <- getDimension(grid.nc, "lat", verbose)
  this.lon <- getDimension(this.nc, "lon", verbose)
  if(is.null(this.lon)) this.lon <- getDimension(grid.nc, "lon", verbose)
  this.time <- getDimension(this.nc, "time", verbose)

  # get the land mask
  this.landmask <- ncvar_get(grid.nc, "lsm", start = c(1,1), count = c(-1,-1))
  dimnames(this.landmask) <- list(this.lon, this.lat)
  this.landmask.dt <- as.data.table(melt(this.landmask))
  setnames(this.landmask.dt, c("Lon", "Lat", "landmask"))
  this.landmask.dt <- this.landmask.dt[landmask > 0.05,]
  this.landmask.dt <- this.landmask.dt[landmask < 1.1,]
  this.landmask.dt[, landmask:=NULL]

  # attempt to automagically determine time axis
  is.monthly <- FALSE
  # monthly starting in 1861
  if(length(this.time) == 1836) {
    is.monthly <- TRUE
    all.years <- 1861:2013
  }
  # annual starting in 1950
  else if(length(this.time) == 64) {
    all.years <- 1950:2013
  }
  # annual starting in 1950
  else if(length(this.time) == 156) {
    all.years <- 1860:2013
  }
  # annual starting in 1950
  else if(length(this.time) == 155) {
    all.years <- 1861:2014
  }
  # monthly starting in 1950
  else if(length(this.time) == 768) {
    is.monthly <- TRUE
    all.years <- 1950:2013
  }
  # monthly starting in 1701 (ignore 11 months of 1700) -- Inferno
  else if(length(this.time) == 3767) {
    is.monthly <- TRUE
    all.years <- 1701:2013
  }
  else {
    stop(paste("Guess time axis for time dimensions length", length(this.time)))
  }



  # also determine if it is perPFT
  is.perPFT <- FALSE
  if(length(vars.present) > 3 || length(dims.present) > 3) {
    if("lev" %in% vars.present || "lev" %in% dims.present || "lev_4" %in% vars.present || "lev_4" %in% dims.present || "lev_3" %in% vars.present || "lev_3" %in% dims.present || "type" %in% vars.present || "type" %in% dims.present){
      is.perPFT <- TRUE
      if("lev" %in% vars.present || "lev" %in% dims.present) this.vegtype <- ncvar_get(this.nc,"lev",verbose=verbose)
      if("lev_4" %in% vars.present || "lev_4" %in% dims.present) this.vegtype <- ncvar_get(this.nc,"lev_4",verbose=verbose)
      if("lev_3" %in% vars.present || "lev_3" %in% dims.present) this.vegtype <- ncvar_get(this.nc,"lev_3",verbose=verbose)
      if("type" %in% vars.present || "type" %in% dims.present) this.vegtype <- ncvar_get(this.nc,"type",verbose=verbose)
      if(length(this.vegtype) == 9) this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb")
      if(length(this.vegtype) == 10) this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb", "Bare")
      else if(length(this.vegtype) == 13) this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb", "Urban", "Water", "Bare", "Ice")
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

      this.slice <- ncvar_get(this.nc, start = c(1,1,1,(year.counter*12)+1+11), count = c(-1,-1,-1, 12))
      dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, 1:12)

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Month", quantity@id))

      # select land gridcells only
      this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)

      # set NAs to zero
      #for (j in seq_len(ncol(this.slice.dt))[5:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

      # add Year dcast back to a column for every PFT
      this.slice.dt[, Year := counter]
      new.order <- c("Lon", "Lat", "Year", "Month", quantity@id)
      setcolorder(this.slice.dt, new.order)
      this.slice.dt <- dcast(this.slice.dt, Lon + Lat + Year + Month ~ PFT, value.var = quantity@id, fill = 0)
      this.slice.dt <- na.omit(this.slice.dt)

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
    # this.slice <- ncvar_get(this.nc, start = c(1,1,1,1), count = c(-1,-1,-1, -1))
    # print(dim(this.slice))

    this.slice <- ncvar_get(this.nc, start = c(1,1,1,year.start.index), count = c(-1,-1,-1,count.index))
    dimnames(this.slice) <- list(this.lon, this.lat, this.pfts, first.year:last.year)

    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))

    # set names, chuck out the water and set NAs to 0
    setnames(this.slice.dt, c("Lon", "Lat", "PFT", "Year", quantity@id))

    # select land gridcells only
    this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)

    # set NAs to zero
    #for (j in seq_len(ncol(this.slice.dt))[5:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)


    # dcast back to a column for every PFT
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
      #this.slice <- ncvar_get(this.nc, start = c(1,1,1), count = c(-1,-1,-1))
      #print(dim(this.slice))


      this.slice <-  ncvar_get(this.nc, start = c(1,1,(year.counter*12)+1+11), count = c(-1,-1,12))
      dimnames(this.slice) <- list(this.lon, this.lat, paste(1:12))

      # if necessary multiply data by a constant
      # this.slice <- (1/0.00001157407407) * this.slice

      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # set names, chuck out the water and set NAs to 0
      setnames(this.slice.dt, c("Lon", "Lat", "Month", quantity@id))
      #setcolorder(this.slice.dt, c("Lon", "Lat","Month", quantity@id))
      this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
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
    this.slice.dt <- selectGridcells(this.slice.dt, this.landmask.dt)
    #for (j in seq_len(ncol(this.slice.dt))[4:ncol(this.slice.dt)])  set(this.slice.dt,which(is.na(this.slice.dt[[j]])),j,0)

    # add it on to the full data.table
    full.dt <- rbind(full.dt, this.slice.dt)
    rm(this.slice, this.slice.dt)

    t2 <- Sys.time()
    print(t2-t1)

  } # END ANNUAL PER-PFT CASE


  # Tidy stuff
  full.dt <- stats::na.omit(full.dt)


  # convert BA per day to per month
  if(quantity@id == "burntArea") {
    layer.names <- names(full.dt)
    layer.names <- layer.names[!layer.names %in% getDimInfo(full.dt)]
    days.in.month <- c()
    for(month in all.months) {
      days.in.month <- append(days.in.month, month@days)
    }
    full.dt[, (layer.names) := .SD  * days.in.month[Month] * 24 * 60 * 60 * 100, .SDcols = layer.names]

    print("Also reading landCoverFrac for burntArea for INFERNO")
    landcover.run <- run
    landcover.run@london.centre <- FALSE
    landcover <- getField(source = landcover.run, var = "landCoverFrac", first.year = first.year, last.year = last.year)




    # make two matrices and mutiply
    #landcover
    landcover.dt <- landcover@data
    setKeyDGVM(landcover.dt)
    landcover.dt <- landcover.dt[,layer.names,with=FALSE]
    landcover.matrix <- as.matrix(landcover.dt)

    # BA
    BA.dt <- copy(full.dt)
    setKeyDGVM(BA.dt)
    BA.dt <- BA.dt[,layer.names,with=FALSE]
    BA.matrix <- as.matrix( BA.dt)

    # multiply and merge
    final.matrix <- landcover.matrix * BA.matrix
    final.dt <-  as.data.table(final.matrix)
    coords.dt <- full.dt[, c("Lon", "Lat", "Year", "Month"), with=TRUE]
    setKeyDGVM(coords.dt)
    full.dt <- cbind(coords.dt, final.dt)

  }

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
  nc_close(grid.nc)
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


availableQuantities_Inferno_FireMIP <- function(source, names){

  # First get the list of *.out files present
  files.present.nc <- list.files(source@dir, "*.nc")
  files.present.nc4 <- list.files(source@dir, "*.nc4")
  files.present <- append(files.present.nc,  files.present.nc4)

  quantities.present <- list()
  for(file in files.present) {

    # remove the.nc
    var.str <- gsub(".nc4", "", file)
    var.str <- gsub(".nc", "", var.str)

    split.thing <- unlist(strsplit(var.str, "_"))
    var.str <- split.thing[length(split.thing)]
    if(var.str == "cLitter") var.str <- NULL # mutiple layers, not sure how to handle...
    else if(var.str == "LandCoverFrac") var.str <- "landCoverFrac" # stupid naming
    else if(var.str == "cVegPFT") var.str <- "cVegpft" # stupid naming

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
Inferno_FireMIP.PFTs <- list(

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



####################################################
########### Inferno_FireMIP FORMAT ########################
####################################################
#' Inferno-FireMIP Format objects
#'
#' @description \code{Inferno_FireMIP} - a Format for reading Inferno FireMIP model output
#'
#' @format A \code{Quantity} object is an S4 class.
#' @keywords datasets
#' @importClassesFrom DGVMTools Quantity Source Format Field Layer Period STAInfo
#' @import DGVMTools
#' @include PFTs.R
#' @export
#'
Inferno_FireMIP<- new("Format",

                      # UNIQUE ID
                      id = "Inferno-FireMIP",

                      # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                      availableQuantities = availableQuantities_Inferno_FireMIP,

                      # FUNCTION TO READ A FIELD
                      getField = openFireMIPOutputFile_Inferno,

                      # DEFAULT GLOBAL PFTS
                      predefined.layers = Inferno_FireMIP.PFTs,

                      # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS
                      quantities = FireMIP.quantities

)
