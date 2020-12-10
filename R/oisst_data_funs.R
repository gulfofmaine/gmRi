####  Sea Surface Extractor Functions for Optimum Interpolation Sea Surface Temperature (OISST)  ####
####  About: https://www.ncdc.noaa.gov/oisst




####______________________________####
####  Rotate Data  ####


#' @title Longitude 180 to 360
#'
#' @description This is a simple function to translate negative longitudes
#' (measured on -180:180 scale) into 0-360, which is coordinate system used by some environmental datasets.
#'
#' Not a solution for when data is erroneously missing a negative sign, when the format is -180 to 180.
#'
#' @param lon Longitude in -180:180 degrees
#'
#' @return lon Longitude from 0-360 degrees
#' @export
#'
#'
make360 <- function(lon) {

  ind <- which(lon < 0)
  lon[ind] <- lon[ind] + 360
  return(lon)
}


####______________________________####
####  Fix Raster from Array  ####

#' @title Fix Raster
#'
#' @description Helper function to convert an array (x, y, z)
#' into raster layers with the correct dimensions and orientation for spatial analyses. Typically used when working from netcdf to raster.
#'
#' @param x  matrix. As currently implemented, x is an individiaul time slice from an x, y, z array.
#' @param lons.use Longitudes, extracted using ncvar_get
#' @param lats.use Latitudes, extracted using ncvar_get
#' @param x.min.use Bounding box x minimum
#' @param x.max.use Bounding box x maximum
#' @param y.min.use Bounding box y minimum
#' @param y.max.use Bounding box y maximum
#'
#'
#' @return rast.out Correctly trimmed and oriented raster object
#' @export
#'
#'
fix_raster <- function(x, lons.use, lats.use, x.min.use, x.max.use, y.min.use, y.max.use) {

  r.temp<- t(x)[ncol(x):1,]
  rast.out<- raster::raster(r.temp, xmn = lons.use[x.min.use], xmx = lons.use[x.max.use], ymn = lats.use[y.min.use], ymx = lats.use[y.max.use])
  return(rast.out)

  ## End function
}



####______________________________####
#### Fix Raster Stack Names  ####


#' @title Make Valid Raster Stack Dates
#'
#' @details Takes a dataframe object containing columns for the year, month, and day
#' and creates a new "valid_dates" column that will match the naming convention for
#' a raster stack object, i.e. names(raster_stack) which cannot start with a number
#' or contain spaces or underscores. Month and day values are left-padded with zeros
#' to maintain consistent string length.
#'
#' @param point_location_df Dataframe of point locations we wish to use to extract data from raster stack
#' @param year_col name of the column that contains year information (unquoted text)
#' @param month_col name of the column that contains the month information (unquoted text)
#' @param day_col name of the column that contains the day information (unquoted text)
#'
#' @return df_out Original dataframe with added "valid_dates" column
#' @export
#'
#'
make_stack_dates <- function(point_location_df, year_col, month_col, day_col) {

  #tidy quotation
  year_col  <- rlang::enquo(year_col)
  month_col <- rlang::enquo(month_col)
  day_col   <- rlang::enquo(day_col)

  #Create valid_dates
  df_out <-  dplyr::mutate(point_location_df,
                           valid_dates = str_c(
                             "X",         #Must not begin with a number
                             !!year_col,  #followed by year
                             ".",         #spaces become periods
                             str_pad(!!month_col, width = 2, side = "left", pad = "0"), #month is padded
                             ".",         #connected by another period
                             str_pad(!!day_col, width = 2, side = "left", pad = "0"))   #day is also padded
  )


  return(df_out)
}






####______________________________####
#### Download OISST from THREDDS  ####

#' @title Download OISST from Thredds, convert to Stack
#'
#' @description This function loops over daily OISST files online at [earth systems research lab](https://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/noaa.oisst.v2.highres/),
#' then downloads and returns a single raster stack.
#'
#' Installs "ncdf4" and "raster" packages if not installed.
#'
#' @param box bounding box object used to clip netcdf data
#' @param times.window two element vector of desired start and end dates. Must be able to convert to date using as.Date
#'
#'
#' @return rast.temp Raster stack of OI Sea Surface Temperatures
#' @export
#'
#'
oisst_list_to_stack <- function(box, times.window) {

  ## Start function
  # Install libraries
  libraries <- c("ncdf4", "raster")
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      utils::install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })



  # Set arguments for debugging -- this will NOT run when you call the function.
  # Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    x = oisst.files
  }


  # OISST data stem
  data.stem <- "https://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/noaa.oisst.v2.highres/"

  # Connecting and extracting lat/lon/time variables from netcdf file
  my.nc   <- ncdf4::nc_open(paste(data.stem, x, sep = ""))
  lats    <- ncdf4::ncvar_get(my.nc, var = "lat")
  lons    <- ncdf4::ncvar_get(my.nc, var = "lon")
  times   <- ncdf4::ncvar_get(my.nc, var = "time")

  # Make times a little bit easier to handle
  dates.full <- as.Date(times, origin ='1800-01-01', tz = "GMT")

  # Find indices and windows corresponding to spatial box of interest,
  # which are then used in the "start" and "count" arguments to the ncvar_get call
  # for the sst variable
  b.box    <- c(make360(box[1]), make360(box[2]), box[3], box[4])
  x.window <- which(lons > b.box[1] & lons < b.box[2])
  x.min    <- min(x.window)
  x.max    <- max(x.window)
  x.count  <- ifelse(x.max-x.min > 0, x.max-x.min, 1)

  y.window <- which(lats > b.box[3] & lats < b.box[4])
  y.min    <- min(y.window)
  y.max    <- max(y.window)
  y.count  <- ifelse(y.max - y.min > 0, y.max - y.min, 1)


  if(!is.null(times)) {
    times.window <- which(dates.full > as.Date(times.window[1]) & dates.full < as.Date(times.window[2]))
  } else {
    times.window <- times
  }

  time.min  <- which.min(times.window)
  time.max  <- which.max(times.window)
  time.count <- ifelse(time.max - time.min > 0, time.max - time.min, 1)

  # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call
  dim.order <- sapply(my.nc$var$sst$dim, function(x) x$name)

  # Set start and counts
  start.use<- c("lon" = x.min, "lat" = y.min, "time" = time.min)
  count.use<- c("lon" = x.count, "lat" = y.count, "time" = time.count)

  # Run ncvar_get, adjusting order of start and count as needed
  temp <- ncdf4::ncvar_get(my.nc, varid = "sst", start = start.use[dim.order], count = count.use[dim.order])

  # Moving from the array format of temp to a raster stack
  temp.list <- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
  rast.temp <- suppressWarnings(raster::rotate(raster::stack(temp.list)))

  #Output Stack
  return(rast.temp)

  ## End function
}



####______________________________####
####  Download OISST/MUR/ERSST from THREDDS  ####

#' @title Satellite Data Extraction Function
#'
#' @description This function accesses webhosted satellite data and then downloads a subset of the data based
#' on dates and long/lat bounding box. After downloading the data, the function processes it and saves it as
#' one raster stack file. Installs "ncdf4" and "raster" packages if not installed.
#'
#' This function is helpful for getting fresh downloads from thredds. In most cases it is faster to access the data that is stored in shared
#' resource locations.
#'
#' @param data.set Env data to extract (options = ERSST, OISST, MURSST)
#' @param dates If !NULL, subset full time series to specific dates. Dates should be specified as dates = c("YYYY-MM-DD", "YYYY-MM-DD")
#' @param box  If !NULL, crop rasters to sepcific box faster downloading and processing. Box should be specified as box = c(xmin, xmax, ymin, ymax).
#' @param out.dir Directory to store resulting raster stack. Note, this will overwrite any rasters with the existing name.
#' @param mask optional mask to trim data with
#'
#' @return stack.out Stack of daily OISST files and also saves the raster stack as a .grd file in the output directory specified by out.dir
#' @export
#'
#'
env_data_extract <- function(data.set = "OISST",
                             dates = NULL,
                             box = c(-77, -60, 35, 46),
                             out.dir = here::here("data", "OISST_thredd_test"),
                             mask = NULL) {


  ## Start function
  # Install libraries
  libraries <- c("ncdf4", "raster")
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      utils::install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })

  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    data.set <- "OISST"
    dates    <- c("2001-01-01", "2008-01-01")
    box      <- c(-77, -60, 34, 46)
    out.dir  <- out.dir
  }

  # Spatial projection infomation
  proj.wgs84 <- sp::CRS("+init=epsg:4326") #WGS84 projection for all rasters

  ####  MURSST Data  ####
  # Access data from the web depending on "data.set"
  # MURSST-- Download of full timeseries takes ~ 20 minutes.
  if(data.set == "MURSST"){

    # Set path to THREDDS server
    data.path<- "http://thredds.jpl.nasa.gov/thredds/dodsC/OceanTemperature/MUR-JPL-L4-GLOB-v4.1.nc"

    # Some steps to make things cooperate with nc functions. MURSST is already in -180 to 180, so we are good there..
    b.box<- c(box[1], box[2], box[3], box[4])

    # Connecting and extracting lat/lon/time variables from netcdf file
    my.nc <- ncdf4::nc_open(data.path)
    lats  <- ncdf4::ncvar_get(my.nc, var = "lat")
    lons  <- ncdf4::ncvar_get(my.nc, var = "lon")
    times <- ncdf4::ncvar_get(my.nc, var = "time")

    # Make times a little bit easier to handle
    dates.full <- as.Date(as.POSIXct(times, origin="1981-01-01", tz = "GMT"), "GMT", "%Y-%m-%d")

    x.window <- which(lons > b.box[1] & lons < b.box[2])
    x.min    <- min(x.window)
    x.max    <- max(x.window)
    x.count  <- ifelse(x.max - x.min > 0, x.max-x.min, 1)

    y.window <- which(lats > b.box[3] & lats < b.box[4])
    y.min    <- min(y.window)
    y.max    <- max(y.window)
    y.count  <- ifelse(y.max - y.min > 0, y.max - y.min, 1)

    if(!is.null(dates)) {
      times.window<- which(dates.full > as.Date(dates[1]) & dates.full < as.Date(dates[2]))
    } else {
      times.window<- dates.full
    }

    time.min <- which.min(times.window)
    time.max <- which.max(times.window)

    # MURSST files are taking a while to get, so going to try to split up the processing...
    splits <- 569
    breaks <- seq(1, length(times), splits)[-1]

    for(i in seq_along(breaks)){

      # Find indices and windows corresponding to spatial box of interest, which are then used in the "start" and "count" arguments to the ncvar_get call for the sst variable
      if(i == 1){
        time.count<- seq(from = 1, to = breaks[i], by = 1)
        dates.use<- dates.full[1:breaks[i]]
      } else {
        time.count<- seq(from = breaks[i-1]+1, to = breaks[i], by = 1)
        dates.use<- dates.full[(breaks[i-1]+1):breaks[i]]
      }

      # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call
      dim.order <- sapply(my.nc$var$analysed_sst$dim, function(x) x$name)

      # Set start and counts
      start.use <- c("lon" = x.min, "lat" = y.min, "time" = time.min)
      count.use <- c("lon" = x.count, "lat" = y.count, "time" = length(time.count))

      # Run ncvar_get, adjusting order of start and count as needed
      temp <- ncvar_get(my.nc, varid = "analysed_sst", start = start.use[dim.order], count = count.use[dim.order])

      # Moving from the array format of temp to a raster stack
      temp.list        <- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
      rast.temp        <- raster::stack(temp.list)
      rast.temp        <- raster::calc(rast.temp, fun = function(x) x - 273.15)
      names(rast.temp) <- dates.use

      # Update min time
      time.min <- breaks[i] + 1

      # Write out chunk
      sp::proj4string(rast.temp) <- proj.wgs84
      raster::writeRaster(rast.temp, filename = paste(out.dir, "/", data.set, ".chunk", i, ".grd", sep = ""), overwrite = TRUE)

      print(paste("MURSST chunk ", i, " out of ", length(breaks), " is done!"))
    }
  }

  ####  ERSST Data  ####
  if(data.set == "ERSST") {
    # Set data path
    data.path <- "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/noaa.ersst/sst.mnmean.v4.nc"

    # Some steps to make things cooperate with nc functions, including getting variables from -180:180 to 0-360
    b.box <- c(make360(box[1]), make360(box[2]), box[3], box[4])

    # Connecting and extracting lat/lon/time variables from netcdf file
    my.nc <- ncdf4::nc_open(data.path)
    lats  <- ncdf4::ncvar_get(my.nc, var = "lat")
    lons  <- ncdf4::ncvar_get(my.nc, var = "lon")
    times <- ncdf4::ncvar_get(my.nc, var = "time")

    # Make times a little bit easier to handle
    dates.full <- as.Date(times, origin='1800-01-01', tz= "GMT")

    # Find indices and windows corresponding to spatial box of interest, which are then used in the "start" and "count" arguments to the ncvar_get call for the sst variable
    x.window <- which(lons > b.box[1] & lons < b.box[2])
    x.min    <- min(x.window)
    x.max    <- max(x.window)
    x.count  <- ifelse(x.max - x.min > 0, x.max-x.min, 1)

    y.window <- which(lats > b.box[3] & lats < b.box[4])
    y.min    <- min(y.window)
    y.max    <- max(y.window)
    y.count  <- ifelse(y.max - y.min > 0, y.max - y.min, 1)

    if(!is.null(dates)) {
      times.window<- which(dates.full > as.Date(dates[1]) & dates.full < as.Date(dates[2]))
    } else {
      times.window<- dates.full
    }

    time.min   <- which.min(times.window)
    time.max   <- which.max(times.window)
    time.count <- ifelse(time.max - time.min > 0, time.max - time.min, 1)

    # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call
    dim.order <- sapply(my.nc$var$sst$dim, function(x) x$name)

    # Set start and counts
    start.use <- c("lon" = x.min, "lat" = y.min, "time" = time.min)
    count.use <- c("lon" = x.count, "lat" = y.count, "time" = time.count)

    # Run ncvar_get, adjusting order of start and count as needed
    temp <- ncdf4::ncvar_get(my.nc, varid = "sst", start = start.use[dim.order], count = count.use[dim.order])

    # Moving from the array format of temp to a raster stack
    # Adjustment for when x and y count are 1...
    if(length(dim(temp)) == 1){
      temp.list <- lapply(seq(dim(temp)[1]), function(x) raster::raster(as.matrix(temp[x]), xmn = lons[x.min], xmx = lons[x.max], ymn = lats[y.max], ymx = lats[y.min]))
    } else {
      temp.list <- lapply(seq(dim(temp)[3]), function(x) raster::raster(temp[ , , x], xmn = lons[x.min], xmx = lons[x.max], ymn = lats[y.max], ymx = lats[y.min]))
    }

    stack.out <-suppressWarnings(raster::rotate(raster::stack(temp.list)))
    stack.out[stack.out == 327.16]<- NA
    names(stack.out) <- dates.full[-length(dates.full)]

    # Write out raster stack
    sp::proj4string(stack.out) <- proj.wgs84
    raster::writeRaster(stack.out, filename = paste(out.dir, "/", data.set, ".grd", sep = ""), overwrite = TRUE)
    return(stack.out)
  }

  ####  OISST Data  ####
  if(data.set == "OISST") {
    stem.path<- "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/noaa.oisst.v2.highres/"

    ####____Debug Null Box  ####
    #Behavior for box = NULL
    if(is.null(box)) {
      b.box <- c(0.125, 359.875, -89.875, 89.875)
    } else {
      b.box <- c(make360(box[1]), make360(box[2]), box[3], box[4])
    }

    # Get file list
    if(!is.null(dates)){
      # Get time series subset
      date.min <- format(as.Date(dates[1]), "%Y")
      date.max <- format(as.Date(dates[2]), "%Y")
      files    <- paste(stem.path, paste("sst.day.mean.", seq(from = date.min, to = date.max, by = 1), ".v2.nc", sep = ""), sep = "")
    } else {
      current.year <- format(Sys.Date(), "%Y")
      files <- paste(stem.path, paste("sst.day.mean.", seq(from = 1981, to = current.year, by = 1), ".v2.nc", sep = ""), sep = "")
    }

    stack.out <- stack()

    for(i in seq_along(files)) {
      # Connecting and extracting lat/lon/time variables from netcdf file
      my.nc <- nc_open(files[i])
      lats  <- ncvar_get(my.nc, var = "lat")
      lons  <- ncvar_get(my.nc, var = "lon")
      times <- ncvar_get(my.nc, var = "time")

      # Make times a little bit easier to handle
      dates.full <- as.Date(times, origin = '1800-01-01', tz= "GMT")


      # Find indices and windows corresponding to spatial box of interest, which are then used in the "start" and "count" arguments to the ncvar_get call for the sst variable
      x.window <- which(lons > b.box[1] & lons < b.box[2])
      x.min    <- min(x.window)
      x.max    <- max(x.window)
      x.count  <- ifelse(x.max - x.min > 0, x.max-x.min, 1)

      y.window <- which(lats > b.box[3] & lats < b.box[4])
      y.min    <- min(y.window)
      y.max    <- max(y.window)
      y.count  <- ifelse(y.max - y.min > 0, y.max - y.min, 1)

      time.min   <- which.min(dates.full)
      time.max   <- which.max(dates.full)
      time.count <- ifelse(time.max - time.min > 0, (time.max - time.min)+1, 1)

      # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call
      dim.order <- sapply(my.nc$var$sst$dim, function(x) x$name)

      # Set start and counts
      start.use <- c("lon" = x.min, "lat" = y.min, "time" = time.min)
      count.use <- c("lon" = x.count, "lat" = y.count, "time" = time.count)

      # Run ncvar_get, adjusting order of start and count as needed
      ####____Debug 2 NULL Box  ####
      # if(is.null(box)) {
      #   temp <- ncvar_get(my.nc, varid = "sst", start = c("lon" = 1, "lat" = 1, "time" = 1), count = c("lon" = -1, "lat" = -1, "time" = time.count))
      # } else {
        temp <- ncvar_get(my.nc, varid = "sst", start = start.use[dim.order], count = count.use[dim.order])
      #}




      # Moving from the array format of temp to a raster stack
      temp.list <- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
      rast.temp <- suppressWarnings(raster::rotate(stack(temp.list)))

      if(i == 1) {
        stack.out <- rast.temp
      } else {
        stack.out <- raster::stack(stack.out, rast.temp)
      }
      print(paste(files[i], "is done", sep = " "))
    }

    # Names
    stack.layers <- raster::nlayers(stack.out)

    if(!is.null(dates)){
      stack.names      <- seq(from = as.Date(dates[1]), to = as.Date(dates[1]) + (raster::nlayers(stack.out)-1), by = "day")
      names(stack.out) <- stack.names
    } else {
      start.name       <- as.Date("1981-09-01")
      end.name         <- (start.name + stack.layers) - 1
      stack.names      <- seq(from = start.name, to = end.name, by = "day")
      names(stack.out) <- stack.names
    }
  }

  # Write out raster stack
  proj4string(stack.out) <- proj.wgs84

  # Mask?
  if(!is.null(mask)){
    stack.out <- raster::mask(stack.out, mask)
    raster::writeRaster(stack.out, filename = paste(out.dir, "/", data.set, ".grd", sep = ""), overwrite = TRUE)
    return(stack.out)

  } else {
    raster::writeRaster(stack.out, filename = paste(out.dir, "/", data.set, ".grd", sep = ""), overwrite = TRUE)
    return(stack.out)
  }

} #Close env_data_extract





####______________________________####
#### Load OISST Data from Box area/time   ####


#' @title Access OISST via Data Window of lat/lon/time
#'
#' @description Returns a list of OISST raster stacks that
#' has been clipped to the desired lat/lon/time extent. Each list element
#' is named according to year, and contains all daily measurements within the
#' desired window of time, and clipped to the desired lat/lon extent.
#'
#'
#' Useful for loading OISST observations from box, but only for a particular area and/or time.
#'
#' @param okn_path User specific path to the OKN Demo Data Folder on Box. Can be obtained using shared.path funciton.
#' @param data_window dataframe with columns for lat, lon, & time indicating the extent of data desired.
#' @param anomalies Boolean indication of whether to return observed sst or anomalies. Default = TRUE.
#'
#' @return
#' @export
#'
#'@examples
#'#not run
#'# okn_path <- shared.path(group = "NSF OKN", folder = "")
#'# data_window <- data.frame(lon = c(-72, -65), lat = c(42,44), time = as.Date(c("2016-08-01", "2020-12-31")))
#'
oisst_window_load <- function(okn_path, data_window, anomalies = FALSE){

  # Get OISST data  from Box
  if(anomalies == FALSE){
    oisst_path  <- list.files(stringr::str_c(okn_path, "oisst/annual_observations/"))
    oisst_paths <- stringr::str_c(okn_path, "oisst/annual_observations/", oisst_path)
    oisst_paths <- oisst_paths[!stringr::str_detect(oisst_paths, ".zarr")] # Paths
    oisst_years <- stringr::str_sub(oisst_paths, -10, -7)                  # Yr Labels
    oisst_paths <- stats::setNames(oisst_paths, oisst_years)
  } else if(anomalies == TRUE){
    oisst_path  <- list.files(stringr::str_c(okn_path, "oisst/annual_anomalies/"))
    oisst_paths <- stringr::str_c(okn_path, "oisst/annual_anomalies/", oisst_path)
    oisst_paths <- oisst_paths[stringr::str_detect(oisst_paths, ".nc")]    # Paths
    oisst_years <- stringr::str_sub(oisst_paths, -7, -4)                   # Yr Labels
    oisst_paths <- stats::setNames(oisst_paths, oisst_years)
  }


  ###  Set limits based on desired data window

  # Shift from -180 ~ 180 to 0 ~ 360
  data_window$lon <- data_window$lon + 360
  if (max(data_window$lon) > 540){ data_window$lon <- data_window$lon - 360 }

  # Pull the min/max lat/lon to use as bbox
  lon_min <- floor(min(data_window$lon))
  lon_max <- ceiling(max(data_window$lon))
  lat_min <- floor(min(data_window$lat))
  lat_max <- ceiling(max(data_window$lat))

  # Get year vector to subset list at end and remove empty years
  start_year <- min(lubridate:::year(data_window$time))
  end_year <- max(lubridate:::year(data_window$time))
  year_vector <- as.character(c(start_year:end_year))


  #  Format dates
  if( class(data_window$time) == "Date") {
    time_min <- min(data_window$time)
    time_max <- max(data_window$time)
  } else {
    message("Time dimension not of class 'Date', all years returned.")
    time_min <- as.Date("1981-01-01")
    time_max <- as.Date("2020-12-31")
  }



  ####____ a.   Set up Rasters from Netcdf Files  ####
  oisst_ras_list <- purrr::map(oisst_paths, function(nc_year){

    # Open connection, get subsetting indices from limits
    my_nc <- ncdf4::nc_open(nc_year)

    # Years are at different locations for the file names for anomalies
    if(anomalies == FALSE){
      nc_year_label <- stringr::str_sub(nc_year, -10, -7)
    } else if(anomalies == TRUE){
      nc_year_label <- stringr::str_sub(nc_year, -7, -4)
    }


     # Tester
    #my_nc <- nc_open(oisst_paths["2018"]) ; nc_year_label <- "2018"

    # Subset to area and times of interest
    lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
    lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)



    # Date Origin is different for each anomaly netcdf so we need to split here again
    # That should be fixed, but this will work for now...
    if(anomalies == FALSE){
      time_idx <- which(
        as.Date(my_nc$dim$time$vals, origin = '1800-01-01', tz = "GMT") > time_min &
          as.Date(my_nc$dim$time$vals, origin = '1800-01-01', tz = "GMT") < time_max)
    } else if(anomalies == TRUE){
      time_idx <- which(
        as.Date(my_nc$dim$time$vals, origin = paste0(nc_year_label, '-01-01'), tz = "GMT") > time_min &
          as.Date(my_nc$dim$time$vals, origin = paste0(nc_year_label, '-01-01'), tz = "GMT") < time_max)
    }


    # If time index is less than one, output message indicating that year will be empty
    if (length(time_idx) < 1) {
      message(paste0(nc_year_label, " outside data range, not included in stack."))
      return("Year outside time extent of data")}

    # Pull netcdf data that you need using indexes
    if(anomalies == FALSE){
      nc_data <- ncdf4::ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
    } else if(anomalies == TRUE){
      nc_data <- ncdf4::ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
    }

    ####____ b. Make raster Stack from subset array  ####

    #Get lon/lat/time dimensions
    xvals <- my_nc$dim$lon$vals[lon_idx] - 360
    yvals <- my_nc$dim$lat$vals[lat_idx]
    time_dims <- 1:dim(nc_data)[3]

    # Get the dates that correspond in the least streamlined way possible, for naming purposes
    if(anomalies == FALSE){
      dates <- as.Date(my_nc$dim$time$vals[time_idx], origin = '1800-01-01', tz = "GMT")
    } else if(anomalies == TRUE){
      dates <- as.Date(my_nc$dim$time$vals[time_idx], origin = paste0(nc_year_label, '-01-01'), tz = "GMT")
    }

    # Convert Each day to a raster, rotate, and stack
    nc_stack <- purrr::map(time_dims, function(time_index){
      single_date <- nc_data[, , time_index]
      dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
                                    lat = my_nc$dim$lat$vals[lat_idx])
      single_date <- t(single_date)          # transpose

      # make/configure raster
      ras <- raster::raster(single_date,
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                            xmn = min(xvals),
                            xmx = max(xvals),
                            ymn = min(yvals),
                            ymx = max(yvals))
      ras <- raster::flip(ras, 2)
      return(ras)
    }) %>%
      raster::stack() %>%
      stats::setNames(dates)

    # Progress message
    message(paste0(nc_year_label, " done"))

    # Return the raster stack for the year
    return(nc_stack)

  })

  # Drop empty years
  out_stack <- oisst_ras_list[year_vector]
  return(out_stack)



}




# Testing Code:
# okn_path <- shared.path(group = "NSF OKN", folder = "")
# data_window <- data.frame(lon = c(-72, -65), lat = c(42,44), time = as.Date(c("2019-08-01", "2020-12-31")))
# oisst_stack <- oisst_window_load(okn_path, data_window, anomalies = TRUE)







####______________________________####
####  Mean OISST for Area/Time  ####

#' @title OISST Time-Period Means
#'
#' @description Generate average annual SST measurements from the daily OISST
#' raster brick using custom date windows. Takes the daily OISST mean temperature
#' raster stack and a dataframe outlining period start and end dates as inputs.
#'
#' Returns a raster stack with mean SST For every year for each intra-annual time-period
#' specified.
#'
#' Useful for getting means across a specific time window within a year, like a month or season.
#'
#' @param stack_in Raster stack of daily SST Measurements from OISST dataset
#' @param projection_crs The coordinate reference system number/proj4string for the desired output
#' @param time_res_df Dataframe detailing the time-period structure you wish to calculate SST means for
#'
#' @return
#' @export
#'
oisst_period_means <- function(stack_in, projection_crs = 26919, time_res_df) {

  # ####  1. Reprojection  ####
  # #Reproject if necessary
  # project_utm <- sf::st_crs(projection_crs)
  #
  # #Default is NAD1983 / UTM zone 19N Gulf of Maine
  # if(projection_crs != 26919){
  #   stack_in <- raster::projectRaster(stack_in, crs = project_utm$proj4string)
  # }

  #Intra-annual break names
  break_names <- unique(time_res_df$breaks)

  #Annual Slices, and start and end date information
  year_min     <- min(lubridate::year(time_res_df$start_date))
  year_max     <- max(lubridate::year(time_res_df$end_date))
  years        <- seq(from = year_min, to = year_max, by = 1)
  names(years) <- years
  start_months <- str_pad(lubridate::month(time_res_df$start_date), width = 2, pad = "0", side = "left")
  start_days   <- str_pad(lubridate::day(time_res_df$start_date), width = 2, pad = "0", side = "left")
  end_months   <- str_pad(lubridate::month(time_res_df$end_date), width = 2, pad = "0", side = "left")
  end_days     <- str_pad(lubridate::day(time_res_df$end_date), width = 2, pad = "0", side = "left")



  ### 2. List organization  ####

  #Set up flexible organizational structure

  # 1. Break dates are the date ranges we want to summarize by, in a list
  break_dates <- vector(mode = "list", length = length(break_names)) %>%
    stats::setNames(break_names)
  break_dates <-  break_dates %>% map(function(x) {
    x <- vector(mode = "list", length = length(years)) %>%
      stats::setNames(years)
  })


  # 2. Break indices are the raster layer indices that match them
  break_indices <- break_dates

  # 3. Break Means
  break_summs <- break_dates

  #Stacks out
  stacks_out <- vector(mode = "list", length = length(break_names)) %>%
    stats::setNames(break_names)


  ####  3. Processing Loop  ####

  #Loop through n-breaks (j) over n-years(i)
  for (j in 1:length(break_dates)) {
    for(i in seq_along(years)){

      # 1. Pull the dates for the season distinction
      break_dates[[j]][[i]] <- seq(from = as.Date(paste(years[i], start_months[j], start_days[j], sep = "-")),
                                   to   = as.Date(paste(years[i], end_months[j], end_days[j], sep = "-")), by = 1)


      # 2. Check to see if there's data to subset
      break_indices[[j]][[i]] <- which(gsub("[.]", "-", gsub("X", "", names(stack_in))) %in% as.character(break_dates[[j]][[i]]))

      #Calculate mean values for each season
      if(length(break_indices[[j]][[i]]) != 0) {
        break_summs[[j]][[i]] <- raster::calc(stack_in[[break_indices[[j]][[i]]]], mean)
      } else{
        break_summs[[j]][[i]] <- "period outside data range"

      }

      names(break_summs[[j]][[i]]) <- str_c(break_names[j], years[i],sep = ".")

    } # Close I Loop


  } #Close J Loop


  #### 4. Drop Nulls ####
  stacks_out <- break_summs %>%
    map(function(.x) {.x %>% discard(~ class(.x) == "character")})


  #### 5. Build names from valid layers  ####
  stack_names <- imap(stacks_out, function(x,y) {
    name_out <- str_c(y, names(x), sep = ".")}) %>%
    unname() %>%
    unlist()


  ####  Unlist Periods and Stack  ####
  all_breaks_mu_stack <- stacks_out %>%
    unlist() %>%
    stats::setNames(stack_names) %>%
    raster::stack()


  #And return the mother stack
  return(all_breaks_mu_stack)

}





####______________________________####
####  Calculate Daily Climatology for Area  ####



#' @title Vectorized Daily OISST 30-Year Climatology Means
#'
#' @description Generate Daily 30-year climatologies for a given area.
#'
#' This function takes a raster stack of OISST data (usually with a clipped extent for memory). Default climatology ranges from 1982-01-01 to 2011-12-31, as such
#' the input stack must extend beyond those dates estimate the climatology correctly.
#'
#'
#' Returns a raster stack with mean SST for every day in the year over the desired time window.
#'
#' @param stack_in Raster stack of daily SST Measurements from OISST dataset
#' @param projection_crs The coordinate reference system number/proj4string for the desired output
#' @param anom_period Character vector indicating start and end years to calculate climatology for. NULL defaults to 1982-2011.
#'
#' @return
#' @export
#'
calc_daily_climatologies <- function(stack_in, projection_crs = 26919, anom_period = NULL) {

  ####  1. Set up daily indexing DF  ####
  if(is.null(anom_period) == TRUE) {
    #Set up daily df
    start_dates <- seq.Date(from = as.Date("1982-01-01"), to = as.Date("1982-12-31"), by = "days")
    end_dates   <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
    time_res_df <- data.frame(
      "start_date" = start_dates,
      "end_date" = end_dates) %>%
      mutate(breaks = str_c(lubridate::month(start_date, label = T), lubridate::day(start_date)))
  } else{
    start_dates <- seq.Date(from = as.Date(str_c(anom_period[1], "-01-01")),
                            to   = as.Date(str_c(anom_period[1], "-12-31")), by = "days")
    end_dates   <- seq.Date(from = as.Date(str_c(anom_period[2], "-01-01")),
                            to   = as.Date(str_c(anom_period[2], "-12-31")), by = "days")

    time_res_df <- data.frame(
      "start_date" = start_dates,
      "end_date" = end_dates)

    time_res_df <- mutate(time_res_df,
                          breaks = str_c(lubridate::month(start_date, label = T), lubridate::day(start_date)))
  }


  ####  2. Reproject  ####
  #Reproject if necessary
  project_utm <- sf::st_crs(projection_crs)

  #Default is NAD1983 / UTM zone 19N Gulf of Maine
  if(projection_crs != 26919){
    stack_in <- raster::projectRaster(stack_in, crs = project_utm$proj4string)
  }

  #Names for the raster layers we are calculating
  break_names <- unique(time_res_df$breaks)

  ### 3. Calculate Daily Climatologies  ####

  #Set up flexible organizational structure that we can vectorize

  # 1. Break dates are the dates we wants a summary raster layer for
  break_dates <- vector(mode = "list", length = length(break_names)) %>% stats::setNames(break_names)

  # 2. Calculate daily means
  daily_climatologies <- pmap(time_res_df, function(...){

    #Walk each row using this object
    current <- tibble(...)

    #Pull that day from the stack for all years
    date_pull <- seq.Date(from = current$start_date, to = current$end_date, by = "year")

    # #Check to Make sure that date is in the stack
    break_indices <- which(gsub("[.]", "-", gsub("X", "", names(stack_in))) %in% as.character(date_pull))


    # #Calculate Mean values for each day
    if(length(break_indices) != 0) {
      daily_climatology <- raster::calc(stack_in[[break_indices]], mean)
    } else{
      daily_climatology <- "SST data Unavailable"

    }

    #Name it
    names(daily_climatology) <- current$breaks
    return(daily_climatology)

  })

  #Name the list of raster layers
  names(daily_climatologies) <- time_res_df$breaks

  #### 3. Drop Invalid Dates ####
  daily_climatologies <- daily_climatologies %>%
    discard(~ class(.x) == "character")

  #Stack and return the mother stack
  daily_stack <- raster::stack(daily_climatologies)
  return(daily_stack)

}







