####  Sea Surface Extractor Functions for Optimum Interpolation Sea Surface Temperature (OISST)  ####
####  About: https://www.ncdc.noaa.gov/oisst


#' @title Longitude 180 to 360
#'
#' @description This is a simple function to translate negative longitudes
#' (measured on -180:180 scale) into 0-360, which is coordinate system used by some environmental datasets.
#'
#' @param lon Longitude in -180:180 degrees
#'
#' @return lon Longitude from 0-360 degrees
#' @export
#'
#' @examples
#'
make360 <- function(lon) {

  ind <- which(lon < 0)
  lon[ind] <- lon[ind] + 360
  return(lon)
}


#' @title Fix Raster
#'
#' @description his function helps convert an array (x, y, z)
#' into raster layers with the correct dimensions and orientation.
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
#' @examples
#'
fix_raster <- function(x, lons.use, lats.use, x.min.use, x.max.use, y.min.use, y.max.use) {

  r.temp<- t(x)[ncol(x):1,]
  rast.out<- raster::raster(r.temp, xmn = lons.use[x.min.use], xmx = lons.use[x.max.use], ymn = lats.use[y.min.use], ymx = lats.use[y.max.use])
  return(rast.out)

  ## End function
}


#' @title OISST list to Stack
#'
#' @description This function loops over daily OISST files and returns a single stack.
#' Installs "ncdf4" and "raster" packages if not installed.
#'
#' @param box bounding box object used to clip netcdf data
#' @param times.window two element vector of desired start and end dates. Must be able to convert to date using as.Date
#'
#'
#' @return rast.temp Raster stack of OI Sea Surface Temperatures
#' @export
#'
#' @examples
#'
oisst_list_to_stack <- function(box, times.window) {

  ## Start function
  # Install libraries
  libraries <- c("ncdf4", "raster")
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
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
  my.nc  <- ncdf4::nc_open(paste(data.stem, x, sep = ""))
  lats    <- ncdf4::ncvar_get(my.nc, var = "lat")
  lons    <- ncdf4::ncvar_get(my.nc, var = "lon")
  times   <- ncdf4::ncvar_get(my.nc, var = "time")

  # Make times a little bit easier to handle
  dates.full <- as.Date(times, origin='1800-01-01', tz= "GMT")

  # Find indices and windows corresponding to spatial box of interest,
  # which are then used in the "start" and "count" arguments to the ncvar_get call
  # for the sst variable
  b.box   <- c(make360(box[1]), make360(box[2]), box[3], box[4])
  x.window <- which(lons > b.box[1] & lons < b.box[2])
  x.min   <- min(x.window)
  x.max   <- max(x.window)
  x.count <- ifelse(x.max-x.min > 0, x.max-x.min, 1)

  y.window <- which(lats > b.box[3] & lats < b.box[4])
  y.min   <- min(y.window)
  y.max   <- max(y.window)
  y.count <- ifelse(y.max - y.min > 0, y.max - y.min, 1)


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
  temp <- ncvar_get(my.nc, varid = "sst", start = start.use[dim.order], count = count.use[dim.order])

  # Moving from the array format of temp to a raster stack
  temp.list<- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
  rast.temp<- raster::rotate(raster::stack(temp.list))

  #Output Stack
  return(rast.temp)

  ## End function
}






#' @title Satellite Data Extraction Function
#'
#' @description This function accesses webhosted satellite data and then downloads a subset of the data based
#' on dates and long/lat bounding box. After downloading the data, the function processes it and saves it as
#' one raster stack file. Installs "ncdf4" and "raster" packages if not installed.
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
#' @examples
#'
env_data_extract <- function(data.set = "OISST", dates = NULL, box = c(-77, -60, 35, 46), out.dir = here::here("data", "OISST_thredd_test"), mask = NULL) {


  ## Start function
  # Install libraries
  libraries <- c("ncdf4", "raster")
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
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

  # ERSST
  if(data.set == "ERSST") {
    # Set data path
    data.path <- "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/noaa.ersst/sst.mnmean.v4.nc"

    # Some steps to make things cooperate with nc functions, including getting variables from -180:180 to 0-360
    b.box <- c(make360(box[1]), make360(box[2]), box[3], box[4])

    # Connecting and extracting lat/lon/time variables from netcdf file
    my.nc <- nc_open(data.path)
    lats  <- ncvar_get(my.nc, var = "lat")
    lons  <- ncvar_get(my.nc, var = "lon")
    times <- ncvar_get(my.nc, var = "time")

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

    stack.out <- raster::rotate(stack(temp.list))
    stack.out[stack.out == 327.16]<- NA
    names(stack.out)<- dates.full[-length(dates.full)]

    # Write out raster stack
    sp::proj4string(stack.out) <- proj.wgs84
    raster::writeRaster(stack.out, filename = paste(out.dir, "/", data.set, ".grd", sep = ""), overwrite = TRUE)
    return(stack.out)
  }

  # OISST -- Download of full timeseries takes ~ 20 minutes.
  if(data.set == "OISST") {
    stem.path<- "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/noaa.oisst.v2.highres/"

    b.box <- c(make360(box[1]), make360(box[2]), box[3], box[4])

    # Get file list
    if(!is.null(dates)){
      # Get time series subset
      date.min <- format(as.Date(dates[1]), "%Y")
      date.max <- format(as.Date(dates[2]), "%Y")
      files    <- paste(stem.path, paste("sst.day.mean.", seq(from = date.min, to = date.max, by = 1), ".v2.nc", sep = ""), sep = "")
    } else {
      current.year <- format(Sys.Date(), "%Y")
      files<- paste(stem.path, paste("sst.day.mean.", seq(from = 1981, to = current.year, by = 1), ".v2.nc", sep = ""), sep = "")
    }

    stack.out <- stack()

    for(i in seq_along(files)){
      # Connecting and extracting lat/lon/time variables from netcdf file
      my.nc <- nc_open(files[i])
      lats  <- ncvar_get(my.nc, var = "lat")
      lons  <- ncvar_get(my.nc, var = "lon")
      times <- ncvar_get(my.nc, var = "time")

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

      time.min   <- which.min(dates.full)
      time.max   <- which.max(dates.full)
      time.count <- ifelse(time.max - time.min > 0, (time.max - time.min)+1, 1)

      # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call
      dim.order <- sapply(my.nc$var$sst$dim, function(x) x$name)

      # Set start and counts
      start.use <- c("lon" = x.min, "lat" = y.min, "time" = time.min)
      count.use <- c("lon" = x.count, "lat" = y.count, "time" = time.count)

      # Run ncvar_get, adjusting order of start and count as needed
      temp <- ncvar_get(my.nc, varid = "sst", start = start.use[dim.order], count = count.use[dim.order])

      # Moving from the array format of temp to a raster stack
      temp.list <- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
      rast.temp <- raster::rotate(stack(temp.list))

      if(i == 1) {
        stack.out <- rast.temp
      } else {
        stack.out <- stack(stack.out, rast.temp)
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
      end.name         <- (start.name + stack.layers)-1
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

####  Testing  ####




# library(tidyverse)
# library(here)

# # # Using data range from a file
# station_data <- read_csv("/Users/akemberling/Box/Adam Kemberling/Box_Projects/Convergence_ML/data/trawldat.csv")
# station_data <- station_data %>%
#   mutate(ID = as.character(ID),
#          DATE = str_c(EST_YEAR, EST_MONTH, EST_DAY, sep = "-")) %>%
#   arrange(EST_YEAR, EST_MONTH, EST_DAY) %>%
#   filter(EST_YEAR >= 1982)
#
# date_start <- as.character(head(station_data)[1, "DATE"])
# date_stop  <- as.character(tail(station_data, rows = 1)[1, "DATE"])
# date_range <- c(date_start, date_stop)

# #Manual, small date range for testing
# date_range <- c("2017-1-1", "2018-5-11")
#
#
#
# #### env_data_extract -test  ####
# env_data_extract(data.set = "OISST",
#                  dates = date_range,
#                  box = c(-77, -60, 35, 46),
#                  out.dir = "/Users/akemberling/Documents/Repositories/testing_dir",
#                  mask = NULL)
#
#
#
# ####__####
# ####  NETCDF Point Value Extraction  ####
# ####  Extracting values from full raster stack for specific locations and specific times
#
# #Load data
# station_data <- read_csv("~/Box/Adam Kemberling/Box_Projects/Convergence_ML/data/trawldat.csv")
# station_data <- station_data %>%
#   mutate(ID = as.character(ID)) %>%
#   arrange(EST_YEAR, EST_MONTH, EST_DAY) %>%
#   filter(EST_YEAR >= 1982)
#
# #Point locations
# head(station_data)
#
# #Raster Stack
# sst_stack <- raster::stack("/Users/akemberling/Documents/Repositories/testing_dir/OISST.grd")
#
#
# 1. Make a column that will match date indices from raster stack

# # raster names cannot start with a number, or contain spaces or underscores
# # goal is to match format of names(sst_stack)
# station_data <- station_data %>%
#   mutate(
#     valid_dates = str_c("X",
#                         EST_YEAR, ".",
#                         str_pad(EST_MONTH, width = 2, side = "left", pad = "0"), ".",
#                         str_pad(EST_DAY, width = 2, side = "left", pad = "0")))




#' Make Valid Raster Stack Dates
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
#' @examples
#'
make_stack_dates <- function(point_location_df, year_col, month_col, day_col) {

  #tidy quotation
  year_col  <- rlang::enquo(year_col)
  month_col <- rlang::enquo(month_col)
  day_col   <- rlang::enquo(day_col)

  #Create valid_dates
  df_out <-  dplyr::mutate(point_location_df,
      valid_dates = str_c("X",         #Must not begin with a number
                          !!year_col,  #followed by year
                          ".",         #spaces become periods
                          str_pad(!!month_col, width = 2, side = "left", pad = "0"), #month is padded
                          ".",         #connected by another period
                          str_pad(!!day_col, width = 2, side = "left", pad = "0"))   #day is also padded
    )


  return(df_out)
}


# #Works without quoting the columns
# station_data <- make_stack_dates(
#   point_location_df = station_data,
#   year_col = EST_YEAR,
#   month_col = EST_MONTH,
#   day_col = EST_DAY)
#
#
#
# # 2. Put station locations into a list, grouped by those index values
# stations_l <- station_data %>% split(.$valid_dates)
#
# # 3. Subset dates to just the ones available in stack
# stations_l <- stations_l[which(names(stations_l) %in% names(sst_stack))]
#
# # 4. Function to extract values by date
# point_extraction <- function(points_list = stations_l, ras_stack = sst_stack, xcoord = "DECDEG_BEGLON", ycoord = "DECDEG_BEGLAT") {
#
#   #Extract the date index from the station data
#   date_index <- as.character(points_list[1, "valid_dates"])
#
#   #Pull the values for each station that day
#   vals_out <- raster::extract(
#     ras_stack[[date_index]],           #Target Date Layer
#     points_list[ , c(xcoord, ycoord)], #Coordinates
#     df = T                             #Output as Dataframe
#     )
#
#   #Add extracted values to original data
#   df_out <- points_list %>% mutate(ras_val = vals_out[,2])
#   return(df_out)
#
#
#
# } # End Point extraction function
#
# # 4. Use map() or lapply() to iterate through the list and extract values
# test_extract <- purrr::map(.x = stations_l, .f = point_extraction, sst_stack)
#
# # 5. Collapse to a dataframe, rename variable
# sst_out <- bind_rows(test_extract) %>% rename(sst = ras_val)







