####
####  Accessing OISST Mainstays Processed Data  ####
####
#### Many commonly used forms of rocessed OISST and ERSST data were used
#### Things like anomaly timeseries related to important regions like GOM, GB, or even LME's
####
#### Usage may depend on the research objective, so functionality to return
#### a netcdf/raster object or a table may be desired
####
#### Data is stored on ~/Box/RES_Data/OISST/oisst_mainstays
#### Acccess to this folder required for these paths to work.




####  Research Access Paths  ####

#' @title Establish Research Box Paths
#'
#'
#' @description Returns list of box resource paths for quick access to commonly accessed
#' resources. Includes the Mills lab, Res data, knowledge graph data, and oisst mainstays.
#'
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return res_paths List containing user-specific paths
#' @export
#'
#' @examples
#' # Not run:
#' # box_paths <- research_access_paths(box_location = "default_boxpath|cloudstorage)
research_access_paths <- function(box_location = "default_boxpath|cloudstorage"){


  # Switch for Mojave users or other mac versions with CloudStorage folder
  path_fun <- boxpath_switch(box_location = box_location)


  # Path to NSF OKN Demo Data
  okn_path <- path_fun(box_group = "NSF OKN")

  # Path to Research Team Data
  res_path   <- path_fun(box_group = "RES_Data")

  # Path to Kathy Mills Research Lab
  mills_path <- path_fun(box_group = "Mills Lab")

  # OISST Mainstays Folder
  oisst_path <- path_fun(box_group = "RES_Data", subfolder = "OISST/oisst_mainstays/")


  # group them all together for export
  res_paths <- list(okn = okn_path,
                    res = res_path,
                    mills = mills_path,
                    oisst_mainstays = oisst_path)

  # Return the list of directories
  return(res_paths)

}







# # testing
# box_paths <- gmRi::research_access_paths()



####  Timeseries Region Catalog  ####
#' @title Lookup Tool for Timeseries & Mask Locations
#'
#' @description This is a hard-coded catalog of what resources have been
#' processed as part of the oisst-mainstays processing steps.
#'
#' Names are presented in the way that they are saved. This function then
#' is used as a unified lookup source for accessing timeseries or shapefiles.
#'
#' @param region_group Name of the group of shapefiles of interest:
#' "gmri_sst_focal_areas", "lme", "nmfs_trawl_regions", ""
#'
#' @return Vector of names included in the selectedd region group
#' @export
#'
#' @examples
#' lme_areas <- get_region_names("lme")
get_region_names <- function(region_group = NULL){

  # Inform users of options
  if(is.null(region_group)){
    message("Available Region Groups Include:
            gmri_sst_focal_areas, lme, epu, nmfs_trawl_regions, nelme_regions, & gom_physio_regions")
    return(NULL) }


  # Make region names more forgiving
  # Add some text formatting so people can use spaces
  name_tidy <- stringr::str_replace_all(region_group, " ", "_")
  name_tidy <- stringr::str_to_lower(name_tidy)


  # 1. GMRI SST Focal Areas
  gmri_focal_areas <- c(
    "apershing_gulf_of_maine",
    "cpr_gulf_of_maine",
    "aak_northwest_atlantic",
    "long_island_sound")


  # 2. NMFS Regions
  nmfs_regions <- c(
    "georges_bank",
    "gulf_of_maine",
    "southern_new_england",
    "mid_atlantic_bight",
    "inuse_strata",
    "regions_collection")


  # 3. NELME Regions
  nelme_regions <- c(
    "GoM",
    "NELME",
    "SNEandMAB")

  # 4. Large Marine Ecosystems
  lme_regions <- c(
    "agulhas_current",                        "aleutian_islands",
    "antarctica",                             "arabian_sea",
    "baltic_sea",                             "barents_sea",
    "bay_of_bengal",                          "beaufort_sea",
    "benguela_current",                       "black_sea",
    "california_current",                     "canadian_eastern_arctic_west_greenland",
    "canadian_high_arctic_north_greenland",   "canary_current",
    "caribbean_sea",                          "celtic_biscay_shelf",
    "central_arctic",                         "east_bering_sea",
    "east_brazil_shelf",                      "east_central_australian_shelf",
    "east_china_sea",                         "east_siberian_sea",
    "faroe_plateau",                          "greenland_sea",
    "guinea_current",                         "gulf_of_alaska",
    "gulf_of_california",                     "gulf_of_mexico",
    "gulf_of_thailand",                       "hudson_bay_complex",
    "humboldt_current",                       "iberian_coastal",
    "iceland_shelf_and_sea",                  "indonesian_sea",
    "insular_pacific_hawaiian",               "kara_sea",
    "kuroshio_current",                       "labrador_newfoundland",
    "laptev_sea",                             "mediterranean_sea",
    "new_zealand_shelf",                      "north_australian_shelf",
    "north_brazil_shelf",                     "north_sea",
    "northeast_australian_shelf",             "northeast_us_continental_shelf",
    "northern_bering_chukchi_seas",           "northwest_australian_shelf",
    "norwegian_sea",                          "oyashio_current",
    "pacific_central_american_coastal",       "patagonian_shelf",
    "red_sea",                                "scotian_shelf",
    "sea_of_japan",                           "sea_of_okhotsk",
    "somali_coastal_current",                 "south_brazil_shelf",
    "south_china_sea",                        "south_west_australian_shelf",
    "southeast_australian_shelf",             "southeast_us_continental_shelf",
    "sulu_celebes_sea",                       "west_bering_sea",
    "west_central_australian_shelf",          "yellow_sea")


  # 5. Gulf of Maine Physio Regions
  gom_physio_regions <- c(
    "bay_of_fundy",           "bear_seamount",          "browns_bank",            "central_gulf_of_maine",
    "continental_slope",      "eastern_coastal_shelf",  "georges_bank",           "georges_basin",
    "jordan_basin",           "kelvin_seamount",        "manning_seamount",       "northern_coastal_shelf",
    "scotian_coastal_shelf",  "scotian_shelf",          "southern_coastal_shelf", "wikinson_basin"
  )



  # 6. Northeast Ecological Production Units
  ne_epu_regions <- c("GB", "GOM", "SS", "MAB")



    # Dictionary Look-up
    region_catalog <- list(
      "gmri_sst_focal_areas" = gmri_focal_areas,
      "lme"                  = lme_regions,
      "nmfs_trawl_regions"   = nmfs_regions,
      "nelme_regions"        = nelme_regions,
      "gom_physio_regions"   = gom_physio_regions,
      "epu"                  = ne_epu_regions)




    # Return Selected List
    if(name_tidy %in% names(region_catalog)){
      region_selections <- region_catalog[[name_tidy]]
      return(region_selections)
    } else{
      message("Invalid Region Group")
      message("Available Region Groups Include: gmri_sst_focal_areas, lme, nmfs_trawl_regions, nelme_regions, gom physio regions, & epu")
      return(NULL)
    }



}



# # Testing
# get_region_names("gmri sst focal areas")
# get_region_names("nelme regions")
# get_region_names("nmfs trawl regions")
# get_region_names("lme")
# get_region_names("gom physio regions")






####  Timeseries Path Directory  ####

#' @title Timeseries and Shapefile Path Look-Up
#'
#' @description Returns the full file paths for a collection of regionally masked timeseries
#' for sea surface temperature, or to the shapefiles used as the masks.
#'
#' This is a parallel to the python functions used to create them and ensure consistent names.
#'
#' @param region_group Name of the region group used to fetch region names from
#' gmRi::get_region_names(): Options: gmri_sst_focal_areas, nelme_regions, nmfs_trawl_regions, lme
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return Returns list of file paths to both the shapefiles and their sst timeseries
#' @export
#'
#' @examples
#' # Not Run:
#' # r_group <- "lme"
#' # get_timeseries_paths(region_group = r_group)
get_timeseries_paths <- function(
    region_group = "gmri",
    box_location = "default_boxpath|cloudstorage"){

  # Set base box paths using box_path()

  # Switch for Mojave users or other mac versions with CloudStorage folder
  path_fun <- boxpath_switch(box_location = box_location)

  # RES_Data path
  res_path <- path_fun(box_group = "res")

  # Root location to all the shapefiles and timeseries
  poly_root <- paste0(res_path, "Shapefiles/")
  ts_root   <- paste0(res_path, "OISST/oisst_mainstays/regional_timeseries/")

  # text formatting
  region_group <- stringr::str_replace_all(region_group, " ", "_")
  region_group <- tolower(region_group)

  # Check if region groups match available options
  group_options <- c("gmri_sst_focal_areas",
                     "lme",
                     "nmfs_trawl_regions",
                     "nelme_regions",
                     "gom_physio_regions",
                     "epu")

  if((region_group %in% group_options) == FALSE){
    message("Invalid Region Group")
    message("Available Region Groups Include: gmri_sst_focal_areas, lme, nmfs_trawl_regions, nelme_regions, epu, & gom_physio_regions")
    return(NULL)
  }




  ####  1. Polygon Paths

  # Some groups have some prefixes or different file extensions
  # the following list structures captures these patterns

  # Leading text on file names
  poly_start <- ifelse(region_group == "nmfs_trawl_regions", "nmfs_trawl_", "")

  # file types and trailing text patterns
  poly_end <- list(
    "gmri_sst_focal_areas" = ".geojson",
    "lme"                  = "_exterior.geojson",
    "nmfs_trawl_regions"   = ".geojson",
    "nelme_regions"        = "_sf.shp",
    "gom_physio_regions"   = ".geojson",
    "epu"                  = ".geojson")

  # full path prior to region names
  poly_extensions <- list(
    "gmri_sst_focal_areas" = paste0(poly_root, "gmri_sst_focal_areas/", poly_start),
    "lme"                  = paste0(poly_root, "large_marine_ecosystems/", poly_start),
    "nmfs_trawl_regions"   = paste0(poly_root, "nmfs_trawl_regions/", poly_start),
    "nelme_regions"        = paste0(poly_root, "NELME_regions/", poly_start),
    "gom_physio_regions"   = paste0(poly_root, "GulfOfMainePhysioRegions/single_regions/", poly_start),
    "epu"                  = paste0(poly_root, "EPU/individual_epus/", poly_start)
  )


  ####  2. Timeseries Paths

  # Leading text on file names
  ts_start <- "OISSTv2_anom_"

  # file types and trailing text patterns
  ts_end <- ".csv"

  # full path prior to region names
  ts_extensions <- list(
    "gmri_sst_focal_areas" = paste0(ts_root, "gmri_sst_focal_areas/", ts_start),
    "lme"                  = paste0(ts_root, "large_marine_ecosystems/", ts_start),
    "nmfs_trawl_regions"   = paste0(ts_root, "nmfs_trawl_regions/", ts_start),
    "nelme_regions"        = paste0(ts_root, "NELME_regions/", ts_start),
    "gom_physio_regions"   = paste0(ts_root, "GulfOfMainePhysioRegions/", ts_start),
    "epu"                  = paste0(ts_root, "EPU/", ts_start)
  )


  #### 3. Build list to contain all paths

  # Get list of available polygons based on the group
  region_list <- get_region_names(region_group = region_group)

  # sort
  region_list <- sort(region_list)

  # For each one return the timeseries and polygon locations
  region_paths <- purrr::map(region_list, function(region_i){

    # Path to shapefile based on group and appropriate file ending
    shape_path_i <- paste0(poly_extensions[[region_group]], region_i, poly_end[[region_group]])

    # Path to timeseries based on group and appropriate file ending
    ts_path_i <- paste0(ts_extensions[[region_group]], region_i, ts_end)

    # List structure contains both
    return(list("shape_path" = shape_path_i,
                "timeseries_path" = ts_path_i))

  })

  # Set the names
  region_paths <- stats::setNames(region_paths, region_list)

  # Return the list of locations
  return(region_paths)


}



# # # Testing
# oisst_path <- box_path("res", "OISST/oisst_mainstays")
# get_timeseries_paths(region_group = "gmri sst focal areas")
# get_timeseries_paths(region_group = "nelme regions")
# get_timeseries_paths(region_group = "lme")
# get_timeseries_paths(region_group = "nmfs trawl regions")
# get_timeseries_paths(region_group = "gom physio regions")







####  OISST Regional Timeseries  ####
#' @title Access Regional Timeseries from OISSTv2 Mainstays
#'
#' @description Tool for accessing oisst regional timeseries from Box.
#' Regionally masked timeseries have been pre-processed and stored on box. Quick access
#' is available here.
#'
#' This function is used primarily as a pass-through function for oisst_access_timeseries(),
#' which relies on it to look-up file paths.
#'
#' Simply specify your personal path to the oisst_mainstays folder,
#' the group of polygons your area is in, and the name of the region itself.
#'
#'
#'
#' @param region_family Identify the family of shapefiles that you are interested in.
#' Choices = "LME", "nmfs trawl regions", "gmri focus areas"
#' @param poly_name String Identifying the shapefile name that was used as mask.
#' Used to build file name.
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return Time series dataframe for the selected region.
#' @export
#'
#' @examples
#' #Not run:
#'
#' # box_paths <- gmRi::research_access_paths(os.use = "unix",
#' #                                          user.name = "not applicable")
#' # agulhas <- oisst_access_timeseries(
#' #    region_family = "lme",
#' #    poly_name = "agulhas current",
#' #    box_location = "default_boxpath|cloudstorage)
oisst_access_timeseries <- function(
    region_family = "gmri",
    poly_name = "apershing gulf of maine",
    box_location = "default_boxpath|cloudstorage"){



  # Set up data path for sat source, originally all in okn demo data folder
  # Switch for Mojave users or other mac versions with CloudStorage folder
  path_fun <- boxpath_switch(box_location = box_location)


  # Timeseries live in RES_Data/OISST/oisst_mainstays
  source_path <- path_fun(box_group = "res", subfolder = "OISST/oisst_mainstays")

  # do some string adjustment to account for underscores and caps
  tidy_name <- tolower(region_family)
  tidy_name <- stringr::str_replace_all(tidy_name, "_", " ")

  # State the group options for "region family" if user provided option doesn't match
  group_options <- c("nmfs trawl regions", "trawl regions", "trawl", "trawl survey",
                     "lme", "large marine ecosystems",
                     "gmri focus areas", "gmri", "gmri focal areas",
                     "epu", "ecological production units",
                     "nelme regions",
                     "gom physio regions")

  # Return message with group options for the unfamiliar
  if((tidy_name %in% group_options) == FALSE){
    message("Invalid region family.\nAvailable choices are:\n")
    message(paste(group_options, collapse = "\n"))
    return("If no options appear try changing box_location between 'default' & 'cloudstorage'")}




  # Build file path to group folder based on region family
  timeseries_folder <-  switch(
    EXPR = tidy_name,
    "nmfs trawl regions"          = paste0(source_path, "regional_timeseries/nmfs_trawl_regions/"),
    "trawl survey"                = paste0(source_path, "regional_timeseries/nmfs_trawl_regions/"),
    "trawl"                       = paste0(source_path, "regional_timeseries/nmfs_trawl_regions/"),
    "trawl regions"               = paste0(source_path, "regional_timeseries/nmfs_trawl_regions/"),
    "lme"                         = paste0(source_path, "regional_timeseries/large_marine_ecosystems/"),
    "large marine ecosystems"     = paste0(source_path, "regional_timeseries/large_marine_ecosystems/"),
    "epu"                         = paste0(source_path, "regional_timeseries/EPU/"),
    "ecological production units" = paste0(source_path, "regional_timeseries/EPU/"),
    "gmri focus areas"            = paste0(source_path, "regional_timeseries/gmri_sst_focal_areas/"),
    "gmri focal areas"            = paste0(source_path, "regional_timeseries/gmri_sst_focal_areas/"),
    "gulf of maine"               = paste0(source_path, "regional_timeseries/gmri_sst_focal_areas/"),
    "gmri"                        = paste0(source_path, "regional_timeseries/gmri_sst_focal_areas/"),
    "nelme regions"               = paste0(source_path, "regional_timeseries/NELME_regions/"),
    "gom physio regions"          = paste0(source_path, "regional_timeseries/GulfOfMainePhysioRegions/"))



  # Generate list of options in case people don't know what is available
  available_ts <- list.files(timeseries_folder, pattern = "csv")


  # Format the file names to match against poly_name
  available_polys <-  purrr::map_chr(available_ts, function(ts){
    ts <- stringr::str_replace(ts, ".csv", "")
    ts <- stringr::str_replace_all(ts, "_", " ")
    ts <- stringr::str_replace(ts, "OISSTv2 anom ", "")
    ts <- tolower(ts)})


  # Check if the supplied polygon is in the list for that group
  if( (tolower(poly_name) %in% available_polys) == FALSE){
    message(paste0("Invalid poly_name of: ", poly_name, "\nAvailable Polygons for ",
                   region_family, " group include: \n"))
    message(paste0(available_polys, collapse = "\n"))
    return("If no options appear try changing box_location between 'default' & 'cloudstorage'")
  }


  # Name configuration for path to polygon's timeseries file
  # from human literate -> file name
  poly_name <- tolower(poly_name)
  poly_name <- stringr::str_replace_all(poly_name, " ", "_")
  poly_name <- stringr::str_replace_all(poly_name, "-", "_")
  poly_name <- stringr::str_replace_all(poly_name, "___", "")
  poly_name <- stringr::str_replace_all(poly_name, "__", "")
  poly_name <- paste0("OISSTv2_anom_", poly_name, ".csv")

  # Build full file path
  timeseries_path <-  paste0(timeseries_folder, poly_name)

  # Read Timeseries
  timeseries_out <- utils::read.csv(timeseries_path)

  # Return it as output
  return(timeseries_out)


}






# # # Testing
#
# # wrong box path
# oisst_access_timeseries(region_family = "gom physio regions")
#
# # invalid group name
# oisst_access_timeseries(region_family = "1", box_location = "cloudstorage")
#
# # invalid region
# oisst_access_timeseries(region_family = "gmri focus areas", poly_name = "gulf of maine", box_location = "cloudstorage")
#
# # successfull use
# oisst_access_timeseries(region_family = "lme", poly_name = "baltic sea", box_location = "cloudstorage")
#










#### OISST Data for area/time   ####


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
#' @param data_window dataframe with columns for lat, lon, & time indicating the extent of
#' the data desired.
#' @param anomalies Boolean indication of whether to return observed sst or anomalies.
#' Default = TRUE.
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return Raster stack of OISSTv2 data using desired dimensions to crop
#' @export
#'
#'@examples
#'#not run
#'# oisst_path <- box_path(box_group = "res", subfolder = "OISST/oisst_mainstays")
#'# data_window <- data.frame(lon = c(-72, -65),
#'#                           lat = c(42,44),
#'#                           time = as.Date(c("2016-08-01", "2020-12-31")))
#'
oisst_window_load <- function(data_window, anomalies = FALSE, box_location = "default_boxpath|cloudstorage"){

  # Switch for Mojave users or other mac versions with CloudStorage folder
  path_fun <- boxpath_switch(box_location = box_location)


  # Get OISST data  from Box
  oisst_path <- path_fun(box_group = "res", subfolder = "OISST/oisst_mainstays")

  # Accessing Observed Temperatures
  if(anomalies == FALSE){
    file_names <- list.files(stringr::str_c(oisst_path, "annual_observations/"))
    file_paths <- stringr::str_c(oisst_path, "annual_observations/", file_names)
    file_paths <- file_paths[!stringr::str_detect(file_paths, ".zarr")]  # No .zarr files
    file_years <- stringr::str_sub(file_paths, -10, -7)                  # Yr Labels
    file_paths <- stats::setNames(file_paths, file_years)

    # Accessing Anomalies
  } else if(anomalies == TRUE){
    file_names <- list.files(stringr::str_c(oisst_path, "annual_anomalies/1982to2011_climatology"))
    file_paths <- stringr::str_c(oisst_path, "annual_anomalies/1982to2011_climatology/", file_names)
    file_paths <- file_paths[stringr::str_detect(file_paths, ".nc")]     # No .zarr files
    file_years <- stringr::str_sub(file_paths, -7, -4)                   # Yr Labels
    file_paths <- stats::setNames(file_paths, file_years)
  }


  ###  Set limits based on desired data window

  # Shift from -180 ~ 180 to 0 ~ 360
  data_window$lon <- data_window$lon + 360
  if (max(data_window$lon) > 540){ data_window$lon <- data_window$lon - 360 }

  # Pull the min/max lat/lon to use as bbox
  lon_min <- floor(min(data_window$lon))
  lat_min <- floor(min(data_window$lat))
  lon_max <- ceiling(max(data_window$lon))
  lat_max <- ceiling(max(data_window$lat))

  # Get year vector to subset list at end and remove empty years
  start_year  <- min(lubridate::year(data_window$time))
  end_year    <- max(lubridate::year(data_window$time))
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



  #### a.   Set up Rasters from Netcdf Files  ####
  oisst_ras_list <- purrr::map(file_paths, function(nc_year){

    # Open connection, get sub-setting indices from limits
    my_nc <- ncdf4::nc_open(nc_year)

    # Years are at different locations for the file names for anomalies
    if(anomalies == FALSE){
      nc_year_label <- stringr::str_sub(nc_year, -10, -7)
    } else if(anomalies == TRUE){
      nc_year_label <- stringr::str_sub(nc_year, -7, -4)
    }


    # Tester
    #my_nc <- nc_open(file_paths["2018"]) ; nc_year_label <- "2018"

    # Subset to area and times of interest
    lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
    lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)



    # Date Origin WAS different for each anomaly netcdf so we need to split here again
    time_idx <- which(
      as.Date(my_nc$dim$time$vals, origin = '1800-01-01', tz = "GMT") > time_min &
        as.Date(my_nc$dim$time$vals, origin = '1800-01-01', tz = "GMT") < time_max)



    # If time index is less than one, output message indicating that year will be empty
    if(length(time_idx) < 1){
      message(paste0(nc_year_label, " outside data range, not included in stack."))
      return("Year outside time extent of data")
    }

    # Pull netcdf data that you need using indexes
    if(anomalies == FALSE){
      nc_data <- ncdf4::ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
    } else if(anomalies == TRUE){
      nc_data <- ncdf4::ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
    }

    #### b. Make raster Stack from subset array  ####

    #Get lon/lat/time dimensions
    xvals <- my_nc$dim$lon$vals[lon_idx] - 360
    yvals <- my_nc$dim$lat$vals[lat_idx]
    time_dims <- 1:dim(nc_data)[3]

    # Get the dates that correspond in the least streamlined way possible, for naming purposes
    if(anomalies == FALSE){
      dates <- as.Date(my_nc$dim$time$vals[time_idx],
                       origin = '1800-01-01',
                       tz = "GMT")
    } else if(anomalies == TRUE){
      dates <- as.Date(my_nc$dim$time$vals[time_idx],
                       origin = '1800-01-01', # This will change with origin info
                       tz = "GMT")
    }

    # Convert Each day to a raster, rotate, and stack
    nc_list <- purrr::map(time_dims, function(time_index){
      single_date <- nc_data[, , time_index]
      dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
                                    lat = my_nc$dim$lat$vals[lat_idx])
      # transpose
      single_date <- t(single_date)

      # make/configure raster
      ras <- raster::raster(single_date,
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                            xmn = min(xvals),
                            xmx = max(xvals),
                            ymn = min(yvals),
                            ymx = max(yvals))
      ras <- raster::flip(ras, 2)
      return(ras)})

    # Stack and set names
    nc_stack <- raster::stack(nc_list)
    nc_stack <- stats::setNames(nc_stack, dates)

    # Progress message
    message(paste0(nc_year_label, " done"))

    # Return the raster stack for the year
    return(nc_stack)

  })

  # Drop empty years
  out_stack <- oisst_ras_list[year_vector]
  return(out_stack)



}





####  Access Global OISSTv2 Files  ####
#' @title Access OISST Mainstays Arrays
#'
#' @param resource Name of the global extent resource, choices are raw, climatology, warming rates,
#' and anomalies.
#' @param year_range optional vector of years for raw or anomalies data resources.
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return resource_out Raster stack of the desired netcdf array
#' @export
#'
#' @examples
#' # Not run
#' # load_global_oisst(resource = "warming rates", year_range = NULL)
load_global_oisst <- function(resource = c("raw", "climatology82", "climatology91", "anomalies", "warming rates"),
                              year_range = seq(2010, 2020, 1),
                              box_location = "default_boxpath|cloudstorage"){

  # Still testing
  message("Function currently in development.")

  # Path to data
  # Switch for Mojave users or other mac versions with CloudStorage folder
  path_fun <- boxpath_switch(box_location = box_location)

  oisst_path <- path_fun(box_group = "Res", subfolder = "OISST/oisst_mainstays")

  # Redirect to resource folder
  resource <- tolower(resource)
  resource_folder <- switch(
    EXPR = tolower(resource),
    "raw"           = paste0(oisst_path, "/annual_observations/"),
    "climatology82" = paste0(oisst_path, "/daily_climatologies/daily_clims_1982to2011.nc"),
    "climatology91" = paste0(oisst_path, "/daily_climatologies/daily_clims_1991to2020.nc"),
    "anomalies"     = paste0(oisst_path, "/annual_anomalies/1982to2011_climatology/"),
    "warming rates" = paste0(oisst_path, "/warming_rates/annual_warming_rates.nc"))
  resource_folder <- stringr::str_replace(resource_folder, "//", "/")

  # Grab stacks for options that are single netcdf files
  if(resource %in% c("climatology82", "climatology91", "warming rates")){
    resource_out <- raster::stack(resource_folder)

  # for raw obs and anomalies there are multiple files, one for each year
  } else if(resource %in% c("raw", "anomalies")){

    # Make list of paths for each year
    resource_list <- c()
    for(i in 1:length(year_range)){
      resource_list[i] <- switch(
        "raw"       = paste0(resource_folder, "sst.day.mean.", year_range[i], ".v2.nc"),
        "anomalies" = paste0(resource_folder, "daily_anoms_", year_range[i], ".nc"))}

    # Stack the years
    resource_out <- raster::stack(resource_list)

  }

  # Note which climatology the anomalies are from:
  if(stringr::str_detect(resource, "anomalies") == TRUE) {message("Anomalies relative to 1982-2011 climatology.")}


  # Return the raster stack of the array(s)
  return(resource_out)

}



####  CURRENT Development  ####



#' #' @title Load Reference Shapefile for OISST Regional Timeseries
#' #'
#' #'
#' #' @description Import shapefile as sf object for the desired oisst timeseries. Useful for
#' #' validating what shapefile was used, and for adding maps to accompany timeseries.
#' #'
#' #' @param res_path character string indicating local path to Box/RES_Data directory
#' #' @param region_family Character string indicating what family the shapefile is within.
#' #' Choices = "LME", "nmfs trawl regions", "gmri focus areas"
#' #' @param poly_name Specific name of the shape you wish to access
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' oisst_timeseries_poly <- function(res_path,
#'                                   region_family = c("nmfs trawl regions", "lme", "gmri focus areas"),
#'                                   poly_name = "gulf of maine"){
#'
#'
#'
#'   # Set up data path for sat source, originally all in okn demo data folder
#'   # Now all the timeseries are in RES_Data/OISST/oisst_mainstays
#'   source_path <- res_path
#'   if(stringr::str_sub(source_path, -1, -1) != "/") {source_path <- paste0(source_path, "/")}
#'
#'
#'   # State the group options for "region family" if user provided option doesn't match
#'   group_options <- c("nmfs trawl regions", "trawl regions",
#'                      #"lme", "large marine ecosystems",
#'                      "gmri focus areas", "gulf of maine",
#'                      "epu", "ecological production units")
#'
#'   # Return message with group options for the unfamiliar
#'   if((tolower(region_family) %in% group_options) == FALSE){
#'     message("Invalid region family.\n Available choices are:\n")
#'     message(paste(group_options, collapse = "\n"))
#'     return("invalid region choice selected.")}
#'
#'
#'
#'
#'   # Build file path to group folder based on region family
#'   poly_folder <-  switch(
#'     EXPR = tolower(region_family),
#'     "nmfs trawl regions"          = paste0(source_path, "Shapefiles/nmfs_trawl_regions/"),
#'     "trawl regions"               = paste0(source_path, "Shapefiles/nmfs_trawl_regions/"),
#'     "lme"                         = paste0(source_path, "Shapefiles/large_marine_ecosystems/"),
#'     "large marine ecosystems"     = paste0(source_path, "Shapefiles/large_marine_ecosystems/"),
#'     "epu"                         = paste0(source_path, "Shapefiles/EPU/individual_epus/"),
#'     "ecological production units" = paste0(source_path, "Shapefiles/EPU/individual_epus/"),
#'     "gmri focus areas"            = paste0(source_path, "Shapefiles/gmri_sst_focal_areas/"),
#'     "gulf of maine"               = paste0(source_path, "Shapefiles/gmri_sst_focal_areas/"))
#'
#'
#'
#'   # Generate list of options in case people don't know what is available
#'   available_polys <- list.files(poly_folder, pattern = c("shp", ".geojson"))
#'
#'
#'   # Format the file names to match against poly_name
#'   available_polys <-  purrr::map_chr(available_polys, function(poly){
#'     poly <- stringr::str_replace(poly, ".geojson", "")
#'     poly <- stringr::str_replace(poly, ".shp", "")
#'     poly <- stringr::str_replace_all(poly, "_", " ")
#'   })
#'
#'
#'   # Check if the supplied polygon is in the list for that group
#'   if( (tolower(poly_name) %in% available_polys) == FALSE){
#'     message(paste0("Invalid poly_name of: ", poly_name, "\nAvailable Polygons for ",
#'                    region_family, " group include: \n"))
#'     message(paste0(available_polys, collapse = "\n"))
#'     return("Invalid poly_name choice selected.")
#'   }
#'
#'
#'   # Need some code to reverse engineer the actual shape names from human legible ones
#'
#'
#'   # Then need to load them and export
#'
#'
#'
#'
#' }


# # testing
# box_paths <- gmRi::research_access_paths(os.use = "unix",
#                                          user.name = "not applicable")
#
#
# # testing an incorrect group name:
# oisst_timeseries_poly(res_path = box_paths$oisst_mainstays,
#                       region_family = "bad group",
#                       poly_name = "incorrect polygon")
#
#
# # testing an incorrect polygon name:
# oisst_timeseries_poly(res_path = box_paths$oisst_mainstays,
#                       region_family = "gmri focus areas",
#                       poly_name = "incorrect polygon")


