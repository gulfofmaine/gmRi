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
#'
#' @param os.use String flag indicating what operating system the user is currently using.
#' Options are "unix" for mac users or "windows".
#' @param user.name User name for navigating root directory structure on windows.
#'
#' @return res_paths List containing user-specific paths
#' @export
#'
#' @examples
#' # Not run:
#' # box_paths <- research_access_paths(os.use = "unix", user.name = "NA, I use a mac")
research_access_paths <- function(os.use = "unix", user.name = "not applicable, I use a mac."){

  # Pre-load a user name for windows
  user.name <- user.name

  # Path to NSF OKN Demo Data
  okn_path <- shared.path(os.use = os.use, group = "NSF OKN", folder = "")

  # Path to Research Team Data
  res_path   <- shared.path(os.use = os.use, group = "RES_Data", folder = NULL)

  # Path to Kathy Mills Research Lab
  mills_path <- shared.path(os.use = os.use, group = "Mills Lab", folder = "")

  # OISST Mainstays Folder
  oisst_path <- shared.path(os.use = os.use, group = "RES_Data", folder = "OISST/oisst_mainstays/")


  # group them all together for export
  res_paths <- list(okn = okn_path,
                    res = res_path,
                    mills = mills_path,
                    oisst_mainstays = oisst_path)

  # Return the list of directories
  return(res_paths)

}







# # testing
# box_paths <- gmRi::research_access_paths(os.use = "unix",
#                                          user.name = "not applicable")
#
#
# # testing an incorrect group name
# oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays,
#                         region_family = "bad group",
#                         poly_name = "incorrect polygon")
#
#
# # testing an invalid polygon name
# oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays,
#                         region_family = "trawl regions",
#                         poly_name = "incorrect polygon")
#
# # testing a valid polygon name
# oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays,
#                         region_family = "lme",
#                         poly_name = "agulhas current")




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
get_region_names <- function(region_group = "gmri_sst_focal_areas"){

  # Make region names more forgiving
  # Add some text formatting so people can use spaces
  name_tidy <- stringr::str_replace_all(region_group, " ", "_")
  name_tidy <- stringr::str_to_lower(region_group)


  # 1. GMRI SST Focal Areas
  gmri_focal_areas <- c(
    "apershing_gulf_of_maine",
    "cpr_gulf_of_maine",
    "aak_northwest_atlantic")


  # 2. NMFS Regions
  nmfs_regions <- c(
    "georges_bank",
    "gulf_of_maine",
    "southern_new_england",
    "mid_atlantic_bight")


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


    # Dictionary Look-up
    region_catalog <- list(
      "gmri_sst_focal_areas" = gmri_focal_areas,
      "lme"                  = lme_regions,
      "nmfs_trawl_regions"   = nmfs_regions,
      "nelme_regions"        = nelme_regions)

    # Return Selected List
    region_selections <- region_catalog[[region_group]]
    return(region_selections)


}


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
#' @param os.use Operating system setting passed to gmRi::shared.path()
#' @param user.name Optional configuration for Windows users trying to use shared.path
#'
#' @return Returns list of file paths to both the shapefiles and their sst timeseries
#' @export
#'
#' @examples
#' # Not Run:
#' # r_group <- "lme"
#' # get_timeseries_paths(region_group = r_group, region_list = get_region_names(r_group))
get_timeseries_paths <- function(region_group, os.use = "unix", user.name = NULL){

  # Set base box paths using shared.path()
  res_path  <- shared.path(os.use = os.use, group = "RES_Data", folder = "", user.name = user.name)

  # Root location to all the shapefiles and timeseries
  poly_root <- paste0(res_path, "Shapefiles/")
  ts_root   <- paste0(res_path, "OISST/oisst_mainstays/regional_timeseries/")

  # text formatting
  region_group <- stringr::str_replace_all(region_group, " ", "_")
  region_group <- tolower(region_group)


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
    "nelme_regions"        = "_sf.shp")

  # full path prior to region names
  poly_extensions <- list(
    "gmri_sst_focal_areas" = paste0(poly_root, "gmri_sst_focal_areas/", poly_start),
    "lme"                  = paste0(poly_root, "large_marine_ecosystems/", poly_start),
    "nmfs_trawl_regions"   = paste0(poly_root, "nmfs_trawl_regions/", poly_start),
    "nelme_regions"        = paste0(poly_root, "NELME_regions/", poly_start)
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
    "nelme_regions"        = paste0(ts_root, "NELME_regions/", ts_start)
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

  }) %>% stats::setNames(region_list)

  # Return the list of locations
  return(region_paths)


}



####  OISST Regional Timeseries  ####
#' @title Access Regional Timeseries from OISSTv2 Mainstays
#'
#' @description Tool for accessing oisst regional timeseries from Box.
#' Regionally masked timeseries have been pre-processed and stored on box. Quick access
#' is available here.
#'
#' Simply specify your personal path to the oisst_mainstays folder,
#' the group of polygons your area is in, and the name of the region itself.
#'
#'
#'
#' @param oisst_path Personal path to ~Box/RES_Data/OISST/oisst_mainstays
#' @param region_family Identify the family of shapefiles that you are interested in.
#' Choices = "LME", "nmfs trawl regions", "gmri focus areas"
#' @param poly_name String Identifying the shapefile name that was used as mask.
#' Used to build file name.
#'
#' @return Time series dataframe for the selected region.
#' @export
#'
#' @examples
#' #Not run:
#'
#' # box_paths <- gmRi::research_access_paths(os.use = "unix",
#' #                                          user.name = "not applicable")
#' # agulhas <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays,
#' #                                    region_family = "lme",
#' #                                    poly_name = "agulhas current")
oisst_access_timeseries <- function(oisst_path,
                                    region_family = c("nmfs trawl regions", "lme", "gmri focus areas"),
                                    poly_name = "gulf of maine"){


  # Set up data path for sat source, originally all in okn demo data folder
  # Now all the timeseries are in RES_Data/OISST/oisst_mainstays
  source_path <- oisst_path
  if(stringr::str_sub(source_path, -1, -1) != "/") {source_path <- paste0(source_path, "/")}


  # State the group options for "region family" if user provided option doesn't match
  group_options <- c("nmfs trawl regions", "trawl regions",
                     "lme", "large marine ecosystems",
                     "gmri focus areas", "gulf of maine",
                     "epu", "ecological production units")

  # Return message with group options for the unfamiliar
  if((tolower(region_family) %in% group_options) == FALSE){
    message("Invalid region family.\nAvailable choices are:\n")
    message(paste(group_options, collapse = "\n"))
    return("invalid region choice selected.")}




  # Build file path to group folder based on region family
  timeseries_folder <-  switch(
    EXPR = tolower(region_family),
    "nmfs trawl regions"          = paste0(source_path, "regional_timeseries/nmfs_trawl_regions/"),
    "trawl regions"               = paste0(source_path, "regional_timeseries/nmfs_trawl_regions/"),
    "lme"                         = paste0(source_path, "regional_timeseries/large_marine_ecosystems/"),
    "large marine ecosystems"     = paste0(source_path, "regional_timeseries/large_marine_ecosystems/"),
    "epu"                         = paste0(source_path, "regional_timeseries/ecological_production_units/"),
    "ecological production units" = paste0(source_path, "regional_timeseries/ecological_production_units/"),
    "gmri focus areas"            = paste0(source_path, "regional_timeseries/gmri_sst_focal_areas/"),
    "gulf of maine"               = paste0(source_path, "regional_timeseries/gmri_sst_focal_areas/"))



  # Generate list of options in case people don't know what is available
  available_ts <- list.files(timeseries_folder, pattern = "csv")


  # Format the file names to match against poly_name
  available_polys <-  purrr::map_chr(available_ts, function(ts){
    ts <- stringr::str_replace(ts, ".csv", "")
    ts <- stringr::str_replace_all(ts, "_", " ")
    ts <- stringr::str_replace(ts, "OISSTv2 anom ", "")})


  # Check if the supplied polygon is in the list for that group
  if( (tolower(poly_name) %in% available_polys) == FALSE){
    message(paste0("Invalid poly_name of: ", poly_name, "\nAvailable Polygons for ",
                   region_family, " group include: \n"))
    message(paste0(available_polys, collapse = "\n"))
    return("Invalid poly_name choice selected.")
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








####  Access Global OISSTv2 Files  ####
#' @title Access OISST Mainstays Arrays
#'
#' @param oisst_path Local path to oisst folder on box, subfolder of RES_Data/OISST/oisst_mainstays
#' @param resource Name of the global extent resource, choices are raw, climatology, warming rates,
#' and anomalies.
#' @param year_range optional vector of years for raw or anommalies data resources.
#'
#' @return resource_out Raster stack of the desired netcdf array
#' @export
#'
#' @examples
#' # Not run
#' # load_global_oisst(oisst_path = "~Box/RES_Data/OISST/oisst_mainstays",
#' #                   resource = "warming rates", year_range = NULL)
load_global_oisst <- function(oisst_path = "~/Box/RES_Data/OISST/oisst_mainstays",
                              resource = c("raw", "climatology82", "climatology91", "anomalies", "warming rates"),
                              year_range = seq(2010, 2020, 1)){

  message("Function currently in development.")

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
# 3/2/2021


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
#'     message("Invalid region family.\nAvailable choices are:\n")
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
#'     #"lme"                         = paste0(source_path, "Shapefiles/large_marine_ecosystems/"),
#'     #"large marine ecosystems"     = paste0(source_path, "Shapefiles/large_marine_ecosystems/"),
#'     "epu"                         = paste0(source_path, "Shapefiles/EPU/"),
#'     "ecological production units" = paste0(source_path, "Shapefiles/EPU/"),
#'     "gmri focus areas"                  = paste0(source_path, "Shapefiles/gmri_sst_focal_areas/"),
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


