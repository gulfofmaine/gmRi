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
#' @param res_path Personal path to ~Box/RES_Data/OISST/oisst_mainstays
#' @param region_family Identify the family of shapefiles that you are interested in.
#' Choices = "LME", "Groundfish Regions"
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
#' # agulhas <- oisst_access_timeseries(res_path = box_paths$oisst_mainstays,
#' #                                    region_family = "lme",
#' #                                    poly_name = "agulhas current")
oisst_access_timeseries <- function(res_path,
                                    region_family = c("nmfs trawl regions", "lme", "gmri focus areas"),
                                    poly_name = "gulf of maine"){

  # Set up data path for sat source, originally all in okn demo data folder
  # Now all the timeseries are in RES_Data/OISST/oisst_mainstays
  source_path <- res_path
  if(stringr::str_sub(source_path, -1, -1) != "/") {source_path <- paste0(source_path, "/")}


  # State the group options for "region family" if user provided option doesn't match
  group_options <- c("nmfs trawl regions", "trawl regions",
                     "lme", "large marine ecosystems",
                     "gmri focus", "gulf of maine",
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
    "gmri focus"                  = paste0(source_path, "regional_timeseries/gmri_focus_areas/"),
    "gulf of maine"               = paste0(source_path, "regional_timeseries/gmri_focus_areas/"))



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


# # testing
# box_paths <- gmRi::research_access_paths(os.use = "unix",
#                                          user.name = "not applicable")
#
#
# # testing an incorrect group name
# oisst_access_timeseries(res_path = box_paths$oisst_mainstays,
#                         region_family = "bad group",
#                         poly_name = "incorrect polygon")
#
#
# # testing an invalid polygon name
# oisst_access_timeseries(res_path = box_paths$oisst_mainstays,
#                         region_family = "trawl regions",
#                         poly_name = "incorrect polygon")
#
# # testing a valid polygon name
# oisst_access_timeseries(res_path = box_paths$oisst_mainstays,
#                         region_family = "lme",
#                         poly_name = "agulhas current")


####  Access OISST NETCDF's  ####
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
#' # Nor run
#' # load_global_oisst(oisst_path = "~Box/RES_Data/OISST/oisst_mainstays",
#' #                   resource = "warming rates", year_range = NULL)
load_global_oisst <- function(oisst_path = "~Box/RES_Data/OISST/oisst_mainstays",
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





