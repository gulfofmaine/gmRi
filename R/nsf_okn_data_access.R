####
####  Accessing NSF OKN Processed Data  ####
####
#### Many commonly used forms of rocessed OISST and ERSST data were used
#### Things like anomaly timeseries related to important regions like GOM, GB, or even LME's
####
#### Usage may depend on the research objective, so functionality to return
#### a netcdf/raster object or a table may be desired
####
#### Data is stored on ~/Box/NSF OKN Demo Data/
#### Acccess to this folder required for these paths to work.





####  NSF OKN Paths  ####
####  This will change for each user, but will be foundational to all these functions
#' @title Connect to the NSF OKN, RES, and Mills Lab Folders
#'
#'
#' @description Returns list of box resource paths for quick access.
#'
#'
#' @param os.use String flag indicating what operating system the user is currently using. Options are "unix" for mac users or "windows".
#' @param user.name User name for navigating root directory structure on windows.
#'
#' @return res_paths List containing user-specific paths
#' @export
#'
#' @examples box_paths <- research_access_paths(os.use = "unix", user.name = "not applicable I use a mac")
research_access_paths <- function(os.use = "unix", user.name = "not applicable I use a mac"){

  # Pre-load a user name for windows
  user.name = user.name

  # Path to NSF OKN Demo Data
  okn_path <- shared.path(os.use = os.use, group = "NSF OKN", folder = "")

  # Path to Research Team Data
  res_path   <- shared.path(os.use = os.use, group = "RES Data", folder = NULL)

  # Path to Kathy Mills Research Lab
  mills_path <- shared.path(os.use = os.use, group = "Mills Lab", folder = "")


  # group them all together for export
  res_paths <- list(okn = okn_path,
                    res = res_path,
                    mills = mills_path)
  return(res_paths)

}




####  Access Regional Timeseries, OISST  ####
#' @title Access Regional Timeseries from OKN Demo Data
#'
#' @description Tool for accessing NSF OKN Demo Data Products from Box. Global time series,
#' as well as regionally masked timeseries have been pre-processed and stored on box. Wuick access
#' is available here.
#'
#' Simply name the satellite data source, the desired extent or mask, and the function will return the corresponding table.
#'
#'
#' @param okn_path Personal path to ~Box/NSF OKN Demo Data
#' @param sat_source String indicating satellite data source.
#' @param region_family Identify the family of shapefiles that you are interested in. Choices = "LME", "Groundfish Regions"
#'
#' @return
#' @export
#'
#' @examples box_paths <- gmRi::research_access_paths(os.use = "unix", user.name = "not applicable I use a mac")
#' @examples agulhas_timeseries <- okn_access_timeseries(okn_path = box_paths$okn, sat_source = "oisst", region_family = "lme", poly_name = "agulhas current")
okn_access_timeseries <- function(okn_path,
                                  sat_source = "oisst",
                                  region_family = c("nmfs trawl regions", "lme", "gmri focus areas"),
                                  poly_name = "gulf of maine"){

  # get okn data path
  okn_path <- okn_path


  # Path to satellite source
  source_path <- paste0(okn_path, sat_source, "/")


  # Folder for the region group, path to polygon
  poly_name <- tolower(poly_name)
  poly_name <- stringr::str_replace_all(poly_name, " ", "_")
  poly_name <- paste0("OISSTv2_anom_", poly_name, ".csv")
  timeseries_path <-  switch(tolower(region_family),
                        "nmfs trawl regions"          = paste0(source_path, "nmfs_trawl_regions/", poly_name),
                        "trawl regions"               = paste0(source_path, "nmfs_trawl_regions/", poly_name),
                        "lme"                         = paste0(source_path, "large_marine_ecosystems/", poly_name),
                        "large marine ecosystems"     = paste0(source_path, "large_marine_ecosystems/", poly_name),
                        "epu"                         = paste0(source_path, "ecological_production_units/", poly_name),
                        "ecological production units" = paste0(source_path, "ecological_production_units/", poly_name),
                        "gmri focus"                  = paste0(source_path, "gmri_focus_areas/", poly_name),
                        "gulf of maine"               = paste0(source_path, "gmri_focus_areas/", poly_name)
                        )


  return(timeseries_path)


  # Read Timeseries
  timeseries_out <- read.csv(timeseries_path)
  return(timeseries_out)


}



####  Access OISST NETCDF's  ####
#' @title Access OISST Mainstays Arrays
#'
#' @param oisst_path Local path to oisst folder on box, subfolder of NSF OKN Demo Data
#' @param resource Name of the global extent resource, choices are raw, climatology, warming rates, and anomalies.
#' @param year_range optional vector of years for raw or anommalies data resources.
#'
#' @return resource_out Raster stack of the desired netcdf array
#' @export
#'
#' @examples #not run load_global_oisst(oisst_path = "~Box/NSF OKN Demo Data/oisst", resource = "warming rates", year_range = NULL)
load_global_oisst <- function(oisst_path = "~Box/NSF OKN Demo Data/oisst",
                              resource = c("raw", "climatology", "anomalies", "warming rates"),
                              year_range = seq(2010, 2020, 1)){

  message("Function currently in development.")

  # Redirect to resource folder
  resource <- tolower(resource)
  resource_folder <- switch(
    resource,
    "raw"           = paste0(oisst_path, "/annual_observations/"),
    "climatology"   = paste0(oisst_path, "/daily_climatologies/daily_clims_82to2011.nc"),
    "anomalies"     = paste0(oisst_path, "/annual_anomalies/"),
    "warming rates" = paste0(oisst_path, "/warming_rates/annual_warming_rates.nc"))


  # generate list for the years
  if(resource %in% c("climatology", "warming rates")){
    resource_out <- raster::stack(resource_folder)

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


  # Return the raster stack of the array(s)
  return(resource_out)

}





