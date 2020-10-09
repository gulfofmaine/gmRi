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
#' @title Connect to NSF OKN Demo Data Paths
#'
#'
#' @param os.use String flag indicating what operating system the user is currently using. Options are "unix" for mac users or "windows".
#' @param user.name User name for navigating root directory structure on windows.
#'
#' @return res_paths List containing user-specific paths
#' @export
#'
#' @examples # Not Run
nsf_access_paths <- function(os.use = "unix", user.name = "not applicable I use a mac"){

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
#' @examples
okn_access_timeseries <- function(okn_path = okn_path,
                                  sat_source = "oisst",
                                  region_family = "nefsc"){

  # Path to satellite source
  source_path <- paste0(okn_path, "/", sat_source)

}



####  Access OISST NETCDF's  ####

