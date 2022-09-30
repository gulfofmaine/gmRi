# Functions to automatically generate paths to shared data and functions stored on Box


#' @title Check & Enter User Name
#'
#' @description Prompts user name entry for determining box location on local machine.
#' (needs final check for "windows" machine users). Mac users do not need this step.
#'
#' @return user.name User name
#' @export
#'
#' @examples
#' # Not Run
#' # enter_user_name()
enter_user_name_func <- function() {

  # Detect the operating system
  os.use <- .Platform$OS.type

  # Windows User
  if (os.use == "windows") {
    user.name <- readline(prompt = "Enter user name: ")
    return(user.name)

    # Unix/mac
  } else {
    print("MacOS user name not required")
  }
}






#' @title Access Shared Path from Box Drive for Mojave Users
#'
#' @description This function creates paths to shared Data and Functions folders stored either inside the Mills
#' Lab folder or outside the Mills Lab folder in other top level locations on box.
#'
#' @param os.use Character string defining operating system. Either "unix"/"mac" or "windows"
#' @param group Character string of top-level directory within box to open. Ex. Box/RES_Data
#' Either RES (things shared across the entire research department) or
#' Mills Lab (things shared within the Mills Lab). Additional shortcuts are NSF OKN
#' for knowledge graph data resources and root for the minimal path to box.
#' @param folder Sub-folder within box you wish to access
#' @param user.name User name needed to set root directory for Windows users
#'
#' @return path.out Path to shared folder. We will then use these paths within shared code to load shared data or source shared functions.
#' @export
#'
#' @examples
#' # res.data.path<- shared.path(os.use = os.use, group = "RES", folder = "Data/")
#'
#' # Not Run
#' # gom.shapefile<- st_read(paste0(res.data.path, "Shapefiles/GoM_sf.shp"))
shared.path <- function(os.use = "unix",
                        group = c("RES Data", "Mills Lab", "Climate Change Ecology Lab", "NSF OKN", "root"),
                        folder = "Functions/",
                        user.name = NULL) {
  # Mac Operating System Paths
  if (os.use %in% c("unix", "mac")) {

    # If group is specified use switch() to set it
    if (!is.null(group)) {
      path.out <- switch(tolower(group),
        "res_data"                   = paste0("~/Box/RES_Data/", folder),
        "res data"                   = paste0("~/Box/RES_Data/", folder),
        "res"                        = paste0("~/Box/RES_Data/", folder),
        "mills lab"                  = paste0("~/Box/Mills Lab/", folder),
        "mills"                      = paste0("~/Box/Mills Lab/", folder),
        "climate change ecology lab" = paste0("~/Box/Climate Change Ecology Lab/", folder),
        "nsf okn"                    = paste0("~/Box/NSF OKN Demo Data/", folder),
        "root"                       = paste0("~/Box/", folder))

      # Default Path is Mills Lab
      } else { path.out <- paste0("~/Box/Mills Lab/Projects/", sub(".*\\/", "", getwd()), "/")}

    # Windows Operating System Paths
    } else if (os.use == "windows") {

      # If group is specified use switch() to set it
      if (!is.null(group)) {
      path.out <- switch(tolower(group),
        "res data"                   = paste0("C:/Users/", user.name, "/Box/RES_Data/",  folder),
        "res_data"                   = paste0("C:/Users/", user.name, "/Box/RES_Data/",  folder),
        "res"                        = paste0("C:/Users/", user.name, "/Box/RES_Data/",  folder),
        "mills lab"                  = paste0("C:/Users/", user.name, "/Box/Mills Lab/", folder),
        "mills"                      = paste0("C:/Users/", user.name, "/Box/Mills Lab/", folder),
        "climate change ecology lab" = paste0("C:/Users/", user.name, "/Box/Climate Change Ecology Lab/", folder),
        "nsf okn"                    = paste0("C:/Users/", user.name, "/Box/NSF OKN Demo Data/", folder),
        "root"                       = paste0("C:/Users/", user.name, "/Box/", folder)
      )

      # Default Path is Mills Lab
      } else { path.out <- paste0("C:/Users/", user.name, "/Box/Mills Lab/Projects/", sub(".*\\/", "", getwd()), "/")}

    # Incorrect OS Warning
    } else { print("OS not recognized")}

  # Return the Desired Paths, ensure it ends with /
  if(stringr::str_sub(path.out, -1, -1) != "/") {path.out <- paste0(path.out, "/")}
  return(path.out)
}



#' @title Box Path Generator
#'
#' @description Create OS agnostic paths to different locations on box.
#'
#' @param box_group The top level directory name within Box
#' @param subfolder Any subsequent sub-directory location you wish to access
#'
#' @return returns a path to box
#' @export
#'
#' @examples
#' oisst_path <- box_path(box_group = "res", subfolder = "OISST/oisst_mainstays")
#' shapefile_path <- box_path(box_group = "res", subfolder = "Shapefiles")
box_path <- function(box_group = NULL, subfolder = NULL){

  # Base root to box
  box_root <- paste0(normalizePath("~/"), "/Box/")

  # check if box group is valid:
  group_valid <- tolower(box_group) %in% c("res", "res_data", "res data", "mills", "mills lab", "mills_lab", "ccel", "nsf okn", "okn", "root")
  if(group_valid == FALSE){
    stop("box_group not accepted. Please select from c(res, mills, ccel, okn, or root)")
  }

  # If group is specified use switch() to set it
  if (!is.null(box_group)) {
    box_root <- switch(tolower(box_group),
                       "res data"                   = paste0(box_root, "RES_Data/"),
                       "res_data"                   = paste0(box_root, "RES_Data/"),
                       "res"                        = paste0(box_root, "RES_Data/"),
                       "mills lab"                  = paste0(box_root, "Mills Lab/"),
                       "mills"                      = paste0(box_root, "Mills Lab/"),
                       "climate change ecology lab" = paste0(box_root, "Climate Change Ecology Lab/"),
                       "ccel"                       = paste0(box_root, "Climate Change Ecology Lab/"),
                       "nsf okn"                    = paste0(box_root, "NSF OKN Demo Data/"),
                       "okn"                        = paste0(box_root, "NSF OKN Demo Data/"),
                       "root"                       = paste0(box_root, "") )
  }

  # If sub-directories are provided append those on:
  if (!is.null(subfolder)) {
    box_root <- paste0(box_root, subfolder)
  }

  # Return the Desired Path, ensure it ends with /
  if(stringr::str_sub(box_root, -1, -1) != "/") {box_root <- paste0(box_root, "/")}
  return(box_root)

}



#' @title CloudStorage Box Path Generator
#'
#' @description Create Mac OS path to Box when Box is mounted under "CloudStorage". When box is
#' installed under the cloudstorage path its root directory is different from the default location
#' used in gmRi::box_path. These function is a direct swap for that situation.
#'
#' @param box_group The top level directory name within Box
#' @param subfolder Any subsequent sub-directory location you wish to access
#'
#' @return returns a path to box
#' @export
#'
#' @examples
#' oisst_path <- cs_path(box_group = "res", subfolder = "OISST/oisst_mainstays")
#' shapefile_path <- cs_path(box_group = "res", subfolder = "Shapefiles")
cs_path <- function(box_group = NULL, subfolder = NULL){

  # Base root to box
  box_root <- paste0(normalizePath("~/"), "/Library/CloudStorage/Box-Box/")

  # check if box group is valid:
  group_valid <- tolower(box_group) %in% c("res", "res_data", "res data", "mills", "mills lab", "mills_lab", "ccel", "nsf okn", "okn", "root")
  if(group_valid == FALSE){
    stop("box_group not accepted. Please select from c(res, mills, ccel, okn, or root)")
  }

  # If group is specified use switch() to set it
  if (!is.null(box_group)) {
    box_root <- switch(tolower(box_group),
                       "res data"                   = paste0(box_root, "RES_Data/"),
                       "res_data"                   = paste0(box_root, "RES_Data/"),
                       "res"                        = paste0(box_root, "RES_Data/"),
                       "mills lab"                  = paste0(box_root, "Mills Lab/"),
                       "mills"                      = paste0(box_root, "Mills Lab/"),
                       "climate change ecology lab" = paste0(box_root, "Climate Change Ecology Lab/"),
                       "ccel"                       = paste0(box_root, "Climate Change Ecology Lab/"),
                       "nsf okn"                    = paste0(box_root, "NSF OKN Demo Data/"),
                       "okn"                        = paste0(box_root, "NSF OKN Demo Data/"),
                       "root"                       = paste0(box_root, "") )
  }

  # If sub-directories are provided append those on:
  if (!is.null(subfolder)) {
    box_root <- paste0(box_root, subfolder)
  }

  # Return the Desired Path, ensure it ends with /
  if(stringr::str_sub(box_root, -1, -1) != "/") {box_root <- paste0(box_root, "/")}
  return(box_root)

}



#' @title Box Path Switcher for Mac Storage Location
#'
#' @description Toggles the root location of Box cloud storage based on whether a box was mounted
#' under libraries or under cloudstorage
#'
#' @param box_location String where Mac OS system mounted path to box. Accepts "cloudstorage" &
#' "mojave" to be backwards compatible with code that mistook this for a mojave issue,
#' all other values will use `box_path` and not `cs_path`
#'
#' @return Returns path function based on operating system
#' @export
#'
#' @examples
#' boxpath_fun <- boxpath_switch(box_location = "cloudstorage")
boxpath_switch <- function(box_location = "base"){

  # Toggle function to use based on how Box is mounted on computer
  path_fun <- ifelse(tolower(box_location) %in% c("cloudstorage", "mojave"),
                     gmRi::cs_path,
                     gmRi::box_path)
  return(path_fun)

}
