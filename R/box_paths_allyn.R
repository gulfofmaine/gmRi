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






#' @title Access Shared Path
#'
#' @description This function creates paths to shared Data and Functions folders stored either inside the Mills Lab folder or outside the Mills Lab folder
#'
#' @param os.use Character string defining operating system. Either "unix" or "windows"
#' @param group Character string defining where to load shared Data and Functions.
#' Either RES (things shared across the entire research department) or
#' Mills Lab (things shared within the Mills Lab). Additional shortcuts are NSF OKN
#' for knowledge graph data resources and root for the minimal path to box.
#' @param folder Sub-folder within box you wish to access
#'
#' @return path.out Path to shared folder. We will then use these paths within shared code to load shared data or source shared functions.
#' @export
#'
#' @examples
#' # res.data.path<- shared.path(os.use = os.use, group = "RES", folder = "Data/")
#'
#' # Not Run
#' # gom.shapefile<- st_read(paste(res.data.path, "Shapefiles/GoM_sf.shp", sep = ""))
shared.path <- function(os.use = "unix",
                        group = c("RES Data", "Mills Lab", "Climate Change Ecology Lab", "NSF OKN", "root"),
                        folder = "Functions/") {
  if (os.use == "unix") {
    if (!is.null(group)) {
      path.out <- switch(group,
        "RES Data"                   = paste("~/Box/RES Data/", sep = ""),
        "Mills Lab"                  = paste("~/Box/Mills Lab/", folder, sep = ""),
        "Climate Change Ecology Lab" = paste("~/Box/Climate Change Ecology Lab/", folder, sep = ""),
        "NSF OKN"                    = paste("~/Box/NSF OKN Demo Data/", folder, sep = ""),
        "root"                       = paste("~/Box/", folder, sep = "")
      )
    } else {
      path.out <- paste("~/Box/Mills Lab/Projects/", sub(".*\\/", "", getwd()), "/", sep = "")
    }
  } else if (os.use == "windows") {
    if (!is.null(group)) {
      path.out <- switch(group,
        "RES Data"                   = paste("C:/Users/", user.name, "/Box/Res Data/", sep = ""),
        "Mills Lab"                  = paste("C:/Users/", user.name, "/Box/Mills Lab/", folder, sep = ""),
        "Climate Change Ecology Lab" = paste("C:/Users/", user.name, "/Box/Climate Change Ecology Lab/", folder, sep = ""),
        "NSF OKN"                    = paste("C:/Users/", user.name, "/Box/NSF OKN Demo Data/", folder, sep = ""),
        "root"                       = paste("C:/Users/", user.name, "/Box/", folder, sep = "")
      )
    } else {
      path.out <- paste("C:/Users/", user.name, "/Box/Mills Lab/Projects/", sub(".*\\/", "", getwd()), "/", sep = "")
    }
  } else {
    print("OS not recognized")
  }



  return(path.out)
}
