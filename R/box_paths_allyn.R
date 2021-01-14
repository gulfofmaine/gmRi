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
  # Mac Operating System Paths
  if (os.use == "unix") {

    # If group is specified use switch() to set it
    if (!is.null(group)) {
      path.out <- switch(tolower(group),
        "res_data"                   = paste("~/Box/RES_Data/", folder, sep = ""),
        "res data"                   = paste("~/Box/RES_Data/", folder, sep = ""),
        "res"                        = paste("~/Box/RES_Data/", folder, sep = ""),
        "mills lab"                  = paste("~/Box/Mills Lab/", folder, sep = ""),
        "mills"                      = paste("~/Box/Mills Lab/", folder, sep = ""),
        "climate change ecology lab" = paste("~/Box/Climate Change Ecology Lab/", folder, sep = ""),
        "nsf okn"                    = paste("~/Box/NSF OKN Demo Data/", folder, sep = ""),
        "root"                       = paste("~/Box/", folder, sep = ""))

      # Default Path is Mills Lab
      } else { path.out <- paste("~/Box/Mills Lab/Projects/", sub(".*\\/", "", getwd()), "/", sep = "")}

    # Windows Operating System Paths
    } else if (os.use == "windows") {

      # If group is specified use switch() to set it
      if (!is.null(group)) {
      path.out <- switch(tolower(group),
        "res data"                   = paste("C:/Users/", user.name, "/Box/RES_Data/", folder, sep = ""),
        "res_data"                   = paste("C:/Users/", user.name, "/Box/RES_Data/", folder, sep = ""),
        "res"                        = paste("C:/Users/", user.name, "/Box/RES_Data/", folder, sep = ""),
        "mills lab"                  = paste("C:/Users/", user.name, "/Box/Mills Lab/", folder, sep = ""),
        "mills"                      = paste("C:/Users/", user.name, "/Box/Mills Lab/", folder, sep = ""),
        "climate change ecology lab" = paste("C:/Users/", user.name, "/Box/Climate Change Ecology Lab/", folder, sep = ""),
        "nsf okn"                    = paste("C:/Users/", user.name, "/Box/NSF OKN Demo Data/", folder, sep = ""),
        "root"                       = paste("C:/Users/", user.name, "/Box/", folder, sep = "")
      )

      # Default Path is Mills Lab
      } else { path.out <- paste("C:/Users/", user.name, "/Box/Mills Lab/Projects/", sub(".*\\/", "", getwd()), "/", sep = "")}

    # Incorrecct OS Warning
    } else { print("OS not recognized")}

  # Return the Desired Paths
  return(path.out)
}
