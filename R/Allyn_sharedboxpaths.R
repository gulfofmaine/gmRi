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
#' #Not Run
#' #enter_user_name()
#'
enter_user_name_func<- function(){

  # Detect the operating system
  os.use <- .Platform$OS.type

  #Windows User
  if(os.use == "windows"){
    user.name <- readline(prompt= "Enter user name: ")
    return(user.name)

  # Unix/mac
  } else {

    print("MacOS user name not required")

  }

}






#' @title Shared Path
#'
#' @description This function creates paths to shared Data and Functions folders stored either inside the Mills Lab folder or outside the Mills Lab folder
#'
#' @param os.use Character string defining operating system. Either "unix" or "windows"
#' @param group Character string defining where to load shared Data and Functions. Either RES (things shared across the entire research department) or Mills Lab (things shared within the Mills Lab)
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
#'
shared.path <- function(os.use = "unix", group = c("RES Data", "Mills Lab"), folder = "Functions/"){

  if(os.use == "unix"){
    if(!is.null(group)){
      path.out<- switch(group,
                        "RES Data" = paste("~/Box/RES Data/", sep = ""),
                        "Mills Lab" = paste("~/Box/Mills Lab/", folder, sep = ""))
    } else {
      path.out<- paste("~/Box/Mills Lab/Projects/", sub('.*\\/', '', getwd()), "/", sep = "")
    }
  } else if(os.use == "windows"){
    if(!is.null(group)){
      path.out<- switch(group,
                        "RES Data" = paste("C:/Users/", user.name, "/Box/Res Data/", sep = ""),
                        "Mills Lab" = paste("C:/Users/", user.name, "/Box/Mills Lab/", folder, sep = ""))
    } else {
      path.out<- paste("C:/Users/", user.name, "/Box/Mills Lab/Projects/", sub('.*\\/', '', getwd()), "/", sep = "")
    }
  } else {
    print("OS not recognized")
  }
  return(path.out)
}



res.data.path <- shared.path(os.use = "unix", group = "RES Data", folder = "")
res.func.path <- shared.path(os.use = "unix", group = "RES Data", folder = "")
lab.data.path <- shared.path(os.use = "unix", group = "Mills Lab", folder = "Data/")
lab.func.path <- shared.path(os.use = "unix", group = "Mills Lab", folder = "Functions/")
proj.path     <- shared.path(os.use = "unix", group = NULL, folder = NULL)

cat("You did it! You have created paths to RES and Mills Lab shared Data and Functions folders and the project folder on Box.\n", paste("The paths can be called directly using res.data.path, res.func.path, lab.data.path, lab.func.path and proj.path. Go foRth and conqueR!", sep = ""))
