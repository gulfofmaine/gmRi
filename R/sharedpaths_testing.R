#####
## A short piece of code with functions to automatically generate paths to shared data and functions
####

# Detect the operating system
os.use<- .Platform$OS.type

# Set path to shared folders (need someone to check this for "windows" option)
enter_user_name_func<- function(){
  user.name<- readline(prompt= "Enter user name: ")
  return(user.name)
}

if(os.use == "windows"){
  user.name<- enter_user_name_func()
} else {
  print("MacOS user name not required")
}

shared.path<- function(os.use = os.use, group = c("RES Data", "Mills Lab"), folder = "Functions/"){
  # Details: This function creates paths to shared Data and Functions folders stored either inside the Mills Lab folder or outside the Mills Lab folder

  # Args:
  # os.use = Character string defining operating system. Either "unix" or "windows"
  # group = Character string defining where to load shared Data and Functions. Either RES (things shared across the entire research department) or Mills Lab (things shared within the Mills Lab)
  # folder = Character string defining the folder type. Either "Data/" or "Functions/" Note that it must include the last forward slash!

  # Returns: Path to shared folder. We will then use these paths within shared code to load shared data or source shared functions. For example:
  # res.data.path<- shared.path(os.use = os.use, group = "RES", folder = "Data/")
  # gom.shapefile<- st_read(paste(res.data.path, "Shapefiles/GoM_sf.shp", sep = ""))

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



res.data.path<- shared.path(os.use = os.use, group = "RES Data", folder = "")
res.func.path<- shared.path(os.use = os.use, group = "RES Data", folder = "")
lab.data.path<- shared.path(os.use = os.use, group = "Mills Lab", folder = "Data/")
lab.func.path<- shared.path(os.use = os.use, group = "Mills Lab", folder = "Functions/")
proj.path<- shared.path(os.use = os.use, group = NULL, folder = NULL)

cat("You did it! You have created paths to RES and Mills Lab shared Data and Functions folders and the project folder on Box.\n", paste("The paths can be called directly using res.data.path, res.func.path, lab.data.path, lab.func.path and proj.path. Go foRth and conqueR!", sep = ""))
