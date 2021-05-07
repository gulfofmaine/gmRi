DownloadBuoyCTD <- function(buoy, destfolder){
  
  numCores <- parallel::detectCores()
  doParallel::registerDoParallel(numCores-1)
  foreach(i = 1:length(buoy)) %dopar% {
    path<-"http://www.neracoos.org/erddap/tabledap/"
    file<-"_sbe37_all.nc?station%2Ctime%2Ctemperature%2Ctemperature_qc%2Csalinity%2Csalinity_qc%2Csigma_t%2Csigma_t_qc%2Clongitude%2Clatitude%2Cdepth"
    destfile <- paste(buoy[[i]], ".nc", sep="")
    filename <- paste0(here::here(), "/", destfolder, "/", destfile)
    
    download.file(paste(path, buoy[[i]], file, sep=""), destfile = filename, mode="wb")
  }
}

loadNERACOOSnetcdf <- function(filename, varname){
  
  files <- list()
  for(i in 1:length(filename)){
    
    buoy_name <- stringr::str_extract(filename[[i]], "[:alnum:]+.nc") %>% 
      stringr::str_replace(., ".nc", "")
    ncid <- ncdf4::nc_open(filename[[i]])
    
    # open the netcdf file, extract the variable, and save as a vector
    vari <- list()
    for(i in 1:length(varname)){
      name <- varname[[i]]
      V <- ncdf4::ncvar_get(ncid, varid = name)
      
      # look for any flagged values, replace with NaN
      # values >0 indicate values out of range, broken sensor, or invalid input 
      Q <- ncdf4::ncvar_get(ncid, varid=paste(name,'_qc',sep=''))
      I <- which(Q>0)
      V[I] <- NA
      
      V <- as.vector(V)
      
      # load the time values
      time <- ncdf4::ncvar_get(ncid,varid='time') %>% as.vector()
      depth <- ncdf4::ncvar_get(ncid, varid = "depth") %>% as.vector()
      
      time <- as.POSIXct(time, origin = "1970-01-01T00:00:00Z")
      
      df <- tibble::tibble("Date" = time, "value" = V, depth)
      
      vari[[name]] <- df
      
    }
    
    # close the netcdf file and combine time and variable output into a dataframe
    ncdf4::nc_close(ncid)
    
    files[[buoy_name]] <- dplyr::bind_rows(vari, .id = "variable")
    
  }
  
  V <- dplyr::bind_rows(files, .id = "buoy")
  return(V)
}

makeDailyMean <- function(df){
  # Convert decimal day to Year-month-day, calculate daily means
  df <- df %>% dplyr::mutate(day = lubridate::day(Date),
                      month = lubridate::month(Date),
                      year = lubridate::year(Date),
                      Date = as.Date(paste(year, month, day, sep = "-"))) %>% 
    dplyr::group_by(Date, depth, buoy) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Create a tibble with all of the dates to make a full time series including missing values
  start_date <- head(as.Date(df$Date), n=1)
  end_date <- tail(as.Date(df$Date), n=1)
  ts_date <- tibble::tibble(Date = seq(as_date(start_date), as_date(end_date), by = "day"))
  df <- dplyr::left_join(ts_date, df, by = "Date")  
}

surfbotcorr <- function(df){
  # extract the surface (1m) amd bottom (50m) temperature data from each buoy
  df_sansBT <- df %>% dplyr::filter(depth != 50, variable == "temperature")
  bottomT <- df %>% dplyr::filter(depth == 50, variable == "temperature")
  surfT <- df %>% dplyr::filter(depth == 1, variable == "temperature")
  
  # match rows with the same dates to make a table of 1m and 50m temperature data
  Table <- dplyr::left_join(surfT, bottomT, by = c("Date", "buoy", "variable")) %>% 
    dplyr::mutate(yr = lubridate::year(Date),
                  ord = lubridate::yday(Date))
  Table$depth.y <- 50
  
  # Linear model: 50 meter temperature dependent on 1 meter temperature grouped by ordinal day
  lm1 <-  Table %>% 
    dplyr::group_by(ord, buoy, variable) %>% 
    do(broom::tidy(lm(value.y ~ value.x, data = .))) %>% 
    dplyr::select(ord, term, estimate) %>% 
    tidyr::pivot_wider(names_from = term, values_from = estimate) %>% 
    rename("slope" = value.x,
           "intercept" = `(Intercept)`)
  
  # combine the temperature data with the slope and intercept of the lm
  Table <- dplyr::left_join(Table, lm1, by = "ord", all.x = TRUE) %>% 
    mutate(value.y = if_else(is.na(value.y), 
                             slope*value.x + intercept,
                             value.y)) %>% 
    ungroup() %>% 
    rename(value = value.y) %>% 
    select(Date, value, "depth" = depth.y) %>% 
    bind_rows(df_sansBT)
  return(Table)
} 


