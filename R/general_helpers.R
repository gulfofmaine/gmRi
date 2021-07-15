#### General Use Functions  ####

#' @title Floor Decade
#'
#' @description Returns the decade for vector of years.
#'
#' @param year_vector Vector of years.
#' @param return_class Optional flag to return vector formatted as a factor, alternative is numeric.
#'
#' @return returns vector of values representing the decade each year occurred in.
#' @export
#'
#' @examples # not run
floor_decade <- function(year_vector, return_class = "factor"){

  if(class(year_vector) == "numeric") {
    decade_vector <- year_vector - year_vector %% 10
  }

  if(class(year_vector) %in% c("factor", "character")) {
    year_vector <- as.numeric(as.character(year_vector))
    decade_vector <- year_vector - year_vector %% 10
  }

  if(return_class == "factor") {
    decade_vector <- factor(decade_vector)

  }

  return(decade_vector)
}


#' @title Not In
#'
#' @description Negation of the `%in%` function.
#'
#' @param x vector that you wish to check for matches
#' @param table Table or vector containing strings that you wish to check x against.
#'
#' @return Boolean indication of whether x is not in table
#' @export
#'
#' @examples # not run
`%not in%` <- function(x, table){ is.na(match(x, table, nomatch = NA_integer_))}



#' @title Convert to Fahrenheit from Celsius
#'
#' @description Convert temperature in Celsius to Fahrenheit.
#' Toggle for anomaly data (does not add 32).
#'
#' @param temp_c Values of temperature or anomalies in Deg C
#' @param data_type T/F Toggle to convert temperatures or anomalies
#'
#' @return
#' @export
#'
#' @examples as_fahrenheit(c(15,20), data_type = "temperature")
as_fahrenheit <- function(temp_c, data_type = "temperature"){

  temp_convers <- function(temp_c){(temp_c * 9/5) + 32}
  anom_convers <- function(temp_c) {(temp_c * 9/5)}

  # Switch between anomaly or true temperature mode
  conv_fun <- switch (
    data_type,
    "temperature" = temp_convers,
    "anomalies" = anom_convers
  )

  # Do conversion
  temp_f <- conv_fun(temp_c)
  return(temp_f)
}




#' @title Convert Daily Stack to Monthly Means
#'
#'
#' @description Takes a stack of daily data with names of "XYYYY.MM.DD" and
#' returns monthly averages.
#'
#' @param daily_ras Raster stack with daily time step
#'
#' @return
#' @export
#'
#' @examples # not run
make_monthly <- function(daily_ras){

  # Months to subset with
  month_key <- str_pad(c(1:12), 2, "left", 0) %>% setNames(month.abb)

  # names to match index to
  layer_index <- names(daily_ras)
  month_index <- str_sub(layer_index, 7, 8)

  # Pull distinct months
  months_present <- unique(month_index)
  month_key <- month_key[which(month_key %in% months_present)]

  # Pull the indices that match, take means
  map(month_key, function(x){

    # Pull days in month
    days_in_month <- which(month_index == x)

    # Take mean of those days
    month_avg <- mean(daily_ras[[days_in_month]])
  }) %>%
    setNames(names(month_key))

}
