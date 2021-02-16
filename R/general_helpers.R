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
