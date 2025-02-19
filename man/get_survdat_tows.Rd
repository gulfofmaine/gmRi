% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nefsc_groundfish_access.R
\name{get_survdat_tows}
\alias{get_survdat_tows}
\title{Get unique tows from cleaned SURVDAT trawl data}
\usage{
get_survdat_tows(survdat_clean)
}
\arguments{
\item{survdat_clean}{Survdat data, after usual preparations are completed.
These include removal of old strata, labeling of areas of interest, and inclusion
of the annual effort in each.}
}
\value{
survdat trawl dataframe containing one row for every unique tow
}
\description{
extract the unique tows (lat, long, time) from the cleaned SURVDAT traw data, which can then be enhanced with other environmental covariates (e.g., OISST).
}
\examples{
# not run
# get_survdat_tows(survdat_clean = survdat_clean)
}
