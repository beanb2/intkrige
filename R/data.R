#'An interval-valued design ground snow load dataset for Utah.
#'
#' A dataset containing the interval-valued data used in the analysis
#' of \insertRef{Bean2019-int}{intkrige}. The 415 measurement locations
#' included in the dataset are taken from the 2018 Utah Snow Load
#' Report \insertRef{Bean2018-report}{intkrige}.
#'
#' @format A data frame with 415 rows and 10 variables:
#'
#'  \describe{
#'  \item{STATION}{The global historical climatological network (GHCN)
#'  station identifier}
#'  \item{STATION_NAME}{The GHCN station name}
#'  \item{LATITUDE}{Latitude coordinate position}
#'  \item{LONGITUDE}{Longitude coordinate position}
#'  \item{ELEVATION}{Elevation of the measurement location (meters)}
#'  \item{minDL}{The lower endpoint of the interval-valued design snow load as measured in kilopascals (kpa)}
#'  \item{maxDL}{The upper endpoint of the design snow load interval}
#'  \item{pointDL}{The original point-valued design snow load as found in the 2018 Utah Snow Load Study}
#'
#'  }
#'
"utsnow"
