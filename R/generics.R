#' @import utils
#' @importFrom methods new
NULL
#' An interval extension of a SpatialPixelsDataFrame
#'
#' @slot interval a matrix of two columns representing the lower and upper
#' endpoints of an interval.
#'
#' @export
intgrd <- methods::setClass("intgrd",
                            contains = "SpatialPixelsDataFrame",
                            slots = c(interval = "matrix"))

#' An interval extension of a SpatialPointsDataFrame
#'
#' @slot interval a matrix of two columns representing the lower and upper
#' endpoints of an interval.
#'
#' @export
intsp <- methods::setClass("intsp",
                  contains = "SpatialPointsDataFrame",
                  slots = c(interval = "matrix"))

#=============================================================================
# Generic to change the contents of the "intsp" interval slot.
#' Function to extract the interval of an intsp or intgrd object
#' @param x an object of class intsp or intgrd
#'
#' @name interval
#' @export
methods::setGeneric("interval",
                    function(x)
                      standardGeneric("interval"))

# Generic to inititate the contents of the "intsp" interval slot.
#' Function to set the value of the interval slot in an intsp or intgrd object
#' @param x an object of class intsp or intgrd
#' @param value either column names specifying the location of the interval
#' data or a matrix of two columns with the interval data
#'
#' @name interval<-
#' @export
methods::setGeneric("interval<-",
                    function(x, value)
                      standardGeneric("interval<-"))

# Generic to inititate the contents of the "intsp" interval slot.
#' Function to fit empirical variograms for an interval-valued spatial object
#' @param x an object of class intsp or intgrd
#'
#' @name intvariogram
#' @export
methods::setGeneric("intvariogram",
                    function(x, formulas = list(center ~ 1, radius ~ 1), ...)
                      standardGeneric("intvariogram"))
#=============================================================================
