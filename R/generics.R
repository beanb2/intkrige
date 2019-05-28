# The following link saved my bacon when trying to document s4 methods:
# - https://github.com/variani/pckdev/wiki/Documenting-with-roxygen2#s4-methods
#' @import utils
#' @importFrom methods new
NULL
#' An interval extension of a SpatialPixelsDataFrame
#'
#' @slot interval a matrix of two columns representing the lower and upper
#'  endpoints of an interval.
#'
#' @export
intgrd <- methods::setClass("intgrd",
                            contains = "SpatialPixelsDataFrame",
                            slots = c(interval = "matrix"))

#' An interval extension of a SpatialPointsDataFrame
#'
#' @slot interval a matrix of two columns representing the lower and upper
#'  endpoints of an interval.
#'
#' @export
intsp <- methods::setClass("intsp",
                  contains = "SpatialPointsDataFrame",
                  slots = c(interval = "matrix"))

#=============================================================================
#' Function to extract the interval of an intsp or intgrd object
#' @param x an object of class intsp or intgrd
#' @return A matrix containing the interval data
#' @name interval
#' @rdname interval-methods
#' @exportMethod interval
methods::setGeneric("interval",
                    function(x)
                      standardGeneric("interval"))

#' Function to reassign the contents of the interval slot
#' @param x an object of class intsp or intgrd
#' @return either a character vector of length two specifying the column names
#'  which will occupy the interval slot. Or a matrix of two columns to fill
#'  the slot.
#' @name interval<-
#' @rdname interval-methods-assign
#' @exportMethod interval<-
methods::setGeneric("interval<-",
                    function(x, value)
                      standardGeneric("interval<-"))

# Generic to inititate the contents of the "intsp" interval slot.

#' Function to fit empirical variograms for an interval-valued spatial object
#'
#' @param x an object of class intsp or intgrd
#' @param formulas a list of length two specifying the formulas related to the
#' centers and radii respectively.
#' @param ... additional arguments for sp::variogram()
#' @return an object of class \'intvariogram\' containing empi
#' rcal variograms
#' for the center, radius, and center/radius interaction.
#' @name intvariogram
#' @rdname intvariogram-methods
#' @exportMethod intvariogram
methods::setGeneric("intvariogram",
                    function(x, formulas = list(center ~ 1, radius ~ 1), ...)
                      standardGeneric("intvariogram"))
#=============================================================================

#=============================================================================
# Documentation for generic functions.
#' Atomic subsetting of interval spatial objects.
#'
#' @param x an object of class \'intsp\' or class \'intgrd\'
#' @param name the variable name on which to subset. This variable name can be
#' located in the data, coordinates, or interval slot.
#' @return the value contained in the requested subset
#' @name $
#' @rdname extract-methods
NULL

#' Atomic assignment of interval spatial objects.
#'
#' @param x an object of class \'intsp\' or class \'intgrd\'
#' @param name the variable name on which to subset. This variable name can be
#' located in the data, coordinates, or interval slot.
#' @name $<-
#' @rdname assign-methods
NULL

#' Convert intgrd or intsp object back to a data frame
#'
#' @param x an object of class \'intsp\' or class \'intgrd\'
#' @return an object of class 'data.frame'
#' @name as.data.frame
#' @rdname interval.as.data.frame-methods
NULL

#' Create an interval plot for spatial points.
#'
#' Calls the sp::spplot() function to plot the locations, centers, and
#' radii of an interval-valued spatial data frame in a single figure.
#'
#' @param x an object of class intsp or intgrd
#' @param beside (intgrd only) if true, center and radius plotted side by side
#'  if false, center and radius are plotted in a single figure with the center
#'  plotted using color and the radius plotted using circles circumsribed
#'  within each grid cell.
#' @param circleCol (intgrdonly) if beside=TRUE, the color of the circles
#'  that will be circumscribed within each grid cell
#' @param locationsOnly a boolean: TRUE calls plot.sp(x)
#' @param legend.positions the positions of the center and radius legend
#' relative to the plotting window
#' @param cuts the number of ranges of values to print in the center and radius
#' legend respectively
#' @param radSize a vector of length 2 indicating the range of point sizes to
#' plot to visualize radii magnitudes
#' @param pch the shape of the points (see plot())
#' @param alpha the transparency of the points
#' @param ... additional arguments to sp::spplot()
#'
#' @name plot
#' @rdname plot.interval-methods
NULL

