### CREATE CLASSES ###
#=============================================================================
# This is the "intgrd" class, which extends the "SpatialPixelsDataFrame"
# class. All we do is add a slot for intervals that can be called by plot,
# print, and summary functions.
#' @import sp
#' @import gstat
#' @include generics.R
NULL
#=============================================================================

### CREATE METHODS FOR GENERICS ###
#=============================================================================
# This method ensures that the "$" operator looks through the data,
# coordinate, and interval slots when looking for a subset.
# Adapted from:
# - https://github.com/edzer/sp/blob/master/R/SpatialPoints-methods.R

#' @name $
#' @rdname extract-methods
#' @aliases $,intgrd-method
methods::setMethod("$", "intgrd",
                   function(x, name) {
                     if (name %in% coordnames(x))
                       return(x@coords[,name])
                     if (name %in% colnames(x@interval))
                       return(x@interval[, name])
                     if (!("data" %in% methods::slotNames(x)))
                       stop("no $ method for object without attributes")
                     x@data[[name]]
                   }
)

# This method stops users from editing the coordinates or intervals
# using the standard "$" operator.
# Adapted from:
# - https://github.com/edzer/sp/blob/master/R/Spatial-methods.R

#' @name $<-
#' @rdname assign-methods
#' @aliases $<-,intgrd-method
methods::setMethod("$<-", "intgrd",
                   function(x, name, value) {
                     if (name %in% coordnames(x))
                       stop(paste(name,
                                  "is a coordinate name, please choose another name"))
                     # Addition for coordinate slot.
                     if (name %in% colnames(x@interval))
                       stop(paste(name, "is currently assigned to the interval slot,
                         please choose another name"))
                     if (!("data" %in% methods::slotNames(x))) {
                       df = list(value); names(df) = name
                       return(addAttrToGeom(x, data.frame(df), match.ID = FALSE))
                       # stop("no $<- method for object without attributes")
                     }
                     #if (is.list(value))
                     #	warning("assigning list or data.frame to attribute vector")
                     x@data[[name]] = value
                     x
                   }
)

#' @name interval
#' @rdname interval-methods
#' @aliases interval,intgrd-method
methods::setMethod("interval", "intgrd", function(x) {
  return(x@interval)
})

#' @name interval<-
#' @rdname interval-methods-assign
#' @aliases interval<-,intgrd-method
methods::setMethod("interval<-", "intgrd",
                   function(x, value) {

                     # A null or na value will cause the intgrd object to revert to
                     # its parent class.
                     if(is.null(value) || is.na(value)){
                       # If the interval slot is not empty, return these values
                       # to the data frame.
                       if (nrow(x@interval) > 0) {
                         x@data <- cbind(x@data, as.data.frame(x@interval))
                       }

                       x <- methods::as(x, "SpatialPixelsDataFrame")

                       return(x)
                     }

                     # If a character string is provided, then search the data frame
                     # for the values to place in the interval slot.
                     if(class(value) == "character"){
                       if(length(character) != 2){
                         stop("exactly two variable names are needed
                     to define an interval")
                       }
                       # Ensure that only numeric variables are placed in the interval
                       if (class(x@data[, value[1]]) != "numeric" ||
                           class(x@data[, value[1]]) != "numeric") {
                         stop("non-numeric input detected")
                       }

                       # If the interval slot is not empty, return these values
                       # to the data frame.
                       if (nrow(x@interval) > 0) {
                         x@data <- cbind(x@data, as.data.frame(x@interval))
                       }

                       # Fill the interval slot with the requested columns.
                       x@interval <-
                         matrix(c(x@data[, value[1]], x@data[, value[2]]), ncol = 2)

                       # Remove the variables that are now in the interval slot from the
                       # data frame.
                       x@data[, value[1]] <- NULL
                       x@data[, value[2]] <- NULL

                       # Preserve the column names in the interval slot.
                       colnames(x@interval) <- c(value[1], value[2])

                       return(x)
                       # If a matrix is input, allow the interval values to be replaced.
                     }else if(class(value) == "matrix"){
                       # Ensure that the number of rows in the replacement
                       # matches the number of rows expected.
                       if(nrow(value) != nrow(coordinates(x@coords)) ||
                          ncol(value) != ncol(coordinates(x@coords))){
                         stop("matrix dimensions must match the slot dimensions")
                       }

                       # Replace the values in the interval slot.
                       x@interval <- value

                       return(x)
                     }else{
                       stop("\'value\' must be a vector of column names or a matrix")
                     }
                   })

#' @name interval<-
#' @rdname interval-methods-assign
#' @aliases interval<-,SpatialPixelsDataFrame-method
methods::setMethod("interval<-", "SpatialPixelsDataFrame",
                   function(x, value) {

                     if(is.null(value) || is.na(value)){
                       warning("No interval provided, interval not specified.")
                       return(x)
                     }

                     x <- methods::as(x, "intgrd")

                     if(class(value) == "character"){
                       # Ensure that only numeric variables are placed in the interval
                       if (class(x@data[, value[1]]) != "numeric" ||
                           class(x@data[, value[1]]) != "numeric") {
                         stop("non-numeric input detected")
                       }

                       # Fill the interval slot with the requested columns.
                       x@interval <-
                         matrix(c(x@data[, value[1]], x@data[, value[2]]), ncol = 2)

                       # Remove the variables that are now in the interval slot from the
                       # data frame.
                       x@data[, value[1]] <- NULL
                       x@data[, value[2]] <- NULL

                       # Preserve the column names in the interval slot.
                       colnames(x@interval) <- c(value[1], value[2])

                       return(x)
                     }else if(class(value) == "matrix"){
                       # Ensure that the number of rows in the replacement
                       # matches the number of rows expected.
                       if(nrow(value) != nrow(coordinates(x@coords)) ||
                          ncol(value) != ncol(coordinates(x@coords))){
                         stop("matrix dimensions must match the slot dimensions")
                       }

                       # Replace the values in the interval slot.
                       x@interval <- value

                       return(x)
                     }else{
                       stop("\'value\' must be a vector of column names or a matrix")
                     }
                   })

# This method redefines how to print the intgrd object to the screen.
# Adapted from SP
#' Print the contents of an intgrd object
#'
#' This function extends print.sp by including a display and summary of the
#' interval slot for the object.
#'
#' @param x an object of class intgrd
#' @param ... additional arguments to base::print
#' @param digits default option from sp package
#' @return nothing
#'
#' @export
print.intgrd = function(x, ..., digits = getOption("digits")) {
  cc = substring(paste(as.data.frame(
    t(signif(coordinates(x), digits)))),2,999)
  int = paste("[", as.data.frame(
    t(signif(interval(x), digits))), "]", sep = "")
  int <- gsub(int, pattern = "[()c]", replacement = "")

  # drop = false ensures the resul remains a data frame and
  # cannot be converted to a vector.
  df = data.frame("coordinates" = cc, "interval" = int, x@data)

  row.names(df) <- row.names(x@data)
  print(df, ..., digits = digits)
}
methods::setMethod("show", "intgrd", function(object) print(object))

# This method redefines how to print the head the intgrd object to the screen.
# Adapted from SP
#' Print the head of an intgrd object.
#'
#' This function extends print.sp by including a display and summary of the
#' interval slot for the object.
#'
#' @param x an object of class intgrd
#' @param n number of rows to print to the screen
#' @param ... additional arguments to print
#' @param digits determines how values are printed to the screen
#' (default from sp package)
#' @return nothing
#'
#' @export
head.intgrd = function(x, n = 6, ..., digits = getOption("digits")){
  cc = substring(paste(as.data.frame(
    t(signif(sp::coordinates(x), digits)))),2,999)
  int = paste("[", as.data.frame(
    t(signif(intkrige::interval(x), digits))), "]", sep = "")
  int <- gsub(int, pattern = "[()c]", replacement = "")
  df = data.frame("coordinates" = cc, "interval" = int, x@data)

  row.names(df) <- c("coordinates", "interval", row.names(x@data))
  print(df[1:n, ], ..., digits = digits)
}

# This method redefines how to print the tail of the intgrd object to the screen.
# Adapted from SP
#' Print the tail of an intgrd object.
#'
#' This function extends print.sp by including a display and summary of the
#' interval slot for the object.
#'
#' @param x an object of class intgrd
#' @param n number of rows to print to the screen
#' @param ... additional arguments to utils::tail()
#' @param digits determines how numbers are displayed (default taken from
#' package sp)
#' @return nothing
#'
#' @export
tail.intgrd = function(x, n = 6, ..., digits = getOption("digits")){
  cc = substring(paste(as.data.frame(
    t(signif(sp::coordinates(x), digits)))),2,999)
  int = paste("[", as.data.frame(
    t(signif(intkrige::interval(x), digits))), "]", sep = "")
  int <- gsub(int, pattern = "[()c]", replacement = "")
  df = data.frame("coordinates" = cc, "interval" = int, x@data)

  row.names(df) <- c("coordinates", "interval", row.names(x@data))
  print(df[(nrow(df)-(n-1)):nrow(df), ], ..., digits = digits)
}


# This method adapts the summary.spatial to include a covariance matrix
# for the interval-center and radii in the output.
#' Summarize the contents of an intgrd object, including special summaries for
#' the interval slot.
#'
#' @param object an object of class intgrd
#' @param ... additional arguments to base::summary().
#' @return nothing
#' @method summary intgrd
#'
#' @export
summary.intgrd = function(object, ...) {
  obj = list()
  obj[["class"]] = class(object)
  obj[["bbox"]] = bbox(object)
  obj[["is.projected"]] = is.projected(object)
  obj[["proj4string"]] = object@proj4string@projargs
  # Add the covariance matrix for center/radius interaction.
  obj[["vcov"]] <- stats::cov(interval(object))
  obj[["itvl"]] <- summary(interval(object))
  obj[["grid"]] = gridparameters(object)
  if ("data" %in% methods::slotNames(object) && ncol(object@data) > 0)
    obj[["data"]] = summary(object@data)
  class(obj) = "summary.intgrd"
  obj
}
methods::setMethod("summary", "intgrd", summary.intgrd)

# This method defines how intgrd objects are printed to the screen.
#' Print the object summary to the screen.
#'
#' @param x an object an object of class intgrd
#' @param ... additional arguments to print()
#' @return nothing
#'
#' @export
print.summary.intgrd = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Coordinates:\n")
  print(x[["bbox"]], ...)
  cat(paste("Is projected:", x[["is.projected"]], "\n"))
  #    cat(paste("proj4string : [", x[["proj4string"]], "]\n", sep=""))
  pst <- paste(strwrap(x[["proj4string"]]), collapse="\n")
  if (nchar(pst) < 40) cat(paste("proj4string : [", pst, "]\n", sep=""))
  else cat(paste("proj4string :\n[", pst, "]\n", sep=""))
  if (!is.null(x$npoints)) {
    cat("Number of points: ")
    cat(x$npoints)
    cat("\n")
  }
  if (!is.null(x$n.polygons)) {
    cat("Number of polygons: ")
    cat(x$n.polygons)
    cat("\n")
  }
  if (!is.null(x$grid)) {
    cat("Grid attributes:\n")
    print(x$grid, ...)
  }
  if (!is.null(x[["vcov"]])) {
    cat("center-radius covariance matrix:\n")
    print(x[["vcov"]], ...)
  }
  if (!is.null(x[["itvl"]])) {
    cat("enpoints:\n")
    print(x[["itvl"]], ...)
  }
  if (!is.null(x$data)) {
    cat("Data attributes:\n")
    print(x$data, ...)
  }
  invisible(x)
} # No "methods::setMethod" as this directly calls intgrd.


#' @name intvariogram
#' @rdname intvariogram-methods
#' @aliases intvariogram,intgrd-method
methods::setMethod("intvariogram", "intgrd",
                   function(x, formulas = list(center ~ 1, radius ~ 1), ...){

                     # First ensure that the center and
                     # radius are included in the proper formulas
                     # (When strings are converted to strings, mathematical operators
                     # act as a string split. Because we simply need "center" and
                     # "radius" to appear somewhere in the text, we use the "any"
                     # function to return one logical)
                     check1 <- any(regexpr(pattern = "center",
                                           text = formulas[[1]][[2]]) > 0)
                     check2 <- any(regexpr(pattern = "radius",
                                           text = formulas[[2]][[2]]) > 0)

                     if(!check1){
                       stop("Formula one must contain \'center\'
                            in the dependent variable slot.")
                     }
                     if(!check2){
                       stop("Formula two must contain \'radius\'
                            in the dependent variable slot.")
                     }

                     x$center <- (interval(x)[, 1] + interval(x)[, 2]) / 2
                     x$radius <- (interval(x)[, 2] - interval(x)[, 1]) / 2

                     g1 <- gstat::gstat(NULL, "center",
                                        formula = formulas[[1]], data = x, ...)
                     g2 <- gstat::gstat(g1, "radius",
                                        formula = formulas[[2]], data = x, ...)

                     gv <- gstat::variogram(g2)

                     # Have the intvariogram class inherit the
                     # original variogram class
                     class(gv) <- c("intvariogram", class(gv))

                     return(gv)
                   })

#' @name as.data.frame
#' @rdname interval.as.data.frame-methods
#' @aliases as.data.frame,intgrd-method
methods::setMethod("as.data.frame", "intgrd", function(x){
  interval(x) <- NULL
  return(as.data.frame(x))
})


#' Create an interval plot for spatial points.
#'
#' Calls the sp::spplot() function to plot the locations, centers, and
#' radii of an interval-valued spatial data frame in a single figure.
#'
#' @param x an object of class intsp
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
#' @return a lattice plot object
#'
#' @method plot intgrd
#' @noRd
plot.intgrd <- function(x, beside = TRUE, legend.positions = c("right", "right"),
                        ...){

  if(beside){
    pc <- sp::spplot()
    gridExtra::grid.arrange()
  }

}
