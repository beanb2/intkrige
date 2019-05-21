### CREATE CLASSES ###
#=============================================================================
# This is the "intsp" class, which extends the "SpatialPointsDataFrame"
# class. All we do is add a slot for intervals that can be called by plot,
# print, and summary functions.
#' @import sp
#' @import gstat
NULL
#' @export
methods::setClass("intsp",
                  contains = "SpatialPointsDataFrame",
                  slots = c(interval = "matrix"))
#=============================================================================

### CREATE GENERICS ###
#=============================================================================
# Generic to change the contents of the "intsp" interval slot.
#' @export
methods::setGeneric("interval",
                    function(x, cr = FALSE)
                      standardGeneric("interval"))

# Generic to inititate the contents of the "intsp" interval slot.
#' @export
methods::setGeneric("interval<-",
                    function(x, value, ...)
                      standardGeneric("interval<-"))
#=============================================================================


### CREATE METHODS FOR GENERICS ###
#=============================================================================
# This method ensures that the "$" operator looks through the data,
# coordinate, and interval slots when looking for a subset.
# Adapted from:
# - https://github.com/edzer/sp/blob/master/R/SpatialPoints-methods.R
#' @export
setMethod("$", "intsp",
          function(x, name) {
            if (name %in% coordnames(x))
              return(x@coords[,name])
            if (name %in% colnames(x@interval))
              return(x@interval[, name])
            if (!("data" %in% slotNames(x)))
              stop("no $ method for object without attributes")
            x@data[[name]]
          }
)

# This method stops users from editing the coordinates or intervals
# using the standard "$" operator.
# Adapted from:
# - https://github.com/edzer/sp/blob/master/R/Spatial-methods.R
#' @export
setMethod("$<-", "intsp",
          function(x, name, value) {
            if (name %in% coordnames(x))
              stop(paste(name,
                         "is a coordinate name, please choose another name"))
            # Addition for coordinate slot.
            if (name %in% colnames(x@interval))
              stop(paste(name, "is currently assigned to the interval slot,
                         please choose another name"))
            if (!("data" %in% slotNames(x))) {
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

# This method returns the interval slot.
#' @export
setMethod("interval", "intsp", function(x, cr = FALSE) {
    return(x@interval)
})

# This method allows the user to define (or redefine) the
# interval slot for an existing intsp object.
#' @export
setMethod("interval<-", "intsp",
          function(x, value, cr = FALSE) {
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

            # Add whether or not the interval is defined
            # with a center/radius specification.
            attr(x@interval, "centerRadius") <- cr

            return(x)
          })

# This method allows a user to create an intsp object by
# definining the interval slot. (Similar to sp::coordinates).
#' @export
setMethod("interval<-", "SpatialPointsDataFrame",
          function(x, value, cr = FALSE) {
            x <- as(x, "intsp")

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

            # Add whether or not the interval is defined
            # with a center/radius specification.
            attr(x@interval, "centerRadius") <- cr

            return(x)
          })

# This method redefines how to print the intsp object to the screen.
# Adapted from SP
#' @export
print.intsp = function(x, ..., digits = getOption("digits")) {
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
setMethod("show", "intsp", function(object) print(object))

# This method redefines how to print the head the intsp object to the screen.
# Adapted from SP
#' @export
head.intsp = function(x, ..., digits = getOption("digits")) {
  cc = substring(paste(as.data.frame(
    t(signif(coordinates(x), digits)))),2,999)
  int = paste("[", as.data.frame(
    t(signif(interval(x), digits))), "]", sep = "")
  int <- gsub(int, pattern = "[()c]", replacement = "")
  df = data.frame("coordinates" = cc, "interval" = int, x@data)

  row.names(df) <- row.names(x@data)
  print(df[1:6, ], ..., digits = digits)
}
setMethod("show", "intsp", function(object) print(object))

# This method redefines how to print the tail of the intsp object to the screen.
# Adapted from SP
#' @export
tail.intsp = function(x, ..., digits = getOption("digits")) {
  cc = substring(paste(as.data.frame(
    t(signif(coordinates(x), digits)))),2,999)
  int = paste("[", as.data.frame(
    t(signif(interval(x), digits))), "]", sep = "")
  int <- gsub(int, pattern = "[()c]", replacement = "")
  df = data.frame("coordinates" = cc, "interval" = int, x@data)

  row.names(df) <- row.names(x@data)
  print(df[(nrow(df)-5):nrow(df), ], ..., digits = digits)
}
setMethod("show", "intsp", function(object) print(object))


# This method adapts the summary.spatial to include a covariance matrix
# for the interval-center and radii in the output.
#' @export
summary.intsp = function(object, ...) {
  obj = list()
  obj[["class"]] = class(object)
  obj[["bbox"]] = bbox(object)
  obj[["is.projected"]] = is.projected(object)
  obj[["proj4string"]] = object@proj4string@projargs
  # Add the covariance matrix for center/radius interaction.
  obj[["vcov"]] <- cov(interval(object))
  obj[["itvl"]] <- summary(interval(object))
  if (is(object, "SpatialPoints"))
    obj[["npoints"]] = nrow(object@coords)
  if (is(object, "SpatialGrid") || is(object, "SpatialPixels"))
    obj[["grid"]] = gridparameters(object)
  obj[["vcov"]] <- cov(interval(object))
  obj[["itvl"]] <- summary(interval(object))
  if ("data" %in% slotNames(object) && ncol(object@data) > 0)
    obj[["data"]] = summary(object@data)
  class(obj) = "summary.intsp"
  obj
}
setMethod("summary", "intsp", summary.intsp)

#' @export
print.summary.intsp = function(x, ...) {
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
}
#=============================================================================
