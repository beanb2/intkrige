### CREATE CLASSES ###
#=============================================================================
# This is the "intsp" class, which extends the "SpatialPointsDataFrame"
# class. All we do is add a slot for intervals that can be called by plot,
# print, and summary functions.
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
#=============================================================================
