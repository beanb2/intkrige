#=============================================================================
# This file contains wrappers to packages on which intkrige depends:
# gstat, sp, and raster. These functions simplify the variogram calculation
# and spatial visualizations of spatial data.
#=============================================================================
#' Function to create a variogram object for interval-valued data.
#'
#' @param x an object of class intvariogram
#' @param models an object of class variogramModelList. Must specify at least
#' two variogram models to fit (for center and radius) if less than three
#' models are specified then the method fails to fit a variogram for the
#' center radius interaction.
#' @param ... additional arguments to gstat::fit.variogram()
#'
#' @return a list containing the parameters for each of the three (or two)
#' fitted variograms.
#'
#' @export
fit.intvariogram <-
  function(x, models = gstat::vgm(rep("Sph", 3)), ...){
    if(class(x)[1] != "intvariogram"){
      stop("Function only defined for objects of class intvariogram")
    }
    if(class(models)[1] != "variogramModelList"){
      stop("models must be specified as a list object")
    }
    if(length(models) < 2 || length(models) > 3){
      stop("between 2 and three variogram models must be specified")
    }

    # Fit the variogram models in the following order:
    # 1. center, 2. radius, 3. center/radius.
    vorder <- c("center", "radius", "center.radius")
    varioFit <- vector("list", length(models))
    for(i in 1:length(models)){
      x_sub <- x[x$id == vorder[i], ]

      varioFit[[i]] <- gstat::fit.variogram(x_sub,
                                            model = models[[i]], ...)
    }

    return(varioFit)
  }

#' Function to visualize the three variograms from an interval valued
#' spatial data frame.
#'
#' @param x an object of class intvariogram
#' @param models a list of fitted variogram models, typically an output of
#' fit.intvariogram
#' @param ... additional arguments to gstat::plot.variogram()
#' @return nothing (but do plot to the screen)
#' @export
intvCheck <- function(x, models, ...){
  p1 <- plot(x[x$id == "center", ], model = models[[1]],
             main = "center", ...)
  p2 <- plot(x[x$id == "radius", ], model = models[[2]],
             main = "radius", ...)
  if(length(models) > 2){
    p3 <- plot(x[x$id == "center.radius", ], model = models[[3]],
               main = "center.radius", ...)
  }else{
    p3 <- plot(x[x$id == "center.radius", ])
  }

  return(gridExtra::grid.arrange(p1, p2, p3, nrow = 2, ncol = 2))
}




