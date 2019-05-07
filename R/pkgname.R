#' intkrige package: Interval-valued kriging models in R.
#' @aliases intkrige-package
#'
#' @section Description:
#' An interval valued extension of ordinary and simple kriging.
#' Optimization of the function is based on a generalized interval distance.
#' This creates a non-differentiable cost function that requires a
#' differentiable approximation to the absolute value function. This
#' differentiable appoximation is optimized using a Newton-Raphson algorithm
#' with a penalty function to impose the constraints.
#' Covariances are specified using the \code{\link[gstat]{vgm}}
#' object conventions.
#'
#' @section Functions:
#' \itemize{
#' \item \code{\link{intkrige}} Predict using an interval-valued kriging model.
#' \item \code{\link{dist_cpp}} Function to compute geographic or euclidean
#' distances.
#' }
#'
#' @section Data:
#' \itemize{
#' \item \link{utsnow} An interval-valued design ground snow load dataset for Utah.
#' }
#'
#'
#' @author Brennan Bean \email{brennan.bean.20@@gmail.com}
#' @importFrom Rdpack reprompt
"_PACKAGE"


