#' Algorithmic implementation of interval valued kriging.
#'
#' Function to implement the interval valued extension of ordinary and
#' simple kriging. Includes all necessary input checks and error handling.
#' Essentially acts as a switch function between the R and c++ versions
#' of the algorithm.
#'
#' @param locations A two column matrix containing the x/y
#' (longitude/latitude) coordinates of the input data.
#' @param measurements A two column matrix containing the center/radius
#' measurements corresponding to the input locations.
#' @param newdata A two column matrix containing the x/y
#' (longitude/latitude) coordinates of the prediction locations.
#' @param modelC variogram model of class vgm (see \link[gstat]{vgm})
#' for the centers of the intervals
#' @param modelR variogram model of class vgm for radii
#' @param modelCR If NULL, assume no interaction between center and radius.
#' When specified, represents vgm model for the center/radius interaction.
#' @param eta growth/shrink parameter for penalty term.
#' For simple kriging: eta > 1. For ordinary kriging eta < 1.
#' @param A vector of length three representing the weights
#' of the generalized L2 distance: the vector of three contains the weights for
#' the center, radius, and center/radius respectively.
#' A = c(1, 1, 0) assumes the regular L2 distance calculation for intervals.
#' @param trend If null, use ordinary kriging. When specified, represents the
#' known mean of the stationary process, an indication to use simple kriging.
#' @param thresh Let n = length(locations). When abs(lam_i) < 1/(n*thresh),
#' this lambda value is set to 0.
#' @param tolq For a set penalty term, convergence is satisfied if
#' max(abs(lamUp-lam)) < tolq.
#' @param maxq For a set penalty term, max number of iterations
#' allowed for convergence.
#' @param tolp When abs(sum(abs(lam)) - 1) < tolp, consider the
#' constraints satisfied.
#' @param maxp Maximum number of allowed iterations to satisfy
#' equation constraints.
#' @param r The starting value of the penalty term. Must be relatively large to
#' ensure that the initial solution stays within the feasible region.
#' @param useR If TRUE, use the R version of the algorithm.
#' If FALSE, use the rcppArmadillo version.
#' @param geographic If TRUE, use great circle distance calcuations.
#' If FALSE, use euclidean distances.
#' @param fast (Simple kriging only). If TRUE, allows lambdas to converge to 0
#' and subsets matrices accordingly. When FALSE, runs simple kriging using a
#' barrier penalty at 0. Fast = TRUE is orders of maginitude faster than the
#' full implementation. However, it is not recommended when input measurements
#' are sparse as it is known to have convergence issues in these cases.
#' @param weights If TRUE, return the vector kriging weights for each prediction.
#' If false, simply return the predicted output.
#' @param cores An integer (for parallel computing): specify the number
#' of cores that will be devoted to the computation.
#' Note that the argument 'all' will
#' use all available cores minus one.
#' Parallel processing is only relevant if you are predicting
#' for more than one location.
#' Note there is no parallel option when useR = FALSE.
#' @return A matrix with 4 columns where rows correspond to the prediction
#' locations and columns correspond to:
#'
#' - center prediction
#'
#' - radius predictions
#'
#' - kriging prediction variance
#'
#' - warning column for non-convergent optimization problem
#' (0 - no warning, 1 - warning)
#'
#' @useDynLib intkrige, .registration = TRUE
#' @importFrom Rcpp evalCpp
#'
#' @export
# TODO: Examples.
intkrige <- function(locations, measurements, newdata,
                     modelC, modelR, modelCR = NULL,
                     eta = 0.75, A = c(1, 1, 0), trend = NULL,
                     thresh = 100, tolq = .001, maxq = 100,
                     tolp = .001, maxp = 100, r = 1, useR = TRUE,
                     geographic = FALSE, fast = FALSE, weights = FALSE,
                     cores = 1){

  ### Error checks
  #===========================================================================
  if(thresh < 5 || maxq < 5){
    warning("Very low thresh or maxq parameter has been specified.\n
            Consider a larger specification of these parameters to\n
            ensure convergence...")
  }
  if(eta > 1 || eta < 0){
    stop("Both versions of kriging require 0 < eta < 1.")
  }
  if((thresh %% 1) > 0 || (maxp %% 1) > 0){
    stop("Max iteration parameters thresh and maxp must be integer valued.")
  }
  if(class(A) != "numeric" || length(A) != 3){
    stop("A must be a numeric vector of length 3.")
  }else{
    if(A[3] >= max(A[1], A[2])){
      stop("The weight of the center/radius interaction cannot meet or
           exceed the weight of the individual components.")
    }
  }
  if(class(locations) != "matrix" || class(newdata) != "matrix" ||
     class(measurements) != "matrix"){
    stop("locations, newdata, and measurements must all be matrices.")
  }else{
    if(nrow(locations) != nrow(measurements)){
      stop("Number of locations must match the number of
           measurements exactly.")
    }
    if(ncol(locations) != 2 || ncol(newdata) != 2 || ncol(measurements) != 2)
      stop("locations, newdata, and measurements inputs must all
           have precisely 2 columns.")
  }
  if(tolp > .01 || tolq > .01){
    warning("At least one large tolerance criterion detected. Consider using\n
            smaller tolerance parameters.")
  }else{
    if(tolp <= 0 || tolq <=0){
      stop("Tolerance parameters must be strictly positive.")
    }
  }
  if(class(modelC)[1] != "variogramModel" || class(modelR)[1] != "variogramModel"){
    stop("Variogram models must all be of class variogramModel
         (see gstat package documentation.")
  }
  if(is.null(trend) && fast){
    warning("Fast specification only relevant for simple kriging,
            which requires the trend to be specified.\n
            Fast = TRUE is ignored.")
  }
  if(class(useR) != "logical" || class(geographic) != "logical" ||
     class(fast) != "logical" || class(weights) != "logical"){
    stop("Variables useR, geographic, fast, and weights must all
         be of class \'logical\'")
  }
  if(class(cores) == "character"){
    if(cores != "all"){
      stop("Invalid cores specification. Must be an integer
           value or the character command \'all\'.")
    }else{
      detectCores = parallel::detectCores()
      cores = detectCores - 1
    }
  }
  if(cores %% 1 > 0){
    stop("Invalid cores specification. Must be an integer
           value or the character command \'all\'.")
  }
  if(cores > 1 && !useR){
    warning("Parallel processing not available in c++ version.
            Parallel processing will not be used.")
    cores = 1
  }
  #===========================================================================

  # Prepare the necessary prediction matrices.
  # Figure out distances between sample sites and prediction sites
  # Make sure unprojected coordinates are relevant.
  if(geographic && (min(locations[, 1], newdata[, 1]) < -180 ||
                    max(locations[, 1], newdata[, 1]) > 180 ||
                    min(locations[, 2], newdata[, 2]) < -90 ||
                    max(locations[, 2], newdata[, 2]) > 90)){
    stop("Invalid geographic coordinates detected. Please ensure that\n
           -90 < Latitude < 90 and -180 < Longitude < 180.")
  }

  pwDist <- dist_cpp(locations, locations, geographic)
  loiDist <- dist_cpp(locations, newdata, geographic)

  # Ensure the diagonals of the pairwise matrix are exactly equal to 0.
  for(i in 1:nrow(pwDist)){
    pwDist[i, i] <- 0
  }

  pCovC <- gstat::variogramLine(modelC, dist_vector = pwDist,
                                covariance = TRUE)
  pCovR <- gstat::variogramLine(modelR, dist_vector = pwDist,
                                covariance = TRUE)

  lCovC <- gstat::variogramLine(modelC, dist_vector = loiDist,
                                covariance = TRUE)
  lCovR <- gstat::variogramLine(modelR, dist_vector = loiDist,
                                covariance = TRUE)

  if(!is.null(modelCR)){
    if(class(modelCR)[1] != "variogramModel"){
      stop("modelCR must be of class variogramModel
           (see gstat package for details).")
    }
    pCovCR <- gstat::variogramLine(modelCR, dist_vector =  pwDist,
                                   covariance = TRUE)
    lCovCR <- gstat::variogramLine(modelCR, dist_vector = loiDist,
                                   covariance = TRUE)
  }else{
    if(A[3] != 0){
      warning("Non zero center/radius interaction specified in A with
              no model provided.\nA[3] will be set to 0...")
      A[3] <- 0
    }
    # Make a copy of the radius covariance as a placeholder.
    pCovCR <- pCovR
    lCovCR <- lCovR
  }

  # Prepare the inputs.
  if(useR){
    predicts <- nrShell_R(pCovC, pCovR, pCovCR, lCovC, lCovR, lCovCR,
                          measurements, eta, trend, A, thresh, tolq,
                          maxq, tolp, maxp, r, fast, weights, cores)
  }else{
    if(cores > 1){
      warning("Parallel processing not compatible with useR = FALSE.\n
            Parallel processing will not be used.")
    }

    if(weights){
      warning("weights = TRUE is only a viable option when useR = TRUE.
              This argument will be ignored.")
    }

    # Set the trend equal to the "missing" value if specified by the user.
    if(is.null(trend)){
      trend2 <- -9999.99
    }else if(trend == -9999.99){
      # Accommodate very unlikely case that someone requests the missing value
      # designation in c++ as the actual trend.
      trend2 <- trend + .00001
    }else{
      trend2 <- trend
    }

    # Make the predictions
    predicts <- nrShell(pCovC, pCovR, pCovCR, lCovC, lCovR, lCovCR,
                        measurements, A, thresh, tolq, maxq, tolp,
                        maxp, eta, r, trend2, fast)
  }

  return(predicts)
}
