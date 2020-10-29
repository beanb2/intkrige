#' Algorithmic implementation of interval valued kriging.
#'
#' Function to implement the interval valued extension of ordinary and
#' simple kriging. Includes all necessary input checks and error handling.
#' Essentially acts as a switch function between the R and c++ versions
#' of the algorithm.
#'
#' @param locations An object of class intsp, specifying the prediction
#'   locations with an interval-valued response.
#' @param newdata An object of class SpatialPointsDataFrame or
#'   SpatialPixelsDataFrame specifying the locations at which to predict
#'   intervals.
#' @param models A list of variogram models of class vgm (see \link[gstat]{vgm})
#'   When specified, the third model represents the center/radius interaction.
#' @param centerFormula The formula describing the relationship between the radius center
#'   and any number of explanatory variables. Must have "center" as the lone
#'   dependent variable. Note that there is no option to fit an external
#'   trend to the radius.
#' @param eta A growth/shrink parameter for penalty term.
#'   For simple kriging: eta > 1. For ordinary kriging: eta < 1.
#' @param A vector of length three representing the weights
#'   of the generalized L2 distance: the vector of three contains the weights for
#'   the center, radius, and center/radius respectively.
#'   A = c(1, 1, 0) assumes the regular L2 distance calculation for intervals.
#' @param trend If null, use ordinary kriging. When specified, represents the
#'   known mean of the stationary process and is an indication to use
#'   simple kriging.
#' @param thresh Let n = length(locations). When abs(lam_i) < 1/(n*thresh),
#'   this lambda value is set to 0.
#' @param tolq For a set penalty term, convergence is satisfied if
#'   max(abs(lamUp-lam)) < tolq.
#' @param maxq For a set penalty term, the max number of iterations
#'   allowed for convergence.
#' @param tolp When abs(sum(abs(lam)) - 1) < tolp, consider the
#'   constraints satisfied.
#' @param maxp Maximum number of allowed iterations to satisfy
#'   equation constraints.
#' @param r The starting value of the penalty term. Must be relatively large to
#'   ensure that the initial solution stays within the feasible region.
#' @param useR If TRUE, use the R version of the algorithm.
#'   If FALSE, use the rcppArmadillo version.
#' @param fast (Simple kriging only). If TRUE, allows lambdas to converge to 0
#'   and subsets matrices accordingly. When FALSE, runs simple kriging using a
#'   barrier penalty at 0. Fast = TRUE is orders of magnitude faster than the
#'   full implementation. However, it is not recommended when input
#'   measurements are sparse as it is known to have convergence issues
#'   in these cases.
#' @param weights If TRUE, return the vector kriging weights for each prediction.
#'   If false, simply return the predicted output.
#' @param cores An integer (for parallel computing): specify the number
#'   of cores that will be devoted to the computation.
#'   Note that the argument 'all' will
#'   use all available cores minus one.
#'   Parallel processing is only relevant if you are predicting
#'   for more than one location.
#'   Note there is no parallel option when useR = FALSE.
#' @param progress (Not valid when useR=FALSE or in parallel):
#'   Print a progress bar showing the progress of the predictions
#'   at the new locations.
#' @return A matrix with 4 columns where rows correspond to the prediction
#'  locations and columns correspond to:
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
#' @details
#' The centerFormula argument allows for an external trend to be fit to the
#'   interval centers. No option is provided to scale the interval radii. This
#'   means that any transformations should be applied to the entire interval
#'   prior to input into interval-valued kriging. This ensures that input into
#'   the interval-valued kriging algorithm are well-defined intervals with
#'   properly ordered upper and lower endpoints.
#'
#' @useDynLib intkrige, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats predict
#'
#' @examples
#' # First, define the location and elevation of interest.
#' # (In this case we pick coordinates of Utah State University)
#' tproj <- sp::CRS(SRS_string = "EPSG:4326")
#' templocs <- data.frame(lat = 41.745, long = -111.810, ELEVATION = 1456)
#' sp::coordinates(templocs) <- c("long", "lat")
#' sp::proj4string(templocs) <- tproj
#'
#' # Load the Utah Snow Load Data
#' data(utsnow)
#' utsnow.sp <- utsnow
#'
#' # Convert to an 'intsp' object that inherits a SpatialPointsDataFrame
#' sp::coordinates(utsnow.sp) <- c("LONGITUDE", "LATITUDE")
#' sp::proj4string(utsnow.sp) <- tproj
#' interval(utsnow.sp) <- c("minDL", "maxDL")
#' # Define the formulas we will use to define the intervals.
#' temp_formula <- center ~ ELEVATION
#'
#' # Define, fit and check the variogram fits.
#' varios <- intvariogram(utsnow.sp,
#'                        centerFormula = temp_formula)
#' varioFit <- fit.intvariogram(varios, models = gstat::vgm(c("Sph", "Sph", "Gau")))
#' preds <- intkrige::intkrige(locations = utsnow.sp,
#' newdata = templocs,
#' models = varioFit,
#' centerFormula = temp_formula)
#'
#' @export
intkrige <- function(locations, newdata, models,
                     centerFormula = center ~ 1,
                     eta = 0.75, A = c(1, 1, 0), trend = NULL,
                     thresh = 100, tolq = .001, maxq = 100,
                     tolp = .001, maxp = 100, r = 1, useR = TRUE,
                     fast = FALSE, weights = FALSE,
                     cores = 1, progress = FALSE){
  ### Error checks for for spatial objects
  #===========================================================================
  if(class(locations)[1] != "intsp"){
    stop("locations must be of class intsp")
  }
  if(!inherits(newdata, "SpatialPoints") &&
     !inherits(newdata, "SpatialPixels")){
    stop("newdata must inherit class SpatialPoints or
         SpatialPixels")
  }
  if(class(models)[1] != "list" || length(models) < 2 || length(models) > 3){
    stop("models must be a list of 2-3 variograms")
  }
  if(class(models[[1]])[1] != "variogramModel" || class(models[[2]])[1] != "variogramModel"){
    stop("Variogram models must all be of class variogramModel
         (see gstat package documentation.")
  }
  #===========================================================================

  ### Error checks for optimization parameters
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
  if(!inherits(A, "numeric") || length(A) != 3){
    stop("A must be a numeric vector of length 3.")
  }else{
    if(A[3] >= max(A[1], A[2])){
      stop("The weight of the center/radius interaction cannot meet or
           exceed the weight of the individual components.")
    }
  }
  if(tolp > .01 || tolq > .01){
    warning("At least one large tolerance criterion detected. Consider using\n
            smaller tolerance parameters.")
  }else{
    if(tolp <= 0 || tolq <=0){
      stop("Tolerance parameters must be strictly positive.")
    }
  }
  if(inherits(cores, "character")){
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

  ### Formula Checks
  #===========================================================================
  # Check for the center formula:
  if(all.vars(centerFormula)[1] != "center"){
    stop("Formula must have center as the dependent variable.")
  }
  #===========================================================================

  ### Other checks
  #===========================================================================
  if(is.null(trend) && fast){
    warning("Fast specification only relevant for simple kriging,
            which requires the trend to be specified.\n
            Fast = TRUE is ignored.")
  }
  if(!inherits(useR, "logical") || !inherits(fast, "logical") ||
     !inherits(weights, "logical")){
    stop("Variables useR, fast, and weights must all
         be of class \'logical\'")
  }
  #===========================================================================
  # Add centers and radii to data columns
  locations$center <- (interval(locations)[, 1] + interval(locations)[, 2]) / 2
  locations$radius <- (interval(locations)[, 2] - interval(locations)[, 1]) / 2

  # Prepare the necessary prediction matrices.
  # dist_cpp is twice as fast as spDists() but the speedup is not worth the
  # increased complexity for the user.
  pwDist <- sp::spDists(locations)
  loiDist <- sp::spDists(locations, newdata)

  # Ensure the diagonals of the pairwise matrix are exactly equal to 0.
  # (To avoid numerical precision errors).
  for(i in 1:nrow(pwDist)){
    pwDist[i, i] <- 0
  }

  # Determine covariance calculations for all variogram models specified.
  pCov <- lCov <- vector("list", length(models))
  for (i in 1:length(models)){
    pCov[[i]] <- gstat::variogramLine(models[[i]], dist_vector = pwDist,
                                      covariance = TRUE)
    lCov[[i]] <- gstat::variogramLine(models[[i]], dist_vector = loiDist,
                                      covariance = TRUE)
  }

  pCovC <- pCov[[1]]
  pCovR <- pCov[[2]]

  lCovC <- lCov[[1]]
  lCovR <- lCov[[2]]

  # Determine if the interaction term needs
  if(length(pCov) > 2){
    pCovCR <- pCov[[3]]
    lCovCR <- lCov[[3]]
  }else{
    # create matrices of 0 values for placeholders
    pCovCR <- matrix(0, nrow = nrow(pCovC), ncol = ncol(pCovC))
    lCovCR <- matrix(0, nrow = nrow(lCovC), ncol = ncol(lCovC))
  }

  # Create non interval version of the dataset:
  locations2 <- locations
  interval(locations2) <- NULL

  # Remove any linear trends in the interval data.
  if(length(labels(stats::terms(centerFormula))) > 0){
    templm <- gstat::gstat(NULL, "center",
                           formula = centerFormula,
                           data = locations2,
                           model = models[[1]])
    tpred <- predict(templm, newdata = locations2, BLUE = TRUE)
    center_inputs <- locations$center - tpred@data$center.pred
    lma <- TRUE
  }else{
    center_inputs <- locations$center
    lma <- FALSE
  }

  measurements <- matrix(c(center_inputs, locations$radius),
                         ncol = 2, byrow = FALSE)

  # Prepare the inputs.
  if(useR){
    predicts <- nrShell_R(pCovC, pCovR, pCovCR, lCovC, lCovR, lCovCR,
                          measurements, eta, trend, A, thresh, tolq,
                          maxq, tolp, maxp, r, fast, weights, cores,
                          progress)
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

  # If weights=TRUE, only deal with the predicted data.
  if(weights){
    temp_predicts <- predicts[[1]]
  }else{
    temp_predicts <- predicts
  }

  # Determine if we need to create a data slot for the upper and lower enpoints.
  if(class(newdata)[1] == "SpatialPixels"){
    newdata <-
      sp::SpatialPixelsDataFrame(newdata,
                                 data = data.frame(center = temp_predicts[, 1],
                                                   radius = temp_predicts[, 2],
                                                   kriging_variance = temp_predicts[, 3],
                                                   warn = temp_predicts[, 4]))
  }else if(class(newdata)[1] == "SpatialPoints"){
    newdata <-
      sp::SpatialPointsDataFrame(newdata,
                                 data = data.frame(center = temp_predicts[, 1],
                                                   radius = temp_predicts[, 2],
                                                   kriging_variance = temp_predicts[, 3],
                                                   warn = temp_predicts[, 4]))
  }else{
    # add raw prediction information to the existing data frame.
    # this will inadvertently overwrite data with the same names
    # already existing in the frame.
    newdata$center <- temp_predicts[, 1]
    newdata$radius <- temp_predicts[, 2]
    newdata$kriging_variance <- temp_predicts[, 3]
    newdata$warn <- temp_predicts[, 4]
  }

  # Transform back to original units and fill the interval slot if a linear model was fit.
  if(lma){
    temp_predicts <- predict(templm, newdata = newdata, BLUE = TRUE)
    center_final <- temp_predicts$center.pred + newdata$center
  }else{
    center_final <- newdata$center
  }


  # Create interval-valued spatial object
  newdata$lower <- center_final - newdata$radius
  newdata$upper <- center_final + newdata$radius

  interval(newdata) <- c("lower", "upper")

  # Return the appended newdata spatial object
  if(weights){
    return(list(newdata, predicts[[2]]))
  }else{
    return(newdata)
  }
}

