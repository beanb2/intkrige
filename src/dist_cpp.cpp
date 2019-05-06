// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
#include <cmath>

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// Remember that % denotes element-wise multiplication


//' Distance matrix calculation.
//'
//' Calculation of euclidean or geograhic distance. Based on an adaptation of
//' rdist.earth in the fields package. Uses earth radius = 6378.388 for
//' great circle distance calculations.
//' @param loc1 a two column matrix represeting the longitude/latitude
//' (i.e. x/y) coordinates of the first set of distances.
//' @param loc2 a two column matrix representing the longitude/latitude
//' (i.e. x/y) coordinates of the second set of distances
//' (for pairwise distnace matrix set loc1 = loc2).
//' @param geographic If TRUE, calculate great circle distance. If FALSE, calculate
//'              Euclidean distance.
//' @return Matrix of pairwise distances between each combination of rows from loc1
//' and loc2 respectively.
//' @examples
//' data(utsnow)
//' locs1 <- matrix(utsnow[, c("LONGITUDE", "LATITUDE")], ncol = 2)
//' distMat <- dist_cpp(locs1, locs1, geographic = TRUE)
//' @export
// [[Rcpp::export]]
arma::mat dist_cpp(const arma::mat & loc1, const arma::mat & loc2,
                   const bool & geographic) {

  int l = loc1.n_rows;
  int q = loc2.n_rows;
  arma::mat tdist(l, q, arma::fill::zeros);

  // If geographic coordinates are given, return the great circle distance.
  // Help on haversine method from:
  // - https://www.r-bloggers.com/great-circle-distance-calculations-in-r/
  if(geographic){
    double lonDiff;
    double latDiff;
    double a;
    for(int i = 0; i < l; i++){
      for(int j = 0; j < q; j++){

        // Calculate degree differences (in radians)
        lonDiff = (loc2(j, 0) - loc1(i, 0))*M_PI/180;
        latDiff = (loc2(j, 1) - loc1(i, 1))*M_PI/180;

        a = sqrt( sin(latDiff/2)*sin(latDiff/2) +
          cos(loc1(i, 1)*M_PI/180)*cos(loc2(j, 1)*
          M_PI/180)*sin(lonDiff/2)*sin(lonDiff/2) );

        tdist(i, j) = 2*6378.388*asin(std::min(1.0, a));
      }
    }
  }else{ // Otherwise, return eucledian distance.
    for(int i = 0; i < l; i++){
      for(int j = 0; j < q; j++){

        tdist(i, j) = sqrt((loc1(i, 0) - loc2(j, 0))*(loc1(i, 0) - loc2(j, 0)) +
          (loc1(i, 1) - loc2(j, 1))*(loc1(i, 1) - loc2(j, 1)));

      }
    }
  }
  return tdist;
}
