require(compiler)
require(inline)

code1 <- '
arma::mat lam = Rcpp::as<arma::mat>(a);
double threshold = Rcpp::as<double>(b);
lam.elem( arma::find(arma::abs(lam) <  threshold) ).zeros();
return Rcpp::wrap(lam);
'

code2 <- '
arma::mat lam = Rcpp::as<arma::mat>(a);
double threshold = Rcpp::as<double>(b);
lam.clean(threshold);
return Rcpp::wrap(lam);
'

subset1 <- cxxfunction(signature(a="numeric", b="numeric"),
                       code1, plugin="RcppArmadillo")
subset2 <- cxxfunction(signature(a="numeric", b="numeric"),
                       code2, plugin="RcppArmadillo")

A <- matrix(rnorm(250000), nrow = 500)

x <- subset1(A, 1)
y <- subset2(A, 1)

all.equal(x, y)
identical(x, y)


microbenchmark::microbenchmark(subset1(A, 1), subset2(A, 1))


