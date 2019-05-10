
<!-- README.md is generated from README.Rmd. Please edit that file -->
intkrige
========

This package implements the interval-valued kriging models described in Bean et. al. (2019a). Details regarding the algorithmic implementation can be found in Bean et. al. (2019b).

Installation
------------

This is a preliminary version of the intkrige package. The final package will include additional user functions and visualization tools designed to simplify and enhance interval-valued spatial data analysis. This developmental version of the pacakge can be installed by setting the source file location to the parent folder and running the command

``` r
devtools::install()
```

Note that the developmental installation makes use of the devtools package (Wickham, Hester, and Chang 2019). The most current version of this package can be found at .

Package Contents
----------------

The current version of the package contains one main function named *intkrige*. Information about this function can be obtained by entering

``` r
?intkrige::intkrige
```

in the console.

The package also contains an interval-valued design ground snow load datset named *utsnow*. These data are available by installing the intkrige package and invoking the command

``` r
data(utsnow)
```

with dataset documentation obtained by

``` r
?utsnow
```

Descriptions for the source folders included in this package are as follows:

-   **data**: Includes a copy of the utsnow dataset.
-   **data-raw**: Includes the raw data files used to create utsnow as well as the scripts use to process the data.
-   **inst**: Includes a latex bibliography of citations used in the documentation.
-   **man**: Includes files used to render formatted function documentation R.
-   **R**: Functions and scripts used to execute the interval-valued kriging models.
-   **src**: c++ version of the interval-valued kriging models created using Rcpp and RcppArmadillo (Eddelbuettel and Sanderson 2014; Eddelbuettel and François 2011; Eddelbuettel 2013; Eddelbuettel and Balamuta 2017).

Example
-------

Once installed, this package runs simple and ordinary interval-valued kriging models. The following example comes from the design ground snow load application described in Bean et. al. (Bean, Sun, and Maguire 2019a). The example shows how to make an interval valued kriging design ground snow load prediction (after removing the effect of elevation) for Logan, Utah. This example makes use of several existing resources available in the gstat and sp packages (Pebesma 2004; Gräler, Pebesma, and Heuvelink 2016; Bivand et al. 2013).

``` r
# First, define the location and elevation of interest. 
# (In this case we pick coordinates of Utah State University)
lats <- c(41.745)
lons <- c(111.810)
elevation <- c(1456)

# Load the Utah Snow Load Data
data(utsnow)

# Determine the centers and radii on a log scale.
utsnow$center = (log(utsnow$maxDL) + log(utsnow$minDL)) / 2
utsnow$radius = (log(utsnow$maxDL) - log(utsnow$minDL)) / 2

# Scale the interval centers and radii
cLM <- lm(center ~ ELEVATION, data = utsnow)
utsnow$rC <- cLM$residuals
utsnow$rR <- utsnow$radius*(utsnow$ELEVATION/median(utsnow$ELEVATION))

# Extract the variograms for the centered/scaled residuals. 
# This requires converting the provided dataset to a 
# spatialdataframe using the gstat package. 
# Note that the center, radius, and center/radius interaction each
# require their own variograms. 
utsnow.sp <- utsnow
sp::coordinates(utsnow.sp) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(utsnow.sp) <- "+proj=longlat +ellps=WGS84
                              +datum=WGS84 +no_defs +towgs84=0,0,0"
g1 <- gstat::gstat(NULL, "center", rC ~ 1, data = utsnow.sp)
g1 <- gstat::gstat(g1, "radius", rR ~ 1, data = utsnow.sp)

# Automatic variogram fitting uses the gstat package. 
evgm <- gstat::variogram(g1)
vsub1 <- evgm[evgm$id == "center", ]
vsub2 <- evgm[evgm$id == "radius", ]
vsub3 <- evgm[evgm$id == "center.radius", ]
tvgm1 <- gstat::fit.variogram(vsub1, model = gstat::vgm("Sph"))
tvgm2 <- gstat::fit.variogram(vsub2, model = gstat::vgm("Sph"))
tvgm3 <- gstat::fit.variogram(vsub3, model = gstat::vgm("Gau"))

# Prepare data for function input
input_locs <- sp::coordinates(utsnow.sp)
input_int <- as.matrix(utsnow[, c("rC", "rR")])
pred_locs <-  matrix(c(lons, lats), ncol = 2)

# Run a basic kriging model using argument defaults. 
preds <- intkrige::intkrige(locations = input_locs, measurements = input_int, 
                            newdata = pred_locs, modelC = tvgm1, 
                            modelR = tvgm2, modelCR = tvgm3)

# The final results are predicted intervals after removing the effect of elevation.  
preds
#>             [,1]      [,2]      [,3] [,4]
#> [1,] -0.06162802 0.6482878 0.5857494    0
```

Note that the prediction for a single location returns four outputs including (in order):

-   center prediction
-   radius prediction
-   kriging prediction variance
-   0-1 warning for non-convergent optimization (1 if warning, 0 if not).

References
==========

Bean, Brennan, Yan Sun, and Marc Maguire. 2019a. “Interval-Valued Kriging Models for Geostatistical Mapping with Uncertain Inputs.”

———. 2019b. “Supplement to ‘Interval-Valued Kriging Models for Geostatistical Mapping with Uncertain Inputs’.”

Bivand, Roger S, Edzer J Pebesma, Virgilio Gomez-Rubio, and Edzer Jan Pebesma. 2013. *Applied Spatial Data Analysis with R*. 2nd ed. Vol. 747248717. Springer.

Eddelbuettel, Dirk. 2013. *Seamless R and C++ Integration with Rcpp*. New York: Springer. doi:[10.1007/978-1-4614-6868-4](https://doi.org/10.1007/978-1-4614-6868-4).

Eddelbuettel, Dirk, and James Joseph Balamuta. 2017. “Extending extitR with extitC++: A Brief Introduction to extitRcpp.” *PeerJ Preprints* 5 (August): e3188v1. doi:[10.7287/peerj.preprints.3188v1](https://doi.org/10.7287/peerj.preprints.3188v1).

Eddelbuettel, Dirk, and Romain François. 2011. “Rcpp: Seamless R and C++ Integration.” *Journal of Statistical Software* 40 (8): 1–18. doi:[10.18637/jss.v040.i08](https://doi.org/10.18637/jss.v040.i08).

Eddelbuettel, Dirk, and Conrad Sanderson. 2014. “RcppArmadillo: Accelerating R with High-Performance C++ Linear Algebra.” *Computational Statistics and Data Analysis* 71 (March): 1054–63. <http://dx.doi.org/10.1016/j.csda.2013.02.005>.

Gräler, Benedikt, Edzer Pebesma, and Gerard Heuvelink. 2016. “Spatio-Temporal Interpolation Using Gstat.” *The R Journal* 8 (1): 204–18. <https://journal.r-project.org/archive/2016-1/na-pebesma-heuvelink.pdf>.

Pebesma, Edzer J. 2004. “Multivariable Geostatistics in S: The Gstat Package.” *Computers & Geosciences* 30: 683–91.

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. *Devtools: Tools to Make Developing R Packages Easier*. <https://github.com/r-lib/devtools>.
