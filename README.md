
<!-- README.md is generated from README.Rmd. Please edit that file -->
intkrige
========

This package implements the interval-valued kriging models described in Bean et. al. (2019a). Details regarding the algorithmic implementation can be found in Bean et. al. (2019b).

Installation
------------

This is a preliminary version of the intkrige package. The final package will include additional user functions and visualization tools designed to simplify and enhance interval-valued spatial data analysis. This developmental version of the package can be installed by setting the source file location to the parent folder and running the command

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

The package also contains an interval-valued design ground snow load dataset named *utsnow*. These data are available by installing the intkrige package and invoking the command

``` r
data(utsnow)
```

with dataset documentation obtained by

``` r
?utsnow
```

Descriptions for the source folders included in this package are as follows:

-   **data**: Includes a copy of the utsnow dataset.
-   **data-raw**: Includes the source files used to create utsnow as well as the scripts use to process these data.
-   **inst**: Includes a latex bibliography of citations used in the documentation.
-   **man**: Includes files used to render formatted function documentation R.
-   **R**: Functions and scripts used to execute the interval-valued kriging models.
-   **src**: c++ version of the interval-valued kriging models created using Rcpp and RcppArmadillo (Eddelbuettel and Sanderson 2014; Eddelbuettel and François 2011; Eddelbuettel 2013; Eddelbuettel and Balamuta 2017).

Intro
-----

Once installed, this package runs simple and ordinary interval-valued kriging models. The following example comes from the design ground snow load application described in Bean et. al. (Bean, Sun, and Maguire 2019a). The snow load problem requires a proper consideration of the effect of elevation on ground snow loads prior to input into kriging. The current interval-valued kriging models allow for linear considerations of secondary variables in the model predictions. Such models are akin to "kriging with an external drift" (Goovaerts 1997) or "simple kriging with varying local means" (Goovaerts 2000).

The example shows how to make an interval valued kriging design ground snow load prediction (after removing the effect of elevation) for Logan, Utah. Many of the functions in the following examples are wrappers to functions in the gstat and sp packages (Pebesma 2004; Gräler, Pebesma, and Heuvelink 2016; Bivand et al. 2013).

Defining Classes and Variograms
-------------------------------

The intkrige package relies on interval-valued spatial objects. These objects are named "intsp" and "intgrd" respectively. "intsp" inherits directly from "SpatialPointsDataFrame" objects in the sp class, while "intgrd" inherits from "SpatialPixelsDataFrames". These class extensions are created using the interval() function as demonstrated below.

``` r
library(intkrige)

# First, define the location and elevation of interest. 
# (In this case we pick coordinates of Utah State University)
templocs <- data.frame(lat = 41.745, long = -111.810, ELEVATION = 1456)
sp::coordinates(templocs) <- c("long", "lat")
sp::proj4string(templocs) <- "+proj=longlat +ellps=WGS84
                              +datum=WGS84 +no_defs +towgs84=0,0,0"

# Load the Utah Snow Load Data
data(utsnow)
utsnow.sp <- utsnow

# Convert to an 'intsp' object that inherits a SpatialPointsDataFrame
sp::coordinates(utsnow.sp) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(utsnow.sp) <- sp::proj4string(templocs)
interval(utsnow.sp) <- c("minDL", "maxDL")
```

The objects are defined by the interval slot. Intervals must be defined according to their upper and lower endpoints. The program will not accept values in the interval slot that do not meet this convention. The interval-valued kriging models in the package rely on characterizations of intervals by their center and radius. The center/radius values are calculated automatically as needed by the various functions. This implies that transformations of the interval need to be completed using the upper/lower endpoint specification and can be accomplished by additional calls to the interval slot.

``` r
# analyze interval on log scale
interval(utsnow.sp) <- log(interval(utsnow.sp))
```

Fitting variograms
------------------

Next, we demonstrate how to create and fit empirical variograms. The functions that accomplish this task are fairly shallow wrappers to the sp packages functions for variograms. These functions simplify the workflow by fitting the center, radius, and center/radius variograms using the information contained in the interval slot. The intvariogram defines the empirical variograms, while the fit.intvariogram function automatically fits variograms to the output of intvariogram. Note that intvariogram defines an "intvariogram" class which is the only class that fit.intvariogram will accept. This new class designation is intended to ensure that intvariogram and fit.intvariogram are always used in tandem. These particular variograms first account for the effect of elevation on ground snow loads before calculating the empirical variograms.

``` r
# Define the formulas we will use to define the intervals. 
temp_formulas <- list(center ~ ELEVATION, 
                      radius*(ELEVATION/median(ELEVATION)) ~ 1)

# Define, fit and check the variogram fits. 
varios <- intvariogram(utsnow.sp,
                       formulas = temp_formulas)
varioFit <- fit.intvariogram(varios, models = gstat::vgm(c("Sph", "Sph", "Gau")))
intvCheck(varios, varioFit)
```

<img src="man/figures/README-varioCheck2-1.png" width="100%" />

Interval-valued kriging models
------------------------------

With variograms defined for each component of the interval, we are now ready to make interval-valued predictions using our interval-valued kriging models. If you explore the function documentaiton, you will notice that many of the function arguments relate to the Newton-Rhapson optimization technique that is used to calculate the kriging model weights. These arguments should not need to be changed from the defaults for most user purposes. The following demonstrates an ordinary kriging model after accounting for elevation.

``` r
preds <- intkrige::intkrige(locations = utsnow.sp,
                            newdata = templocs, 
                            models = varioFit,
                            formulas = temp_formulas)

# The final results are predicted intervals after removing the effect of elevation.  
preds
#>         coordinates                 interval ELEVATION   center    radius
#> 1 (-111.81, 41.745) [-0.03611231, 0.9702524]      1456 0.433943 0.5031823
#>   kriging_variance warn
#> 1        0.1517327    0
```

The interval prediction that reverses the elevation effect is included in the interval slot. The raw predictions are appended to the data frame under the following variable names:

-center: the value of the interval centers returned directly from the kriging model,

-radius: raw radii predictions,

-variance: the kriging variance according the generalized L2 distance,

-warn: a boolean that is 0 for a convergent optimization and 1 for a non-convergent optimization. This quiet approach to warnings ensures a non-disruptive experience for the user when predicting a large number of locations.

References
==========

Bean, Brennan, Yan Sun, and Marc Maguire. 2019a. “Interval-Valued Kriging Models for Geostatistical Mapping with Uncertain Inputs.”

———. 2019b. “Supplement to ‘Interval-Valued Kriging Models for Geostatistical Mapping with Uncertain Inputs’.”

Bivand, Roger S, Edzer J Pebesma, Virgilio Gomez-Rubio, and Edzer Jan Pebesma. 2013. *Applied Spatial Data Analysis with R*. 2nd ed. Vol. 747248717. Springer.

Eddelbuettel, Dirk. 2013. *Seamless R and C++ Integration with Rcpp*. New York: Springer. doi:[10.1007/978-1-4614-6868-4](https://doi.org/10.1007/978-1-4614-6868-4).

Eddelbuettel, Dirk, and James Joseph Balamuta. 2017. “Extending extitR with extitC++: A Brief Introduction to extitRcpp.” *PeerJ Preprints* 5 (August): e3188v1. doi:[10.7287/peerj.preprints.3188v1](https://doi.org/10.7287/peerj.preprints.3188v1).

Eddelbuettel, Dirk, and Romain François. 2011. “Rcpp: Seamless R and C++ Integration.” *Journal of Statistical Software* 40 (8): 1–18. doi:[10.18637/jss.v040.i08](https://doi.org/10.18637/jss.v040.i08).

Eddelbuettel, Dirk, and Conrad Sanderson. 2014. “RcppArmadillo: Accelerating R with High-Performance C++ Linear Algebra.” *Computational Statistics and Data Analysis* 71 (March): 1054–63. <http://dx.doi.org/10.1016/j.csda.2013.02.005>.

Goovaerts, Pierre. 1997. *Geostatistics for Natural Resources Evaluation*. Oxford University Press.

———. 2000. “Geostatistical Approaches for Incorporating Elevation into the Spatial Interpolation of Rainfall.” *Journal of Hydrology* 228 (1). Elsevier: 113–29.

Gräler, Benedikt, Edzer Pebesma, and Gerard Heuvelink. 2016. “Spatio-Temporal Interpolation Using Gstat.” *The R Journal* 8 (1): 204–18. <https://journal.r-project.org/archive/2016-1/na-pebesma-heuvelink.pdf>.

Pebesma, Edzer J. 2004. “Multivariable Geostatistics in S: The Gstat Package.” *Computers & Geosciences* 30: 683–91.

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. *Devtools: Tools to Make Developing R Packages Easier*. <https://github.com/r-lib/devtools>.
