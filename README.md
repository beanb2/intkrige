
<!-- README.md is generated from README.Rmd. Please edit that file -->
intkrige
========

This package implements the interval-valued kriging models described in Bean et. al. (2019a). Details regarding the algorithmic implementation can be found in Bean et. al. (2019b).

Installation
------------

This is a preliminary version of the intkrige package that will be submitted to CRAN. The public package will include additional user functions and visualization tools designed to simplify and enhance interval-valued spatial data analysis. This developmental version of the pacakge can be installed by setting the parent folder as your source file location and invoking the command.

``` r
devtools::install()
```

Note that the developmental installation makes use of the devtools package (Wickham, Hester, and Chang 2019). The most current version of this package can be found at .

Package Contents
----------------

The current version of the package contains one main function named intkrige. Information about this package can be obtained by entering

``` r
?intkrige::intkrige
```

in the console.

The package also contains an inter-valued design ground snow load datset named utsnow. This data is available by installing the intkrige package and invoking the command

``` r
data(utsnow)
```

Documentation for this dataset can be obtained through the command

``` r
?utsnow
```

Example
-------

Once installed, this package runs simple and ordinary interval-valued kriging models. The following example comes from the design ground snow load application described in Bean et. al. (Bean, Sun, and Maguire 2019a). The example shows how to make an interval valued kriging design ground snow load prediction (after removing the effect of elevation) for Logan, Utah.

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
pred_locs = matrix(c(lons, lats), ncol = 2)

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

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. *Devtools: Tools to Make Developing R Packages Easier*. <https://github.com/r-lib/devtools>.
