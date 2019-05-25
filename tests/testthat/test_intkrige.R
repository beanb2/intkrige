context("Intkrige predictions")
library(intkrige)

test_that("prediction for sample case is as expected", {
  data(utsnow)
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
  check <- gstat::vgm(model = c("Sph", "Sph", "Gau"))
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
  expect_equal(as.vector(preds), c(-0.06162802, 0.64828781, 0.58574939, 0.00000000))
})
