data(utsnow)
sp::coordinates(utsnow) <- c("LONGITUDE", "LATITUDE")
utsnow2 <- utsnow
intkrige::interval(utsnow) <- c("minDL", "maxDL")
intkrige::interval(utsnow) <- log(intkrige::interval(utsnow))
plot(utsnow)

summary(utsnow)

class(utsnow2)

x1 <- intvariogram(utsnow,
                   formulas = list(center ~ ELEVATION,
                                   radius*(ELEVATION/median(ELEVATION)) ~ 1))
test <- fit.intvariogram(x1, models = gstat::vgm(c("Sph", "Sph", "Gau")))

intvCheck(x1, test)

intkrige::interval(utsnow) <- NA
intkrige::interval(utsnow) <- c("minDL", "maxDL")

plot(x1[x1$id == "radius", ], model = test[[2]])

head(utsnow, 5)


data(meuse.grid)
coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
gridded(meuse.grid) <- TRUE # promote to SpatialPixelsDataFrame
x = as(meuse.grid, "SpatialGridDataFrame") # creates the full grid
interval(meuse.grid) <- c("part.a", "part.b")


sp::spplot(utsnow, zcol = utsnow@interval[, 1])



meuse.grid

mtcars$transmission <- factor(mtcars$am, levels=c(0, 1),
                              labels=c("Automatic", "Manual"))
colors = c("red", "blue")
lines = c(1,2) #1
points = c(16,17)

key.trans <- list(title="Radius",
                  points=list(pch=16, cex = c(1, 2), col="black"),
                  text=list(levels(mtcars$transmission)),
                  cex.title=1.25)
legend = list(right = list(fun = draw.key(key.trans)))

test <- sp::spplot(utsnow, zcol = "pointDL",
           cex = seq(from = 1, to = 3, length = nrow(utsnow)),
           alpha = 0.5, pch = "+",
           legend = legend,
           pretty = TRUE, key.space = "left",
           auto.key = list(title = "center"), cuts = 8)
test


