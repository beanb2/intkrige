require(intkrige)
data(utsnow)
sp::coordinates(utsnow) <- c("LONGITUDE", "LATITUDE")
utsnow2 <- utsnow
intkrige::interval(utsnow) <- c("minDL", "maxDL")
intkrige::interval(utsnow) <- log(intkrige::interval(utsnow))
plot(utsnow)

summary(utsnow)
print(utsnow)
head(utsnow)
tail(utsnow)

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
meuse.grid$part.c <- meuse.grid$part.a + 10
interval(meuse.grid) <- c("part.b", "part.c")

interval(meuse.grid) <- NULL
tempRas <- as(meuse.grid, "RasterLayer")

tempRas <- raster::aggregate(tempRas, 8)

duh <- raster::res(tempRas)

tempRas <- as(tempRas, "SpatialPixelsDataFrame")
tempRas$part.b <- 10 + tempRas$part.a
interval(tempRas) <- c("part.a", "part.b")

plot(tempRas, beside = FALSE, circleCol = "blue", radPos = 4)



corner <- tempRas@grid@cellcentre.offset

rads <- 1:360*pi/180
tempcoords <- sp::coordinates(tempRas)
ll <- nrow(tempcoords)
templines <- vector("list", ll)
for(i in 1:ll){
  x <- tempcoords[i, 1] + (320/2)*cos(rads)
  y <- tempcoords[i, 2] + (320/2)*sin(rads)

  templines[[i]] <- Lines(Line(cbind(x, y)), ID=as.character(i))
}

# Create legend entry.
xleg <- tempRas@bbox[1, 2] - (320/2) + (320/2)*cos(rads)
yleg <- tempRas@bbox[2, 1] + (320/2) + (320/2)*sin(rads)

radLeg <- SpatialLines(list(Lines(Line(cbind(xleg, yleg)), ID="legend")))

tester <- SpatialLines(templines)

p1 <- sp::spplot(tempRas)

p1 +
  latticeExtra::layer(sp.lines(tester, col = "green")) +
  latticeExtra::layer(sp.lines(radLeg, col = "green")) +
  latticeExtra::layer(sp.text(matrix(c(tempRas@bbox[1, 2] - (320/2),
                                tempRas@bbox[2, 1] + (320/2)),
                                ncol = 2),
                              txt = as.character(round(max(tempRas@coords), 3))))




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


