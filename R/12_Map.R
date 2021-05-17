

require(rgdal)

#Parametre
color_ocean   <- "#95D8EB"



shape <- readOGR(dsn = ".", layer = "mayotte")

reef <- subset(shape,shape$reef==1)
land <- subset(shape,shape$land==1)
sea <- subset(shape,shape$land==0)
coord <- coord_depth[,c(4:3)]
coords_site <- SpatialPointsDataFrame(coords=coord[,1:2],data = coord,
                                      proj4string=CRS(proj4string(shape)))

head(shape@data)

png(
  file      = here::here("figures", paste0(figname, ".png")),
  width     = 24.00,
  height    = 15.75,
  units     = "in",
  res       = 600,
  pointsize = 18
)

par(
  xaxs     = "i",
  yaxs     = "i",
  family   =  "serif",
  mar      = rep(1, 4),
  cex.axis = 1.25,
  mgp      = c(2, .5, 0),
  tcl      = -0.25,
  xpd      = FALSE,
  new      = FALSE,
  fig      = c(0, 1, 0, 1),
  col      = "#666666",
  col.axis = "#666666",
  fg       = "#666666",
  mfcol    = c(3, 2)
)

## Plot Ocean Background ----
border <- as(raster::extent(c(44.85, 45.35, -13.17628, -12.41611)), "SpatialPolygons")
sp::proj4string(border) <- proj4string(shape)

plot(border, border = T, col = color_ocean)

grd <- list()
grd[[1]] <- data.frame(x=
                         y=)

axis(
  side     = 3,
  at       = grd[[1]][ , "x"],
  labels   = paste0(grd[[1]][ , "label"], "°", grd[[1]][ , "direction"]),
  cex.axis = 0.75,
  lwd      = 0
)

plot(shape, 
     main = "Sampling Sites", add = TRUE)

grd <- addGraticules(add = TRUE)

par(mgp = c(0.5, -0.1, 0))
axis(
  side     = 1,
  at       = grd[[1]][ , "x"],
  labels   = paste0(grd[[1]][ , "label"], "°", grd[[1]][ , "direction"]),
  cex.axis = 0.75,
  lwd      = 0
)

par(mgp = c(0.5, -0.45, 0))
axis(
  side     = 4,
  at       = grd[[2]][ , "y"],
  labels   = paste0(grd[[2]][ , "label"], "°", grd[[2]][ , "direction"]),
  cex.axis = 0.75,
  lwd      = 0
)

par(mgp = c(0.5, 0, 0))
axis(
  side     = 3,
  at       = grd[[1]][ , "x"],
  labels   = paste0(grd[[1]][ , "label"], "°", grd[[1]][ , "direction"]),
  cex.axis = 0.75,
  lwd      = 0
)

par(mgp = c(0.5, -0.35, 0))
axis(
  side     = 2,
  at       = grd[[2]][ , "y"],
  labels   = paste0(grd[[2]][ , "label"], "°", grd[[2]][ , "direction"]),
  cex.axis = 0.75,
  lwd      = 0
)


plot(shape, 
     main = "Sampling Sites", add = TRUE)
plot(land, 
     add = TRUE,
     col = "#458B0046")
plot(sea, 
     add = TRUE,
     col = "#95D8EB")
plot(reef, 
     add = TRUE,
     col = "#FF725646")
plot(coords_site, 
     add = TRUE,
     col = "red",pch= 21)


## Add Figure Box ----

par(xpd = TRUE)

plot(border, border = par()$col, lwd = 4, add = TRUE)
plot(border, border = "white", lwd = 2, add = TRUE)

par(xpd = FALSE)

