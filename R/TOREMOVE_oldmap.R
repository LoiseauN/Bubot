
require(rgdal)

#Parametre
#color_ocean   <- "darkcyan" 
color_ocean   <- "#303946"

color_ocean   <- "#95D8EB" 
require(rgdal)
shape <- readOGR(dsn = ".", layer = "mayotte")
sp::proj4string(shape) <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

reef <- subset(shape,shape$reef==1)
land <- subset(shape,shape$land==1)
lagoon <- subset(shape,shape$land==0)
coord <- coord_depth[,c(4:3)]
coords_site <- SpatialPointsDataFrame(coords=coord[,1:2],data = coord,
                                      proj4string=CRS(proj4string(shape)))

head(shape@data)

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
        mfcol    = c(1, 1)
)
plot(border, border = T, col = color_ocean)

## Plot Ocean Background ----


border <- as(raster::extent(c(44.85, 45.35, -13.17628, -12.41611)), "SpatialPolygons")

sp::proj4string(border) <- proj4string(shape)

plot(border, border = T, col = color_ocean)

## Add Figure Box ----

par(xpd = TRUE)

plot(border, border = par()$col, lwd = 4, add = TRUE)
plot(border, border = "white", lwd = 2, add = TRUE)

par(xpd = FALSE)

## Add map  ----
plot(shape, 
     main = "Sampling Sites", add = TRUE)
plot(lagoon, 
     add = TRUE,
     col = "#1C3E5A")#95D8EB
plot(land, 
     add = TRUE,
     col = "#42D185")#chartreuse3
plot(reef, 
     add = TRUE,
     col = "coral2")#
plot(coords_site, 
     add = TRUE,
     col = "red",pch= 21,cex=1.8)


## Add axis Box ----
grd <- addGraticules(prj        = "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
                     parallels  = seq(-13.17628, -12.41611, length.out = 8) ,
                     meridians  = seq( 44.85,  45.35, length.out = 8),
                     line.color = "#aaaaaa",
                     line.size  =10,
                     line.type  = 1,
                     add        = FALSE
)

grd[[1]]$label <- round(grd[[1]]$label,2)
grd[[2]]$label <- round(grd[[2]]$label,2)

par(mgp = c(3, 6, 0))
axis(
        side     = 1,
        at       = grd[[1]][ , "label"],
        labels   = paste0(grd[[1]][ , "label"], "°", grd[[1]][ , "direction"]),
        cex.axis = 0.75,
        lwd      = 1)


axis(
        side     = 1,
        at       = grd[[1]][ , "label"],
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
        lwd      = 1
)




par(xpd = TRUE)

plot(border, border = par()$col, lwd = 4, add = TRUE)
plot(border, border = "white", lwd = 2, add = TRUE)

par(xpd = FALSE)

