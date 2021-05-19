library(mapsf)
library(rgdal)
library(sf)
shape <- readOGR(dsn = ".", layer = "mayotte")
coord <- coord_depth[,c(4:3)]
coords_site <- SpatialPointsDataFrame(coords=coord[,1:2],data = coord,
                                      proj4string=CRS(proj4string(shape)))


mtq <-st_as_sf(shape)
mtq <-st_transform(mtq, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

coords_site <- st_as_sf(coords_site)
coords_site <-st_transform(coords_site, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


#coords_site <- sf::st_as_sf(coord, coords = c("longitude","latitude"))
#coords_site
#coords_site <-st_transform(coords_site, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))







mtq$var <- NA
for (i in 1:nrow(mtq)){
  if(mtq$land[i]==1)     mtq$var[i] <- "land"     
  if(mtq$land[i]==0)     mtq$var[i] <- "sea"    
  if(mtq$reef[i]==1)     mtq$var[i] <- "reef"    
}

mf_theme("dark")
mf_map(x = mtq, var = "var", type = "typo",
       pal = c("chartreuse3","chocolate3","#95D8EB"),leg_title ="")


mf_map(x = coords_site, 
       col="red",add=T,pch=21,cex=1.5,fill="black")

# Start an inset map
mf_inset_on(x = "worldmap", pos = "right")
# Plot the position of the sample dataset on a worlmap
mf_worldmap(lat = -12.831620255852416, lon= 45.15714512616048, col = "#0E3F5C")
# Close the inset
mf_inset_off()
# Plot a title
mf_title("Mayotte Sampling Sites")
# Plot a north arrow
mf_arrow('topleft')
# Plot a scale bar
mf_scale(size = 5,lwd=2,cex=1.5)







