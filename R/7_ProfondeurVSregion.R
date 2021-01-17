library(reshape2)

#Plot distance decay en fonction des profondeurs
coord_depth <- species.site.matrix$site.data[,c(2,5:7)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]



coord <- coord_depth[,-2]
colnames(coord) <- c("name","lat","lon")
geodist <- round(GeoDistanceInMetresMatrix(coord) / 1000,3)
depthdist <- dist(coord_depth[,-2],"euclidean")
all_beta


m <- data.frame(t(combn(rownames(x),2)), as.numeric(d))
names(m) <- c("c1", "c2", "distance")


library(reshape)
 m <- as.matrix(all_beta$beta.jac)

 m2 <- reshape::melt(m)[melt(upper.tri(m))$value,]
names(m2) <- c("c1", "c2", "distance")

melt_dist((all_beta$beta.jac))
  
  Melt a square distance matrix into long format
In harrietr: Wrangle Phylogenetic Distance Matrices and Other Utilities 