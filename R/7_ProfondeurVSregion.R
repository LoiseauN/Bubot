
library(reshape)
library(ggplot2)

#Plot distance decay en fonction des profondeurs
coord_depth <- species.site.matrix$site.data[,c(2,5:7)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]
#--

coord_depth <- coord_depth[rownames(coord_depth) %in% rownames(abumat),]

coord <- coord_depth[,-2]
colnames(coord) <- c("name","lat","lon")
geodist <-  as.matrix(round(GeoDistanceInMetresMatrix(coord) / 1000,3))
depthdist <- as.matrix(dist(coord_depth[,2],"euclidean"))
colnames(depthdist)<- rownames(coord_depth)
rownames(depthdist)<- rownames(coord_depth)



#Compute beta within depth cate

classDepth <- unique(hab_pc_site_scale$classDepth)


beta_classdepth <- do.call(rbind,lapply(1:length(classDepth), function(i){
   
   beta<-beta.pair(species_site_scale0_1[rownames(subset(hab_pc_site_scale,hab_pc_site_scale$classDepth==classDepth[i])),],index.family = "jaccard")
   
   jac <- reshape::melt(as.matrix(beta$beta.jac))[melt(upper.tri(as.matrix(beta$beta.jac)))$value,]
   jtu <- reshape::melt(as.matrix(beta$beta.jtu))[melt(upper.tri(as.matrix(beta$beta.jtu)))$value,]
   jne <- reshape::melt(as.matrix(beta$beta.jne))[melt(upper.tri(as.matrix(beta$beta.jne)))$value,]
   
   coord_classdepth   <- coord[rownames(coord) %in% rownames(as.matrix(beta$beta.jac)),]
   geodist_classdepth <-  as.matrix(round(GeoDistanceInMetresMatrix(coord_classdepth) / 1000,3))
   geodist_classdepth <- reshape::melt(geodist_classdepth)[melt(upper.tri(geodist_classdepth))$value,]
   names(geodist_classdepth) <- c("c1", "c2", "geodist")
   
   res <- data.frame(jac, 
                     jtu      = jtu[,3],
                     jne      = jne[,3],
                     depthcat = rep(classDepth[i],nrow(jac)),
                     geodist  = geodist_classdepth[,3])
   return(res)
 })

)

ggplot(beta_classdepth, aes(jtu, depthcat, color = depthcat)) +
  geom_boxplot() +
  theme_minimal() 


ggplot(beta_classdepth, aes(jtu, geodist, color = depthcat)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

ggplot(beta_classdepth, aes(x = jtu, y = geodist,colour=depthcat)) +
  geom_point() +
  facet_wrap(~depthcat)+theme_bw()+
  scale_color_viridis_d(direction = -1)



#---


jaccard <- reshape::melt(jaccard)[melt(upper.tri(jaccard))$value,]
names(jaccard) <- c("c1", "c2", "jaccard")

turnover <- reshape::melt(as.matrix(all_beta$beta.jtu))[melt(upper.tri(as.matrix(all_beta$beta.jtu)))$value,]
names(turnover) <- c("c1", "c2", "turnover")

nested <- reshape::melt(as.matrix(all_beta$beta.jne))[melt(upper.tri(as.matrix(all_beta$beta.jne)))$value,]
names(nested) <- c("c1", "c2", "nested")


#---
depthdist <- reshape::melt(depthdist)[melt(upper.tri(depthdist))$value,]
names(depthdist) <- c("c1", "c2", "depthdiff")

#---

geodist <- reshape::melt(geodist)[melt(upper.tri(geodist))$value,]
names(geodist) <- c("c1", "c2", "geodist")

#---
compil <- data.frame(jaccard, 
                    turnover  = turnover[,3],
                    nested    = nested[,3],
                    depthdiff = depthdist[,3],
                    geodist = geodist[,3])


plot(compil$jaccard,compil$geodist)





ggplot(compil, aes(turnover, depthdiff, color = geodist)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

