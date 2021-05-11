





##########################################################################################
#--- At the video scale ---
species_video_scale <- species.site.matrix$species.matrix
species_video_scale <- species_video_scale[,which(colnames(species_video_scale)!="unknown_fish")]

hab_pc_video_scale <- habit.score
rownames(hab_pc_video_scale) <- rownames(species_video_scale)

species_video_scale <- species_video_scale[apply(species_video_scale,1,sum)>0,]
hab_pc_video_scale <- hab_pc_video_scale[rownames(hab_pc_video_scale) %in% rownames(species_video_scale),]

hab_pc_video_scale <- merge(hab_pc_video_scale,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth")]),by.x="row.names",by.y="Sample.name") 


hab_pc_video_scale$classDepth <- NA

for (i in 1: nrow(hab_pc_video_scale)){
  if(hab_pc_video_scale$depth[i]<20){ hab_pc_video_scale$classDepth[i] <- "[0-20["}
  if(hab_pc_video_scale$depth[i]>=20 & hab_pc_video_scale$depth[i]<40){ hab_pc_video_scale$classDepth[i] <- "[20-40["}
  if(hab_pc_video_scale$depth[i]>=40 & hab_pc_video_scale$depth[i]<60){ hab_pc_video_scale$classDepth[i] <- "[40-60["}
  if(hab_pc_video_scale$depth[i]>=60 & hab_pc_video_scale$depth[i]<80){ hab_pc_video_scale$classDepth[i] <- "[60-80["}
  if(hab_pc_video_scale$depth[i]>=80){ hab_pc_video_scale$classDepth[i] <- ">80"}
  
}

hab_pc_video_scale <- merge(hab_pc_video_scale,sites,by.x="Sample.code",by.y="Sample_code")
for(i in 9:11){hab_pc_video_scale[,i] <-as.factor(hab_pc_video_scale[,i]) } 

### dbrda

dbrda.tot.pc <- capscale(species_video_scale ~ classDepth + PC1 + PC2 + PC3 + PC4, data = hab_pc_video_scale, distance = "jaccard")

# partial dbrda

dbrda.depth.pc <- capscale(species_video_scale ~ classDepth + Condition(PC1 + PC2 + PC3 + PC4), data = hab_pc_video_scale, distance = "bray")



# check results

summary(dbrda.tot.pc)

#aov <- anova(dbrda.tot.pc, permutations = 100 )#9999

RsquareAdj(dbrda.tot.pc)

#aov.axe <- anova(dbrda.tot.pc, by = "axis", permutations = 100)#9999

#aov.merg <- anova(dbrda.tot.pc, by = "margin", permutations = 100)#9999



#### plotting

## getting the scores for plotting

dbrda <- dbrda.depth.pc

scores_dbrda <- scores(dbrda)  # getting the scores from the analysis

site_scores <- scores_dbrda$sites     # separating out the site scores

species_video_scale_scores <- scores_dbrda$species_video_scale   # separating out the species_video_scale

species_video_scale_scores <- data.frame(species_video_scale_scores)

# percentage variability explained by axes

sumdbrda <- summary(dbrda)

CAP1 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP1"]*100, 1)

CAP2 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP2"]*100, 1)

R2 <- RsquareAdj(dbrda)$adj.r.squared

# combine in one df

site_scores_environment <- cbind(site_scores,hab_pc_video_scale) %>%   # merge
  
  droplevels()

# set correct factor level order

site_scores_environment$classDepth <- factor(site_scores_environment$classDepth, levels=c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80"))

site_scores_environment$island <- factor(site_scores_environment$island, levels=c("Europa","Juan_de_nova","Mayotte"))



# visualize in ggplot

formula <- as.character(dbrda$call$formula)[3]

pal <- hp(n = 5, house = "ronweasley2")

ggplot(site_scores_environment, aes(x= CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  
  geom_point(data = species_video_scale_scores, aes(x= CAP1,y = CAP2), cex = 0.5, col = "grey40") +  # species_video_scale scores
  
  geom_point(aes(pch = classDepth, fill = classDepth), cex = 4) + # site scores
  
  scale_fill_hp(discrete = TRUE, option = "HarryPotter", name = "classDepth",
                labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  #scale_fill_manual(values = pal,      # purplescale
  
  #name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  scale_shape_manual(values = c(23:19),
                     
                     name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  labs(x = paste0("CAP1 (", CAP1, "%)"), y = paste0("CAP2 (", CAP2, "%)"),
       
       title = paste0("species_video_scale ~ ", formula)) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "black"),
        
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        
        panel.border = element_blank(),panel.background = element_blank())









##################################################################
###############FOR NOW NOT NECESSARY##############################
##################################################################




##########################################################################################
#--- At the station scale ---
station <- read.csv2("station.code.bubot.csv",sep=",",header = T)
colnames(station) <- c("Sample.code", "island","station")


species_station_scale <- species.site.matrix$species.matrix
species_station_scale <- species_station_scale[,which(colnames(species_station_scale)!="unknown_fish")]

hab_pc_station_scale <- habit.score
rownames(hab_pc_station_scale) <- rownames(species_station_scale)

hab_pc_station_scale <- merge(hab_pc_station_scale,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth")]),by.x="row.names",by.y="Sample.name") 
hab_pc_station_scale <- hab_pc_station_scale[,-1]
hab_pc_station_scale<- aggregate(. ~ Sample.code, data = hab_pc_station_scale, mean)
hab_pc_station_scale <- merge(hab_pc_station_scale,station,by="Sample.code",all.x = T)
rownames(hab_pc_station_scale) <- hab_pc_station_scale[,1]
hab_pc_station_scale<- hab_pc_station_scale[,-1]

hab_pc_station_scale$classDepth <- NA

for (i in 1: nrow(hab_pc_station_scale)){
  if(hab_pc_station_scale$depth[i]<20){ hab_pc_station_scale$classDepth[i] <- "[0-20["}
  if(hab_pc_station_scale$depth[i]>=20 & hab_pc_station_scale$depth[i]<40){ hab_pc_station_scale$classDepth[i] <- "[20-40["}
  if(hab_pc_station_scale$depth[i]>=40 & hab_pc_station_scale$depth[i]<60){ hab_pc_station_scale$classDepth[i] <- "[40-60["}
  if(hab_pc_station_scale$depth[i]>=60 & hab_pc_station_scale$depth[i]<80){ hab_pc_station_scale$classDepth[i] <- "[60-80["}
  if(hab_pc_station_scale$depth[i]>=80){ hab_pc_station_scale$classDepth[i] <- ">80"}
  
}


for(i in 7:9){hab_pc_station_scale[,i] <-as.factor(hab_pc_station_scale[,i]) } 


species_station_scale <- species.site.matrix$species.matrix
species_station_scale <- merge(species_station_scale,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code")]),by.x="row.names",by.y="Sample.name") 
species_station_scale <- species_station_scale[,-1]
species_station_scale <- aggregate(. ~ Sample.code, data = species_station_scale, sum)
rownames(species_station_scale) <- species_station_scale[,1]
species_station_scale <- species_station_scale[,-1]
species_station_scale <- species_station_scale[,which(colnames(species_station_scale)!="unknown_fish")]
species_station_scale <- species_station_scale[apply(species_station_scale,1,sum)>0,]
hab_pc_station_scale  <- hab_pc_station_scale[rownames(hab_pc_station_scale) %in% rownames(species_station_scale) ,]

species_station_scale <- merge(species_station_scale,data.frame(species.site.matrix$site.data[,c("Sample.code","depth")]),by.x="row.names",by.y="Sample.code") 
colnames(species_station_scale)[1] <- "Sample.code"
species_station_scale<-  merge(species_station_scale,station,by="Sample.code",all.x = T)

species_station_scale$classDepth <- NA

for (i in 1: nrow(species_station_scale)){
  if(species_station_scale$depth[i]<20){ species_station_scale$classDepth[i] <- "[0-20["}
  if(species_station_scale$depth[i]>=20 & species_station_scale$depth[i]<40){ species_station_scale$classDepth[i] <- "[20-40["}
  if(species_station_scale$depth[i]>=40 & species_station_scale$depth[i]<60){ species_station_scale$classDepth[i] <- "[40-60["}
  if(species_station_scale$depth[i]>=60 & species_station_scale$depth[i]<80){ species_station_scale$classDepth[i] <- "[60-80["}
  if(species_station_scale$depth[i]>=80){ species_station_scale$classDepth[i] <- ">80"}
  
}

species_station_scale <-species_station_scale[,-c(1,321,320)]
for(i in 319:320){species_station_scale[,i] <-as.factor(species_station_scale[,i]) } 

species_station_scale <- aggregate(.~station+classDepth, species_station_scale, sum)
rownames(species_station_scale) <- paste0(species_station_scale$station,"_",species_station_scale$classDepth)  
species_station_scale <- species_station_scale[,-c(1,2)]

hab_pc_station_scale <- aggregate(.~station+classDepth, hab_pc_station_scale, mean)
rownames(hab_pc_station_scale) <- paste0(hab_pc_station_scale$station,"_",hab_pc_station_scale$classDepth)  


### dbrda

dbrda.tot.pc <- capscale(species_station_scale ~ classDepth + PC1 + PC2 + PC3 + PC4, data = hab_pc_station_scale, distance = "jaccard")

# partial dbrda

dbrda.depth.pc <- capscale(species_station_scale ~ classDepth + Condition(PC1 + PC2 + PC3 + PC4 + island), data = hab_pc_station_scale, distance = "jaccard")


# check results

summary(dbrda.tot.pc)

#aov <- anova(dbrda.tot.pc, permutations = 100 )#9999

RsquareAdj(dbrda.tot.pc)

#aov.axe <- anova(dbrda.tot.pc, by = "axis", permutations = 100)#9999

#aov.merg <- anova(dbrda.tot.pc, by = "margin", permutations = 100)#9999



#### plotting

## getting the scores for plotting

dbrda <- dbrda.depth.pc

scores_dbrda <- scores(dbrda)  # getting the scores from the analysis

station_scores <- scores_dbrda$sites     # separating out the station scores

species_station_scale_scores <- scores_dbrda$species_station_scale   # separating out the species_station_scale

species_station_scale_scores <- data.frame(species_station_scale_scores)

# percentage variability explained by axes

sumdbrda <- summary(dbrda)

CAP1 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP1"]*100, 1)

CAP2 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP2"]*100, 1)

R2 <- RsquareAdj(dbrda)$adj.r.squared

# combine in one df

station_scores_environment <- cbind(station_scores,hab_pc_station_scale) %>%   # merge
  
  droplevels()

# set correct factor level order

station_scores_environment$classDepth <- factor(station_scores_environment$classDepth, levels=c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80"))

station_scores_environment$island <- factor(station_scores_environment$island, levels=c("Europa","Juan_de_nova","Mayotte"))



# visualize in ggplot

formula <- as.character(dbrda$call$formula)[3]

ggplot(station_scores_environment, aes(x= CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  
  geom_point(data = species_station_scale_scores, aes(x= CAP1,y = CAP2), cex = 0.5, col = "grey40") +  # species_station_scale scores
  
  geom_point(aes(pch = classDepth, fill = classDepth), cex = 4) + # station scores
  
  geom_encircle(aes(group = classDepth, linetype =classDepth,fill=classDepth), s_shape = 1, expand = 0,size=1,
                alpha = 0.2, show.legend = FALSE) +
  
  scale_fill_hp(discrete = TRUE, option = "HarryPotter", name = "classDepth",
                labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  scale_shape_manual(values = c(25:21),
                     
                     name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  labs(x = paste0("CAP1 (", CAP1, "%)"), y = paste0("CAP2 (", CAP2, "%)"),
       
       title = paste0("species_station_scale ~ ", formula)) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "black"),
        
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        
        panel.border = element_blank(),panel.background = element_blank())












# PHYLOGENY


ape::write.tree(set_fish, file="tree.txt") # 1°/ you need to make a .txt file in a newick format
tree<-paste(readLines('tree.txt')) # 2°/ you need to read it as a character string
tree_phylog<-ade4::newick2phylog(tree) # 3°/ newick2phylog{ade4} 
# force.ultrametric()
# On perd 72 esp 
biomass_mat_phylo <- biomass_mat[,colnames(biomass_mat) %in% names(tree_phylog$leaves)]
biomass_mat_phylo <- biomass_mat_phylo[apply(biomass_mat_phylo,1,sum)>4,]
biomass_mat_phylo <- biomass_mat_phylo[,apply(biomass_mat_phylo,1,sum)>0]

biomass_mat_phylo <- biomass_mat_phylo/apply(biomass_mat_phylo,1,sum)

alpha_beta_hill_phylo <- chao_alpha_beta(matrix = biomass_mat_phylo,q=c(0,1,2), tree_phylog = tree_phylog)




alpha_hill_all <- merge(alpha_hill_all, 
                        data.frame(alpha_beta_hill_phylo$alpha_phylo[,c(1,2)]), by="row.names", all.x = T)
rownames(alpha_hill_all) <- alpha_hill_all[,1]
alpha_hill_all <- alpha_hill_all[,-1]





beta_hill_phylo_richess_t <- reshape::melt(as.matrix(alpha_beta_hill_phylo$beta_phylo$q0))[melt(upper.tri(as.matrix(alpha_beta_hill_phylo$beta_phylo$q0)))$value,]

beta_hill_phylo_entropy_t <- reshape::melt(as.matrix(alpha_beta_hill_phylo$beta_phylo$q1))[melt(upper.tri(as.matrix(alpha_beta_hill_phylo$beta_phylo$q1)))$value,]


beta_hill_phylo <- data.frame(pairsID = paste0(beta_hill_phylo_entropy_t[,1], "__",
                                               beta_hill_phylo_entropy_t[,2]),
                              beta_hill_phylo_richess  = beta_hill_phylo_richess_t[,3],
                              beta_hill_phylo_entropy = beta_hill_phylo_entropy_t[,3])

beta_hill <- merge(beta_hill,beta_hill_phylo,by="pairsID", all.x =T)









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
#Plot distance  en fonction des profondeurs
coord_depth <- species.site.matrix$site.data[,c(2,5:7)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]

coord_depth <- coord_depth[rownames(coord_depth) %in% rownames(abumat),]

coord <- coord_depth[,-2]
colnames(coord) <- c("name","lat","lon")
geodist <-  as.matrix(round(GeoDistanceInMetresMatrix(coord) / 1000,3))
depthdist <- as.matrix(dist(coord_depth[,2],"euclidean"))
colnames(depthdist)<- rownames(coord_depth)
rownames(depthdist)<- rownames(coord_depth)

depthdist_t <- reshape::melt(as.matrix(depthdist))[melt(upper.tri(as.matrix(depthdist)))$value,]
geodist_t  <- reshape::melt(as.matrix(geodist))[melt(upper.tri(as.matrix(geodist)))$value,]

beta_hill_all <- data.frame(X1                      = beta_hill_taxo_richess_t[,1],
                            X2                      = beta_hill_taxo_richess_t[,2],
                            beta_hill_taxo_richess  = beta_hill_taxo_richess_t[,3],
                            beta_hill_taxo_entropy  = beta_hill_taxo_entropy_t[,3],
                            beta_hill_fonct_richess = beta_hill_fonct_richess_t[,3],
                            beta_hill_fonct_entropy = beta_hill_fonct_entropy_t[,3],
                            depthdist               = depthdist_t[,3],
                            geodist                 = geodist_t[,3])

BetaFD_q0 <- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_taxo_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo richness")

BetaFD_q1<- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_taxo_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo entropy")

BetaFD_q0.1<- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_fonct_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc richness")

BetaFD_q01.1<- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_fonct_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc entropy")

grid.arrange(BetaFD_q0,BetaFD_q0.1,BetaFD_q1,BetaFD_q01.1)











##Add Column Pairs comparsion
beta_hill_all$island1 <- NA
beta_hill_all$island2 <- NA

for(i in 1:nrow(beta_hill_all)){
  print(i)
  beta_hill_all$island1[i] <- as.character(unique(dat_complet[dat_complet$Sample.code %in% beta_hill_all[i,1],]$island))
  beta_hill_all$island2[i] <- as.character(unique(dat_complet[dat_complet$Sample.code %in% beta_hill_all[i,2],]$island))
}
beta_hill_all$pairsIsland <- paste0(beta_hill_all$island1,"_",beta_hill_all$island2)

beta_hill_all_mayotte <- subset(beta_hill_all,beta_hill_all$pairsIsland=="Mayotte_Mayotte")

BetaFD_q0 <- ggplot(beta_hill_all_mayotte,aes(x=depthdist,y=beta_hill_taxo_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo richness")

BetaFD_q1<- ggplot(beta_hill_all_mayotte,aes(x=depthdist,y=beta_hill_taxo_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo entropy")

BetaFD_q0.1<- ggplot(beta_hill_all_mayotte,aes(x=depthdist,y=beta_hill_fonct_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc richness")

BetaFD_q01.1<- ggplot(beta_hill_all_mayotte,aes(x=depthdist,y=beta_hill_fonct_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc entropy")

grid.arrange(BetaFD_q0,BetaFD_q0.1,BetaFD_q1,BetaFD_q01.1)

plot(beta_hill_all_mayotte$beta_hill_taxo_richess,beta_hill_all_mayotte$beta_hill_taxo_entropy)
plot(beta_hill_all_mayotte$beta_hill_fonct_richess,beta_hill_all_mayotte$beta_hill_fonct_entropy)



###########################

beta_hill_all_Juan_de_nova <- subset(beta_hill_all,beta_hill_all$pairsIsland=="Juan_de_nova_Juan_de_nova")

BetaFD_q0 <- ggplot(beta_hill_all_Juan_de_nova,aes(x=depthdist,y=beta_hill_taxo_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo richness")

BetaFD_q1<- ggplot(beta_hill_all_Juan_de_nova,aes(x=depthdist,y=beta_hill_taxo_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo entropy")

BetaFD_q0.1<- ggplot(beta_hill_all_Juan_de_nova,aes(x=depthdist,y=beta_hill_fonct_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc richness")

BetaFD_q01.1<- ggplot(beta_hill_all_Juan_de_nova,aes(x=depthdist,y=beta_hill_fonct_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc entropy")

grid.arrange(BetaFD_q0,BetaFD_q0.1,BetaFD_q1,BetaFD_q01.1)


###########################
beta_hill_all_Europa <- subset(beta_hill_all,beta_hill_all$pairsIsland=="Europa_Europa")

BetaFD_q0 <- ggplot(beta_hill_all_Europa,aes(x=depthdist,y=beta_hill_taxo_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo richness")

BetaFD_q1<- ggplot(beta_hill_all_Europa,aes(x=depthdist,y=beta_hill_taxo_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo entropy")

BetaFD_q0.1<- ggplot(beta_hill_all_Europa,aes(x=depthdist,y=beta_hill_fonct_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc richness")

BetaFD_q01.1<- ggplot(beta_hill_all_Europa,aes(x=depthdist,y=beta_hill_fonct_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc entropy")

grid.arrange(BetaFD_q0,BetaFD_q0.1,BetaFD_q1,BetaFD_q01.1)






#DECAY PHYLO

ResHill[j,9] <- mean(beta_hill_phylo_richess[rownames(beta_hill_phylo_richess) %in% rownames(From10toInfdepth[j,]),
                                             colnames(beta_hill_phylo_richess) %notin% rownames(From10toInfdepth[j,])])

ResHill[j,10] <- sd(beta_hill_phylo_richess[rownames(beta_hill_phylo_richess) %in% rownames(From10toInfdepth[j,]),
                                            colnames(beta_hill_phylo_richess) %notin% rownames(From10toInfdepth[j,])])

ResHill[j,11] <- mean(beta_hill_phylo_entropy[rownames(beta_hill_phylo_entropy) %in% rownames(From10toInfdepth[j,]),
                                              colnames(beta_hill_phylo_entropy) %notin% rownames(From10toInfdepth[j,])])

ResHill[j,12] <- sd(beta_hill_phylo_entropy[rownames(beta_hill_phylo_entropy) %in% rownames(From10toInfdepth[j,]),
                                            colnames(beta_hill_phylo_entropy) %notin% rownames(From10toInfdepth[j,])])




##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
hab_selec<-cbind(rownames(Hab),Hab[,c(1,2,5)])
hab_selec[,2]<-as.numeric(hab_selec[,2])
hab_selec[,3]<-as.numeric(hab_selec[,3])
colnames(hab_selec)<-c("site","Lat", "Long", "Prof")

GDM_results<-matrix(NA,3,6)
rownames(GDM_results)<-c("tot","turn","nest")
colnames(GDM_results)<-c("site","Lat", "Long", "Houle","Prof","Habi","Ile")


hab_selec<- cbind(rownames(hab_pc_site_scale),hab_pc_site_scale[,c(1:4,6)])
colnames(hab_selec)<-c("site","PC1","PC2","PC3","PC4","depth")
hab_selec<-merge(hab_selec,unique(species.site.matrix$site.data[,c("Sample.code","latitude", "longitude")]),by.x="site",by.y="Sample.code",all.x=T)
colnames(hab_selec)<-c("site","PC1","PC2","PC3","PC4","depth","Lat", "Long")
############TOTAL#####################################
all_beta$beta.jtu <- as.data.frame(as.matrix(all_beta$beta.jtu))

dat <- na.omit(as.data.frame(as.matrix(all_beta$beta.jtu)))
dat <-dat[,colnames(dat) %in% rownames(dat)]

test <- cbind(rownames(dat), dat)
colnames(test)[1] <- "site"
hab_selec <- hab_selec[hab_selec$site %in% test$site,]

exFormat3 <- formatsitepair(test,3, 
                            XColumn="Long", YColumn="Lat",
                            siteColumn="site",predData=hab_selec)
Mod <- gdm(exFormat3, geo=T)

plot(Mod, plot.layout = c(3, 3))

############TURN#####################################
bob<-anosim (as.dist(all_beta$beta.jac) ~  PC1*PC2*PC3*PC4*depth,data = hab_selec)
bob2<-adonis2(as.dist(BETAX$beta.jac) ~ Houle*Prof*Habi*Ile,data = hab_selec)
BETAX
plot(bob)


load(system.file("./data/gdm.RData", package="gdm"))
sppData <- gdmExpData[, c(1,2,13,14)]
envTab <- gdmExpData[, c(2:ncol(gdmExpData))]


site <- unique(sppData$site)
gdmDissim <- cbind(site, gdmDissim)
exFormat3 <- formatsitepair(gdmDissim, 3, XColumn="Long", YColumn="Lat", predData=envTab,
                            siteColumn="site")


############TOTAL#####################################
exFormat3 <- formatsitepair(as.matrix(all_beta$beta.jac),3, 
                            predData=hab_selec, XColumn="Long", YColumn="Lat",
                            siteColumn="site")
Mod <- gdm(exFormat3, geo=T)
###########################################################################################
#################### start to do some mix model analyses ##################################
###########################################################################################

library("lme4")
library("plyr")
############################### GLM mixte ########################################

#At the video scale


# combine the data in a dataframe to make it easier
number.dat=data.frame(number.ind,
                      depth=as.numeric(species.site.matrix$site.data$depth),
                      habitat.substrat=habit.mca$ind$coord[,2],
                      habitat.bicenose=habit.mca$ind$coord[,1],
                      Island=island.name,
                      site=site.name,
                      replicate=species.site.matrix$site.data$Sample.code,
                      subreplicate=species.site.matrix$site.data$Sample.name)


number.dat <- merge(hab_pc_video_scale,number.dat)
#################################################################
############### cleaning dataset ################################
#################################################################

####### check replication with count function
# summary number of replicates
repl=count(number.dat$replicate)
# get a list of sample with replication
to.select=repl[!repl$freq==1,1]

# remove sample without replication
new.number.dat=number.dat[!is.na(match(number.dat$replicate,to.select)),]

# remove juan de nova data
new.number.dat=new.number.dat[!new.number.dat$Island=="Juan_de_nova",]
new.number.dat$Island

