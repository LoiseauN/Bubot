library(mFD)
`%notin%` <- Negate(`%in%`)









###############################################################################
###############################################################################
#taxo richness : data= 0/1, q=0, tau=min (c'est la richesse spé)
#taxo entropy: data= relative biomass, q=1, tau=min (c'est exp(Shannon) )
#fonctio richness: data= 0/1, q=0, tau=mean
#fonctio entropy: data= 0/1, q=1, tau=mean


ape::write.tree(set_fish, file="tree.txt") # 1°/ you need to make a .txt file in a newick format
tree<-paste(readLines('tree.txt')) # 2°/ you need to read it as a character string
tree_phylog<-ade4::newick2phylog(tree) # 3°/ newick2phylog{ade4} 

# On perd 72 esp 
biomass_mat_phylo <- biomass_mat[,colnames(biomass_mat) %in% names(tree_phylog$leaves)]
biomass_mat_phylo <- biomass_mat_phylo[apply(biomass_mat_phylo,1,sum)>4,]
biomass_mat_phylo <- biomass_mat_phylo[,apply(biomass_mat_phylo,1,sum)>0]

biomass_mat0_1 <- biomass_mat
biomass_mat0_1[biomass_mat0_1>0] <- 1

alpha_beta_hill_phylo<-chao_alpha_beta(matrix = biomass_mat_phylo,q=c(0,1,2), tree_phylog = tree_phylog)

#alpha
alpha_hill_taxo_richess  <- mFD::alpha.fd.hill (asb_sp_w = biomass_mat0_1,
                                           sp_dist  = sp_dist_traits,
                                           q        = 0,
                                           tau      = "min")$asb_FD_Hill

alpha_hill_taxo_entropy  <- mFD::alpha.fd.hill (asb_sp_w = biomass_mat,
                                           sp_dist  = sp_dist_traits,
                                           q        = 1,
                                           tau      = "min")$asb_FD_Hill

alpha_hill_fonct_richess <- mFD::alpha.fd.hill (asb_sp_w = biomass_mat0_1,
                                           sp_dist  = sp_dist_traits,
                                           q        = 0,
                                           tau      = "mean")$asb_FD_Hill

alpha_hill_fonct_entropy <- mFD::alpha.fd.hill (asb_sp_w = biomass_mat,
                                           sp_dist  = sp_dist_traits,
                                           q        = 1,
                                           tau      = "mean")$asb_FD_Hill

alpha_hill_all <- data.frame(hill_taxo_richess  = alpha_hill_taxo_richess[,1],
                             hill_taxo_entropy  = alpha_hill_taxo_entropy[,1],
                             hill_fonct_richess = alpha_hill_fonct_richess[,1],
                             hill_fonct_entropy = alpha_hill_fonct_entropy[,1])


alpha_hill_all <- merge(alpha_hill_all, 
                        data.frame(alpha_beta_hill_phylo$alpha_phylo[,c(1,2)]), by="row.names", all.x = T)
rownames(alpha_hill_all) <- alpha_hill_all[,1]
alpha_hill_all <- alpha_hill_all[,-1]
colnames(alpha_hill_all) <- c("alpha_hill_taxo_richess","alpha_hill_taxo_entropy",
                              "alpha_hill_fonct_richess","alpha_hill_fonct_entropy",
                              "alpha_hill_phylo_richess","alpha_hill_phylo_entropy")


alpha_div_all <- merge(alpha_div_all,alpha_hill_all,by="row.names")
rownames(alpha_div_all) <- alpha_div_all[,1]
alpha_div_all <- alpha_div_all[,-c(1,2)]

colnames(alpha_div_all)[c(15:19)] <- c("PC1_hab","PC2_hab","PC3_hab","PC4_hab","PC5_hab")

save(alpha_div_all,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/alpha_div_all.RData")


#beta
beta_hill_taxo_richess  <- mFD::beta.fd.hill (asb_sp_w = biomass_mat0_1,
                                         sp_dist  = sp_dist_traits,
                                         q        = 0,
                                         tau      = "min",
                                         beta_type = "Jaccard")

beta_hill_taxo_entropy  <- mFD::beta.fd.hill (asb_sp_w = biomass_mat,
                                         sp_dist  = sp_dist_traits,
                                         q        = 1,
                                         tau      = "min",
                                         beta_type = "Jaccard")

beta_hill_fonct_richess <- mFD::beta.fd.hill (asb_sp_w = biomass_mat0_1,
                                         sp_dist  = sp_dist_traits,
                                         q        = 0,
                                         tau      = "mean",
                                         beta_type = "Jaccard")

beta_hill_fonct_entropy <- mFD::beta.fd.hill (asb_sp_w = biomass_mat,
                                         sp_dist  = sp_dist_traits,
                                         q        = 1,
                                         tau      = "mean",
                                         beta_type = "Jaccard")


beta_hill_taxo_richess_t <- reshape::melt(as.matrix(beta_hill_taxo_richess$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_taxo_richess$beta_fd_q$q0)))$value,]
beta_hill_taxo_entropy_t <- reshape::melt(as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1)))$value,]
beta_hill_fonct_richess_t <- reshape::melt(as.matrix(beta_hill_fonct_richess$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_fonct_richess$beta_fd_q$q0)))$value,]
beta_hill_fonct_entropy_t <- reshape::melt(as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1)))$value,]

beta_hill <- data.frame(site1 = beta_hill_taxo_richess_t[,1],
                        site2 = beta_hill_taxo_richess_t[,2],
                        beta_hill_taxo_richess = beta_hill_taxo_richess_t[,3],
                        beta_hill_taxo_entropy = beta_hill_taxo_entropy_t[,3],
                        beta_hill_fonct_richess = beta_hill_fonct_richess_t[,3],
                        beta_hill_fonct_entropy = beta_hill_fonct_entropy_t[,3],
                        pairsID = paste0(beta_hill_taxo_richess_t[,1], "__",
                                         beta_hill_taxo_richess_t[,2]))





beta_hill_phylo_richess_t <- reshape::melt(as.matrix(alpha_beta_hill_phylo$beta_phylo$q0))[melt(upper.tri(as.matrix(alpha_beta_hill_phylo$beta_phylo$q0)))$value,]

beta_hill_phylo_entropy_t <- reshape::melt(as.matrix(alpha_beta_hill_phylo$beta_phylo$q1))[melt(upper.tri(as.matrix(alpha_beta_hill_phylo$beta_phylo$q1)))$value,]


beta_hill_phylo <- data.frame(pairsID = paste0(beta_hill_phylo_entropy_t[,1], "__",
                                                 beta_hill_phylo_entropy_t[,2]),
                              beta_hill_phylo_richess  = beta_hill_phylo_richess_t[,3],
                              beta_hill_phylo_entropy = beta_hill_phylo_entropy_t[,3])

beta_hill <- merge(beta_hill,beta_hill_phylo,by="pairsID", all.x =T)



diff_depth <- data.frame(row.names = rownames(alpha_div_all),
                         depth=alpha_div_all$depth)


diff_depth  <- dist(diff_depth, method = "euclidean")                        
diff_depth <- reshape2::melt(as.matrix(diff_depth))[melt(upper.tri(as.matrix(diff_depth)))$value,]

diff_depth$pairsID <- paste0(diff_depth[,1], "__",
                             diff_depth[,2])          
diff_depth <- diff_depth[,-c(1,2)]     
beta_hill <- merge(beta_hill,diff_depth,by="pairsID",all.x = T)
rownames(beta_hill) <- beta_hill[,1]
beta_hill <- beta_hill[,-c(1)]
colnames(beta_hill)[9] <- "diff_depth"
save(beta_hill,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/beta_hill.RData")


#alpha_hill_all$classDepth <- as.factor(str_split_fixed(rownames(alpha_hill_all), "_", 2)[,2])













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








