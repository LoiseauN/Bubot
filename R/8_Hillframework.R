

library(mFD)
`%notin%` <- Negate(`%in%`)

abumat <-  dat_complet[,c("variable","value","Sample.code")]
abumat <-  as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= dat_complet))
abumat <-  species_site_scale_biomass
traits=dat_complet[,c("variable","Mobility","Activity","Schooling","Position",'Size',
                   "Diet")] #maxLength ,"clean_diet"
colnames(traits)[1]<- "species"
#Dominant Traits for species at the scale of the genus or family
for (i in 1:nrow(dat_complet)){
  if(is.na(dat_complet[i,2])){ 
        
    if(dat_complet[i,1] %in% )
    
    
    }
  
}


cov= unique(cov)
rownames(cov) <- cov[,1]
cov <- cov[,-1]
cov.pcoa <- na.omit(cov)

trait.dist <- cluster::daisy(cov.pcoa,metric ="gower")

abumat <- abumat[,apply(abumat,2,sum)>0]
abumat <- abumat[apply(abumat,1,sum)>0,]
trait.dist_mat <- as.matrix(trait.dist)
abumat <- as.matrix(abumat[,colnames(abumat) %in% rownames(trait.dist_mat)])
trait.dist_mat <- trait.dist_mat[,colnames(trait.dist_mat) %in% colnames(abumat)]
trait.dist_mat <- trait.dist_mat[rownames(trait.dist_mat) %in% colnames(abumat),]


abumat0_1 <- abumat
abumat0_1[abumat0_1>0] <- 1

abumat_relatif <- abumat
for (i in 1:nrow(abumat_relatif)){
  abumat_relatif[i,] <- abumat_relatif[i,]/sum(abumat_relatif[i,])
}


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


###############################################################################
###############################################################################
#taxo richness : data= 0/1, q=0, tau=min (c'est la richesse spé)
#taxo entropy: data= relative biomass, q=1, tau=min (c'est exp(Shannon) )
#fonctio richness: data= 0/1, q=0, tau=mean
#fonctio entropy: data= 0/1, q=1, tau=mean



#alpha
alpha_hill_taxo_richess  <- alpha.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 0,
                                          tau      = "min")$asb_FD_Hill

alpha_hill_taxo_entropy  <- alpha.fd.hill (asb_sp_w = abumat_relatif,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "min")$asb_FD_Hill

alpha_hill_fonct_richess <- alpha.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 0,
                                          tau      = "mean")$asb_FD_Hill

alpha_hill_fonct_entropy <- alpha.fd.hill (asb_sp_w = abumat_relatif,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "mean")$asb_FD_Hill

alpha_hill_all <- data.frame(hill_taxo_richess  = alpha_hill_taxo_richess,
                             hill_taxo_entropy  = alpha_hill_taxo_entropy,
                             hill_fonct_richess = alpha_hill_fonct_richess,
                             hill_fonct_entropy = alpha_hill_fonct_entropy)

#alpha_hill_all$classDepth <- as.factor(str_split_fixed(rownames(alpha_hill_all), "_", 2)[,2])

alpha_hill_all <- merge(alpha_div,alpha_hill_all,by="row.names",all.x=T)


for (i in 1:4){
  print(i)
  if (i == 1) data_plot <- alpha_hill_all
  if (i == 2) data_plot <- subset(alpha_hill_all,alpha_hill_all$island=="Mayotte")
  if (i == 3) data_plot <- subset(alpha_hill_all,alpha_hill_all$island=="Juan_de_nova")
  if (i == 4) data_plot <- subset(alpha_hill_all,alpha_hill_all$island=="Europa")
  
  
  FD_q0 <- ggplot(data_plot, aes(x=depth, y=FD_q0)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
                #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill taxo richness")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  
  FD_q1<- ggplot(data_plot, aes(x=depth, y=FD_q1)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill taxo entropy")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  FD_q0.1<- ggplot(data_plot, aes(x=depth, y=FD_q0.1)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill funct richness")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  FD_q1.1<- ggplot(data_plot, aes(x=depth, y=FD_q1.1)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill funct entropy")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  
  if (i == 1) title <- textGrob("All Islands",
                                gp=gpar(fontsize=20,fontface=2))
  if (i == 2) title <- textGrob("Mayotte",
                                gp=gpar(fontsize=20,fontface=2))
  if (i == 3) title <- textGrob("Juan de Nova",
                                gp=gpar(fontsize=20,fontface=2))
  if (i == 4) title <- textGrob("Europa",
                                gp=gpar(fontsize=20,fontface=2))
                                
  grid.arrange(FD_q0,FD_q0.1,FD_q1,FD_q1.1,ncol=2,top = title)
  
}



#beta
beta_hill_taxo_richess  <- beta.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 0,
                                          tau      = "min",
                                          beta_type = "Jaccard")

beta_hill_taxo_entropy  <- beta.fd.hill (asb_sp_w = abumat_relatif,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "min",
                                          beta_type = "Jaccard")

beta_hill_fonct_richess <- beta.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 0,
                                          tau      = "mean",
                                          beta_type = "Jaccard")

beta_hill_fonct_entropy <- beta.fd.hill (asb_sp_w = abumat_relatif,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "mean",
                                          beta_type = "Jaccard")


beta_hill_taxo_richess_t <- reshape::melt(as.matrix(beta_hill_taxo_richess$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_taxo_richess$beta_fd_q$q0)))$value,]
beta_hill_taxo_entropy_t <- reshape::melt(as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1)))$value,]
beta_hill_fonct_richess_t <- reshape::melt(as.matrix(beta_hill_fonct_richess$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_fonct_richess$beta_fd_q$q0)))$value,]
beta_hill_fonct_entropy_t <- reshape::melt(as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1)))$value,]


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







####################HILL SUGGESTION SEB
#Pour chaque video "profonde de D mètres" (D>10m), tu calcules ses beta avec toutes les vidéos "surfaces" (D<10m).
#Puis tu fais leur moyenne  (et sd) et tu représentes le graph Depth vs beta.
#tu as donc autant de points que de vidéos profondes (et donc aucun point entre 0 et 10 sur l'axe des X)


coord_depth_mayot <- coord_depth[rownames(coord_depth) %in% rownames(subset(alpha_div,alpha_div$island=="Mayotte")),]
#coord_depth_mayot <- coord_depth
From1to10depth <- subset(coord_depth_mayot, coord_depth_mayot$depth<=16)
From10toInfdepth <- subset(coord_depth_mayot, coord_depth_mayot$depth>16)

ResHill <- matrix(NA,nrow(From10toInfdepth),8)
rownames(ResHill) <- rownames(From10toInfdepth) 
colnames(ResHill) <- c("taxo_rich_m","taxo_rich_sd",
                       "taxo_entro_m","taxo_entro_sd",
                       "fct_rich_m","fct_rich_sd",
                       "fct_entro_m","fct_entro_sd")

#TOTO RAJOUTER ICI UN NIVEAU ILE



for(i in 1:nrow(From10toInfdepth)){
  print(i)
  
       abucompa <- abumat[rownames(abumat) %in% c(rownames(From10toInfdepth[i,]) , rownames(From1to10depth)),]
       abucompa <- abucompa[,apply(abucompa,2,sum) > 0]
       abucompa <- as.matrix(abucompa[,colnames(abucompa) %in% rownames(trait.dist_mat)])
       trait.dist_matcompa <- trait.dist_mat[,colnames(trait.dist_mat) %in% colnames(abucompa)]
       trait.dist_matcompa <- trait.dist_matcompa[rownames(trait.dist_matcompa) %in% colnames(abucompa),]
       
       abucompa0_1 <- abucompa
       abucompa0_1[abucompa0_1>0] <- 1
           
           abucompa_relatif <- abucompa
           for (k in 1:nrow(abucompa_relatif)){
             abucompa_relatif[k,] <- abucompa_relatif[k,]/sum(abucompa_relatif[k,])
           }
           
           #Compute HILL
           beta_hill_taxo_richess  <- as.matrix(beta.fd.hill (asb_sp_w = abucompa0_1,
                                                    sp_dist  = trait.dist_matcompa,
                                                    q        = 0,
                                                    tau      = "min",
                                                    beta_type = "Jaccard")$beta_fd_q$q0)
           
           beta_hill_taxo_entropy  <- as.matrix(beta.fd.hill (asb_sp_w = abucompa_relatif,
                                                    sp_dist  = trait.dist_matcompa,
                                                    q        = 1,
                                                    tau      = "min",
                                                    beta_type = "Jaccard")$beta_fd_q$q1)
           
           beta_hill_fonct_richess <- as.matrix(beta.fd.hill (asb_sp_w = abucompa0_1,
                                                    sp_dist  = trait.dist_matcompa,
                                                    q        = 0,
                                                    tau      = "mean",
                                                    beta_type = "Jaccard")$beta_fd_q$q0)
           
           beta_hill_fonct_entropy <- as.matrix(beta.fd.hill (asb_sp_w = abucompa_relatif,
                                                    sp_dist  = trait.dist_matcompa,
                                                    q        = 1,
                                                    tau      = "mean",
                                                    beta_type = "Jaccard")$beta_fd_q$q1)
           
 
           ResHill[i,1] <- mean(beta_hill_taxo_richess[rownames(beta_hill_taxo_richess) %in% rownames(From10toInfdepth[i,]),
                                                  colnames(beta_hill_taxo_richess) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,2] <- sd(beta_hill_taxo_richess[rownames(beta_hill_taxo_richess) %in% rownames(From10toInfdepth[i,]),
                                                       colnames(beta_hill_taxo_richess) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,3] <- mean(beta_hill_taxo_entropy[rownames(beta_hill_taxo_entropy) %in% rownames(From10toInfdepth[i,]),
                                                       colnames(beta_hill_taxo_entropy) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,4] <- sd(beta_hill_taxo_entropy[rownames(beta_hill_taxo_entropy) %in% rownames(From10toInfdepth[i,]),
                                                     colnames(beta_hill_taxo_entropy) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,5] <- mean(beta_hill_fonct_richess[rownames(beta_hill_fonct_richess) %in% rownames(From10toInfdepth[i,]),
                                                       colnames(beta_hill_fonct_richess) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,6] <- sd(beta_hill_fonct_richess[rownames(beta_hill_fonct_richess) %in% rownames(From10toInfdepth[i,]),
                                                     colnames(beta_hill_fonct_richess) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,7] <- mean(beta_hill_fonct_entropy[rownames(beta_hill_fonct_entropy) %in% rownames(From10toInfdepth[i,]),
                                                       colnames(beta_hill_fonct_entropy) %notin% rownames(From10toInfdepth[i,])])
           
           ResHill[i,8] <- sd(beta_hill_fonct_entropy[rownames(beta_hill_fonct_entropy) %in% rownames(From10toInfdepth[i,]),
                                                     colnames(beta_hill_fonct_entropy) %notin% rownames(From10toInfdepth[i,])])
}

ResHill <- as.data.frame(ResHill)
ResHill$depth <- From10toInfdepth$depth

a <- ggplot(ResHill, aes(x=depth, y=taxo_rich_m)) + 
             geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
             geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
             theme_bw()+ylab("Beta Hill taxo richness")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(ResHill, aes(x=depth, y=taxo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=taxo_entro_m-taxo_entro_sd, ymax=taxo_entro_m+taxo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta Hill taxo entropy")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(ResHill, aes(x=depth, y=fct_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_rich_m-fct_rich_sd, ymax=fct_rich_m+fct_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta Hill fonctio richness")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(ResHill, aes(x=depth, y=fct_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_entro_m-fct_entro_sd, ymax=fct_entro_m+fct_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta Hill fonctio entropy")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

title <- textGrob("Depth Decay Mayotte",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,c,b,d,ncol=2,top = title)


