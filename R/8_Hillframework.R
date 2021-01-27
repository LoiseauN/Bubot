#dat_complet <- merge(dat_complet,  species.site.matrix$site.data[,c("Sample.name","Sample.code")],by.x="VideoID",by.y="Sample.name",all.x=T)

abumat <-  dat_complet[,c("variable","value","Sample.code")]
abumat <-   as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= dat_complet))


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
###############################################################################
###############################################################################
#taxo richness : data= 0/1, q=0, tau=min (c'est la richesse spé)
#taxo entropy: data= relative biomass, q=1, tau=min (c'est exp(Shannon) )
#fonctio richness: data= 0/1, q=0, tau=mean
#fonctio entropy: data= 0/1, q=1, tau=mean

abumat    <-species_station_scale

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

alpha_hill_fonct_entropy <- alpha.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "mean")$asb_FD_Hill

alpha_hill_all <- data.frame(hill_taxo_richess  = alpha_hill_taxo_richess,
                             hill_taxo_entropy  = alpha_hill_taxo_entropy,
                             hill_fonct_richess = alpha_hill_fonct_richess,
                             hill_fonct_entropy = alpha_hill_fonct_entropy)

alpha_hill_all$classDepth <- as.factor(str_split_fixed(rownames(alpha_hill_all), "_", 2)[,2])

FD_q0 <- ggplot(alpha_hill_all,aes(x=classDepth,y=FD_q0,colour=classDepth ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill taxo richness")

FD_q1<- ggplot(alpha_hill_all,aes(x=classDepth,y=FD_q1,colour=classDepth ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill taxo entropy")

FD_q0.1<- ggplot(alpha_hill_all,aes(x=classDepth,y=FD_q0.1,colour=classDepth ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill funct richness")

FD_q1.1<- ggplot(alpha_hill_all,aes(x=classDepth,y=FD_q1.1,colour=classDepth ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill funct entropy")

grid.arrange(FD_q0,FD_q0.1,FD_q1,FD_q1.1)



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

beta_hill_fonct_entropy <- beta.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "mean",
                                          beta_type = "Jaccard")



test <- merge(alpha_div,alpha_hill_all,by="row.names",all.x=T)

FD_q0<- ggplot(test,aes(x=classDepth.x,y=FD_q0,colour=classDepth.x ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill taxo richness")

FD_q1<- ggplot(test,aes(x=classDepth.x,y=FD_q1,colour=classDepth.x ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill taxo entropy")

FD_q0.1<- ggplot(test,aes(x=classDepth.x,y=FD_q0.1,colour=classDepth.x ))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill funct richness")

FD_q1.1<- ggplot(test,aes(x=classDepth.x,y=FD_q1.1,colour=classDepth.x))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter()+
  ggtitle("Hill funct entropy")

grid.arrange(FD_q0,FD_q0.1,FD_q1,FD_q1.1)
