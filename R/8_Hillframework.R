dat_complet <- merge(dat_complet,  species.site.matrix$site.data[,c("Sample.name","Sample.code")],by.x="VideoID",by.y="Sample.name",all.x=T)

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

alpha_hill_fonct_entropy <- alpha.fd.hill (asb_sp_w = abumat0_1,
                                          sp_dist  = trait.dist_mat,
                                          q        = 1,
                                          tau      = "mean")$asb_FD_Hill

alpha_hill_all <- data.frame(hill_taxo_richess  = alpha_hill_taxo_richess,
                             hill_taxo_entropy  = alpha_hill_taxo_entropy,
                             hill_fonct_richess = alpha_hill_fonct_richess,
                             hill_fonct_entropy = alpha_hill_fonct_entropy)



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

beta_hill_all <- data.frame(hill_taxo_richess  = apha_hill_taxo_richess,
                             hill_taxo_entropy  = apha_hill_taxo_entropy,
                             hill_fonct_richess = apha_hill_fonct_richess,
                             hill_fonct_entropy = apha_hill_fonct_entropy)


test <- merge(alpha_div,alpha_hill_all,by="row.names",all.x=T)


ggplot(test,aes(x=log10(depth),y=FD_q0))+geom_point()+theme_bw()
ggplot(test,aes(x=log10(depth),y=FD_q1))+geom_point()+theme_bw()
ggplot(test,aes(x=log10(depth),y=FD_q0.1))+geom_point()+theme_bw()
ggplot(test,aes(x=log10(depth),y=FD_q1.1))+geom_point()+theme_bw()

ggplot(test,aes(x=classDepth,y=FD_q0))+geom_boxplot()+theme_bw()
ggplot(test,aes(x=classDepth,y=FD_q1))+geom_boxplot()+theme_bw()
ggplot(test,aes(x=classDepth,y=FD_q0.1))+geom_boxplot()+theme_bw()
ggplot(test,aes(x=classDepth,y=FD_q1.1))+geom_boxplot()+theme_bw()

