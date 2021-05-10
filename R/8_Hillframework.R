library(mFD)
`%notin%` <- Negate(`%in%`)

###############################################################################
###############################################################################
#taxo richness : data= 0/1, q=0, tau=min (c'est la richesse spé)
#taxo entropy: data= relative biomass, q=1, tau=min (c'est exp(Shannon) )
#fonctio richness: data= 0/1, q=0, tau=mean
#fonctio entropy: data= 0/1, q=1, tau=mean


biomass_mat <- as.matrix(biomass_mat)
biomass_mat0_1 <- biomass_mat
biomass_mat0_1[biomass_mat0_1>0] <- 1

#Check NA
sum(is.na(biomass_mat0_1))
sum(is.na(sp_dist_traits))

#Check 1/0
min(biomass_mat0_1)
max(biomass_mat0_1)
range(biomass_mat0_1)

#Check same species
dim(biomass_mat0_1)
length(labels(sp_dist_traits))
sum(!labels(sp_dist_traits) %in% colnames(biomass_mat0_1))
sum(!colnames(biomass_mat0_1) %in% labels(sp_dist_traits))
any(is.na(sp_dist_traits))
#Check no species with 0 occurence and site with 0 species
min(apply(biomass_mat0_1,2,sum))
min(apply(biomass_mat0_1,1,sum))
      

# it is possible to compute taxonomic alpha and beta with beta.fd.hill but functional distance
# must be higher than 0 to avoid transformation of species in FE (reducing the real value of alpha)

sp_dist_traits_for_taxo <- sp_dist_traits
sp_dist_traits_for_taxo[sp_dist_traits_for_taxo==0]<- 0.0001

#alpha

alpha_hill_taxo_entropy  <- mFD::alpha.fd.hill (asb_sp_w = biomass_mat,
                                           sp_dist  = sp_dist_traits_for_taxo,
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

alpha_hill_all <- data.frame(hill_taxo_entropy  = alpha_hill_taxo_entropy[,1],
                             hill_fonct_richess = alpha_hill_fonct_richess[,1],
                             hill_fonct_entropy = alpha_hill_fonct_entropy[,1])


colnames(alpha_hill_all) <- c("alpha_hill_taxo_entropy",
                              "alpha_hill_fonct_richess","alpha_hill_fonct_entropy")


alpha_div_all <- merge(alpha_div,alpha_hill_all,by="row.names")
rownames(alpha_div_all) <- alpha_div_all[,1]
alpha_div_all <- alpha_div_all[,-c(1)]

colnames(alpha_div_all)[c(15:19)] <- c("PC1_hab","PC2_hab","PC3_hab","PC4_hab","PC5_hab")


save(alpha_div_all,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/alpha_div_all.RData")


#beta


beta_hill_taxo_richess  <- mFD::beta.fd.hill (asb_sp_w = biomass_mat0_1,
                                         sp_dist  = sp_dist_traits_for_taxo,
                                         q        = 0,
                                         tau      = "min",
                                         beta_type = "Jaccard")

beta_hill_taxo_entropy  <- mFD::beta.fd.hill (asb_sp_w = biomass_mat,
                                         sp_dist  = sp_dist_traits_for_taxo,
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
colnames(beta_hill)[7] <- "diff_depth"
save(beta_hill,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/beta_hill.RData")


#alpha_hill_all$classDepth <- as.factor(str_split_fixed(rownames(alpha_hill_all), "_", 2)[,2])










