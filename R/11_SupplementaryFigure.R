
##Supplementary


a <- ggplot(alpha_div_sensibility_all, aes(x=classDepth, y=sp_richn)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-richness taxo")+ geom_jitter(width = 0.1, size = 0.5)

b <- ggplot(alpha_div_sensibility_all, aes(x=classDepth, y=alpha_hill_taxo_entropy)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-entropy taxo")+ geom_jitter(width = 0.1, size = 0.5)

c <- ggplot(alpha_div_sensibility_all, aes(x=classDepth, y=alpha_hill_fonct_richess)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-richness fonct")+ geom_jitter(width = 0.1, size = 0.5)

d <- ggplot(alpha_div_sensibility_all, aes(x=classDepth, y=alpha_hill_fonct_entropy)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-entropy fonct")+ geom_jitter(width = 0.1, size = 0.5)

title <- textGrob("Alpha Hill boxplot",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2,top = title)



###PLot Beta

a <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_taxo_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)


b <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)


c <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)


d <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)


title <- textGrob("Beta Hill",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2,top = title)



#SENSIBILITY

#At the site scale
dat_complet_sensibility <- dat_complet

for (i in 1: nrow(dat_complet_sensibility)){
  if(is.na(dat_complet_sensibility$genus[i])) {
    dat_complet_sensibility$genus[i] <- dat_complet_sensibility$family[i]}
}

#Biomass matrix
biomass_mat_sensibility = reshape2::melt(dat_complet_sensibility , id.vars = c( "site" , "genus" ) , measure.vars = "Groupweigth")
biomass_mat_sensibility = reshape2::dcast( biomass_mat_sensibility , site~genus,sum,na.rm=T)
rownames(biomass_mat_sensibility) <- biomass_mat_sensibility[,1]
biomass_mat_sensibility <- biomass_mat_sensibility[,-1]

#Remove site with not enough genus 
preabs_mat_sensibility <- biomass_mat_sensibility
preabs_mat_sensibility[preabs_mat_sensibility>0] <- 1
preabs_mat_sensibility <- preabs_mat_sensibility[apply(preabs_mat_sensibility,1,sum)>4,]
preabs_mat_sensibility <- preabs_mat_sensibility[,apply(preabs_mat_sensibility,2,sum)>0]
biomass_mat_sensibility <- biomass_mat_sensibility[rownames(biomass_mat_sensibility) %in% rownames(preabs_mat_sensibility),]
biomass_mat_sensibility <- biomass_mat_sensibility[,colnames(biomass_mat_sensibility) %in% colnames(preabs_mat_sensibility)]


#Number at the genus and genus and family lever
abundance_mat_sensibility = reshape2::melt(dat_complet_sensibility , id.vars = c( "site" , "genus" ) , measure.vars = "abundance")
abundance_mat_sensibility = reshape2::dcast( abundance_mat_sensibility , site~genus,sum,na.rm=T)
rownames(abundance_mat_sensibility) <- abundance_mat_sensibility[,1]
abundance_mat_sensibility <- abundance_mat_sensibility[,-1]
abundance_mat_sensibility <- abundance_mat_sensibility[rownames(abundance_mat_sensibility) %in% rownames(biomass_mat_sensibility),]
abundance_mat_sensibility <- abundance_mat_sensibility[,colnames(abundance_mat_sensibility) %in% colnames(biomass_mat_sensibility)]

#Traits matrix
trait_mat_sensibility <- dat_complet_sensibility[,c("genus","mobility","activity","schooling","position","clean_diet",
                                                    "maxLengthTL_Fishbase")]
trait_mat_sensibility <-unique(trait_mat_sensibility)


trait_mat_sensibility_2 <- data.frame(matrix(NA,ncol=ncol(trait_mat_sensibility), nrow=length(unique(trait_mat_sensibility$genus))))
rownames(trait_mat_sensibility_2) <- unique(trait_mat_sensibility$genus)
colnames(trait_mat_sensibility_2) <- c("genus","mobility","activity","schooling","position","clean_diet",
                                       "maxLengthTL_Fishbase")
for (i in 1:nrow(trait_mat_sensibility_2)){
  print(i)
  genus <- rownames(trait_mat_sensibility_2)[i]
  
  trait_mat_sensibility_2[i,"mobility"] <-  names(sort(table(trait_mat_sensibility[which(trait_mat_sensibility$genus==genus) ,"mobility"]),decreasing = T)[1])
  trait_mat_sensibility_2[i,"activity"] <-  names(sort(table(trait_mat_sensibility[which(trait_mat_sensibility$genus==genus) ,"activity"]),decreasing = T)[1])
  trait_mat_sensibility_2[i,"schooling"] <-  names(sort(table(trait_mat_sensibility[which(trait_mat_sensibility$genus==genus) ,"schooling"]),decreasing = T)[1])
  trait_mat_sensibility_2[i,"position"] <-  names(sort(table(trait_mat_sensibility[which(trait_mat_sensibility$genus==genus) ,"position"]),decreasing = T)[1])
  trait_mat_sensibility_2[i,"clean_diet"] <-  names(sort(table(trait_mat_sensibility[which(trait_mat_sensibility$genus==genus) ,"clean_diet"]),decreasing = T)[1])
  trait_mat_sensibility_2[i,"maxLengthTL_Fishbase"] <-  mean(trait_mat_sensibility[which(trait_mat_sensibility$genus==genus) ,"maxLengthTL_Fishbase"])
  
}
#Check nature of traits
trait_mat_sensibility_2$position <- factor(as.character(trait_mat_sensibility_2$position), 
                                           order=T)
trait_mat_sensibility_2$schooling <- forcats::fct_rev(factor(as.character(trait_mat_sensibility_2$schooling), 
                                                             order=T))
trait_mat_sensibility_2$clean_diet <- factor(as.character(trait_mat_sensibility_2$clean_diet), 
                                             levels=c("HM","HD","OM","PK","IS","IM","PI"),order=T)


trait_mat_sensibility_2$mobility<- forcats::fct_rev(factor(as.character(trait_mat_sensibility_2$mobility), 
                                                           order=T))
trait_mat_sensibility_2$activity <- factor(trait_mat_sensibility_2$activity)  

trait_mat_sensibility <- trait_mat_sensibility_2



trait_mat_sensibility  <- trait_mat_sensibility[,-1]

trait_mat_sensibility <- trait_mat_sensibility[rownames(trait_mat_sensibility) %in% colnames(biomass_mat_sensibility),]

trait_cat <- data.frame(trait_name = colnames(trait_mat_sensibility),
                        trait_type = c("O","N","O","O","O","Q")
)

traits_summary <- mFD::sp.tr.summary(tr_cat = trait_cat,    # Traits informations
                                     sp_tr  = trait_mat_sensibility, stop_if_NA = TRUE)
#Traits distance     
sp_dist_traits <-  mFD::funct.dist(sp_tr         = trait_mat_sensibility,
                                   tr_cat        = trait_cat,
                                   metric        = "gower",
                                   scale_euclid  = "scale_center",
                                   ordinal_var   = "classic",
                                   weight_type   = "equal",
                                   stop_if_NA    = TRUE)

#Traits space quality    #4 dimension
fspaces_quality <- mFD::quality.fspaces(sp_dist             = sp_dist_traits,
                                        maxdim_pcoa         = 10,
                                        deviation_weighting = "absolute",
                                        fdist_scaling       = FALSE,
                                        fdendro             = "average")

mFD::quality.fspaces.plot(fspaces_quality            = fspaces_quality,
                          quality_metric             = "mad",
                          fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", "pcoa_4d", "pcoa_5d", "pcoa_6d"),
                          name_file                  = NULL,
                          range_dist                 = NULL,
                          range_dev                  = NULL,
                          range_qdev                 = NULL,
                          gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
                          gradient_deviation_quality = c(low = "yellow", high = "red"),
                          x_lab                      = "Trait-based distance")

#Correlation Traits PCOA
sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"
tr_faxes <- mFD::traits.faxes.cor(sp_tr = trait_mat_sensibility, 
                                  sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")], plot = TRUE)

tr_faxes$tr_faxes_plot

#Plot functional space
mFD::funct.space.plot(
  sp_faxes_coord    = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
  faxes             = NULL,
  name_file         = NULL,
  faxes_nm          = NULL,
  range_faxes       = c(NA, NA),
  color_bg          = "grey95",
  color_pool          = "darkturquoise",
  fill_pool           = "white",
  shape_pool          = 21,
  size_pool           = 1,
  plot_ch           = TRUE,
  color_ch          = "darkblue",
  fill_ch           = "white",
  alpha_ch          = 1,
  plot_vertices     = TRUE,
  color_vert        = "darkturquoise",
  fill_vert         = "darkturquoise",
  shape_vert        = 22,
  size_vert         = 1,
  plot_sp_nm         = NULL,
  nm_size            = 3,
  nm_color           = "black",
  nm_fontface        = "plain",
  check_input        = TRUE)


#Compute Functional diversity
alpha_fd_indices <- mFD::alpha.fd.multidim(sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
                                           asb_sp_w      = as.matrix(biomass_mat_sensibility),
                                           ind_vect      = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", "fspe"),
                                           scaling       = TRUE,
                                           check_input   = TRUE,
                                           details_returned = TRUE)


fd_ind_values <- alpha_fd_indices$"functional_diversity_indices"
fd_ind_values


tax_ind_values <- data.frame(biomass = apply(biomass_mat_sensibility,1,sum))

alpha_div_sensibility <- merge(fd_ind_values,tax_ind_values,by = "row.names",
                   all= T )

rownames(alpha_div_sensibility) <- alpha_div_sensibility[,1]
alpha_div_sensibility <- alpha_div_sensibility[,-1]


alpha_div_sensibility <- merge(alpha_div_sensibility,hab_pc_site_scale,by = "row.names",
                   all.x= T)

rownames(alpha_div_sensibility) <- alpha_div_sensibility[,1]
alpha_div_sensibility <- alpha_div_sensibility[,-1]



#######################
biomass_mat_sensibility0_1<- biomass_mat_sensibility
biomass_mat_sensibility0_1[biomass_mat_sensibility0_1>0] <-1

alpha_hill_taxo_entropy  <- mFD::alpha.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility),
                                                sp_dist  = sp_dist_traits,
                                                q        = 1,
                                                tau      = "min")$asb_FD_Hill

alpha_hill_fonct_richess <- mFD::alpha.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility0_1),
                                                sp_dist  = sp_dist_traits,
                                                q        = 0,
                                                tau      = "mean")$asb_FD_Hill

alpha_hill_fonct_entropy <- mFD::alpha.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility),
                                                sp_dist  = sp_dist_traits,
                                                q        = 1,
                                                tau      = "mean")$asb_FD_Hill

alpha_hill_all <- data.frame(hill_taxo_entropy  = alpha_hill_taxo_entropy[,1],
                             hill_fonct_richess = alpha_hill_fonct_richess[,1],
                             hill_fonct_entropy = alpha_hill_fonct_entropy[,1])


colnames(alpha_hill_all) <- c("alpha_hill_taxo_entropy",
                              "alpha_hill_fonct_richess","alpha_hill_fonct_entropy")


alpha_div_sensibility <- merge(alpha_div_sensibility,alpha_hill_all,by="row.names")
rownames(alpha_div_sensibility) <- alpha_div_sensibility[,1]
alpha_div_sensibility <- alpha_div_sensibility[,-c(1)]

colnames(alpha_div_all)[c(15:19)] <- c("PC1_hab","PC2_hab","PC3_hab","PC4_hab","PC5_hab")





ggsave(filename=here::here("fig/Figure2.png"), 
       plot = biom_plot, 
       width = 4, 
       height = 4, 
       units = "in",
       dpi=300)



#######################
a <- ggplot(alpha_div_sensibility, aes(x=depth, y=sp_richn)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab("Taxonomic")+xlab("")+ggtitle("Richness")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)


b <- ggplot(alpha_div_sensibility, aes(x=depth, y=alpha_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab(" ")+xlab(" ")+ggtitle("Entropy")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)


c <- ggplot(alpha_div_sensibility, aes(x=depth, y=alpha_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab("Functional")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)


d <- ggplot(alpha_div_sensibility, aes(x=depth, y=alpha_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab("")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)

alpha_plot_sensi <- grid.arrange(a,b,c,d,ncol=2)#,top = title)
ggsave(filename=here::here("fig/figureS3.png"), 
       plot = alpha_plot_sensi, 
       width = 8, 
       height = 8, 
       units = "in",
       dpi=300)





###SUPPLEMENTARY DECAY
coord_depth <- species.site.matrix$site.data[,c(2,5,7,8)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]

coord_depth <- coord_depth[rownames(coord_depth) %in% rownames(alpha_div_all),]
#coord_depth <- coord_depth
From1to20depth <- subset(coord_depth, coord_depth$depth<=20)
From20toInfdepth <- subset(coord_depth, coord_depth$depth>20)

ResHill_sensibility <- matrix(NA,nrow(From20toInfdepth),12)
rownames(ResHill_sensibility) <- rownames(From20toInfdepth) 
colnames(ResHill_sensibility) <- c("taxo_rich_m","taxo_rich_sd",
                       "taxo_entro_m","taxo_entro_sd",
                       "fct_rich_m","fct_rich_sd",
                       "fct_entro_m","fct_entro_sd",
                       "phylo_rich_m","phylo_rich_sd",
                       "phylo_entro_m","phylo_entro_sd")

biomass_mat_sensibility <- biomass_mat_sensibility[apply(biomass_mat_sensibility,1,sum)>4,]
biomass_mat_sensibility <- biomass_mat_sensibility[,apply(biomass_mat_sensibility,1,sum)>0]
trait.dist_mat <-as.matrix(sp_dist_traits)

for(j in 1:nrow(From20toInfdepth)){
  print(j)
  
  biomasscompa <- biomass_mat_sensibility[rownames(biomass_mat_sensibility) %in% c(rownames(From20toInfdepth[j,]) , rownames(From1to20depth)),]
  biomasscompa <- biomasscompa[,apply(biomasscompa,2,sum) > 0]
  biomasscompa <- as.matrix(biomasscompa[,colnames(biomasscompa) %in% rownames(trait.dist_mat)])
  trait.dist_matcompa <- trait.dist_mat[,colnames(trait.dist_mat) %in% colnames(biomasscompa)]
  trait.dist_matcompa <- trait.dist_matcompa[rownames(trait.dist_matcompa) %in% colnames(biomasscompa),]
  
  biomasscompa0_1 <- biomasscompa
  biomasscompa0_1[biomasscompa0_1>0] <- 1
  
  #biomass_compa_phylo <- biomasscompa[,colnames(biomasscompa) %in% names(tree_phylog$leaves)]
  #biomass_compa_phylo <- biomass_compa_phylo/apply(biomass_compa_phylo,1,sum)
  
  #if(sum(is.na(biomass_compa_phylo))>0) {next}
  
  
  #Compute HILL
  #beta_hill_phylo <- chao_alpha_beta(matrix = biomass_compa_phylo,q=c(0,1,2), tree_phylog = tree_phylog)
  
  #beta_hill_phylo_richess <- as.matrix(beta_hill_phylo$beta_phylo$q0)
  
  #beta_hill_phylo_entropy <- as.matrix(beta_hill_phylo$beta_phylo$q1)
  
  beta_hill_taxo_richess  <- as.matrix(mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility0_1),
                                                          sp_dist  = sp_dist_traits,
                                                          q        = 0,
                                                          tau      = "min",
                                                          beta_type = "Jaccard")$beta_fd_q$q0)
  
  beta_hill_taxo_entropy  <- as.matrix(mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility),
                                                          sp_dist  = sp_dist_traits,
                                                          q        = 1,
                                                          tau      = "min",
                                                          beta_type = "Jaccard")$beta_fd_q$q1)
  
  beta_hill_fonct_richess <- as.matrix(mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility0_1),
                                                          sp_dist  = sp_dist_traits,
                                                          q        = 0,
                                                          tau      = "mean",
                                                          beta_type = "Jaccard")$beta_fd_q$q0)
  
  beta_hill_fonct_entropy <- as.matrix(mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility),
                                                          sp_dist  = sp_dist_traits,
                                                          q        = 1,
                                                          tau      = "mean",
                                                          beta_type = "Jaccard")$beta_fd_q$q1)
  
  
  ResHill_sensibility[j,1] <- mean(beta_hill_taxo_richess[rownames(beta_hill_taxo_richess) %in% rownames(From20toInfdepth[j,]),
                                              colnames(beta_hill_taxo_richess) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,2] <- sd(beta_hill_taxo_richess[rownames(beta_hill_taxo_richess) %in% rownames(From20toInfdepth[j,]),
                                            colnames(beta_hill_taxo_richess) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,3] <- mean(beta_hill_taxo_entropy[rownames(beta_hill_taxo_entropy) %in% rownames(From20toInfdepth[j,]),
                                              colnames(beta_hill_taxo_entropy) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,4] <- sd(beta_hill_taxo_entropy[rownames(beta_hill_taxo_entropy) %in% rownames(From20toInfdepth[j,]),
                                            colnames(beta_hill_taxo_entropy) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,5] <- mean(beta_hill_fonct_richess[rownames(beta_hill_fonct_richess) %in% rownames(From20toInfdepth[j,]),
                                               colnames(beta_hill_fonct_richess) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,6] <- sd(beta_hill_fonct_richess[rownames(beta_hill_fonct_richess) %in% rownames(From20toInfdepth[j,]),
                                             colnames(beta_hill_fonct_richess) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,7] <- mean(beta_hill_fonct_entropy[rownames(beta_hill_fonct_entropy) %in% rownames(From20toInfdepth[j,]),
                                               colnames(beta_hill_fonct_entropy) %notin% rownames(From20toInfdepth[j,])])
  
  ResHill_sensibility[j,8] <- sd(beta_hill_fonct_entropy[rownames(beta_hill_fonct_entropy) %in% rownames(From20toInfdepth[j,]),
                                             colnames(beta_hill_fonct_entropy) %notin% rownames(From20toInfdepth[j,])])
  
}
Decay_Hill_20toInfdepth_sensibility <- ResHill_sensibility
save(Decay_Hill_20toInfdepth_sensibility,file=here::here("results/Decay_Hill_20toInfdepth_sensibility.RData"))

###PLot Distance decay
ResHill<- Decay_Hill_20toInfdepth_sensibility
ResHill <- as.data.frame(ResHill)
ResHill <- merge(ResHill,coord_depth, by="row.names",all.x=T)

a <- ggplot(ResHill, aes(x=depth, y=taxo_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(ResHill$depth))+
  geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Taxonomic")+xlab("")+ggtitle("Dissimiliarity composition")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)

b <- ggplot(ResHill, aes(x=depth, y=taxo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(ResHill$depth))+
  geom_errorbar(aes(ymin=taxo_entro_m-taxo_entro_sd, ymax=taxo_entro_m+taxo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab(" ")+xlab(" ")+ggtitle("Dissimiliarity structure")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)

c <- ggplot(ResHill, aes(x=depth, y=fct_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_rich_m-fct_rich_sd, ymax=fct_rich_m+fct_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Functional")+xlab("Difference in Depth (m)")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)

d <- ggplot(ResHill, aes(x=depth, y=fct_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_entro_m-fct_entro_sd, ymax=fct_entro_m+fct_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("")+xlab("Difference in Depth (m)")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)

#title <- textGrob("Depth Decay",
#                 gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2)#,top = title)


#                 gp=gpar(fontsize=20,fontface=2))
decayplot_sensi <- grid.arrange(a,b,c,d,ncol=2)#,top = title)

ggsave(filename=here::here("fig/figureS4.png"), 
       plot = decayplot_sensi, 
       width = 8, 
       height = 8, 
       units = "in",
       dpi=300)













beta_hill_taxo_richess_sensibility  <- mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility0_1),
                                              sp_dist  = sp_dist_traits,
                                              q        = 0,
                                              tau      = "min",
                                              beta_type = "Jaccard")

beta_hill_taxo_entropy_sensibility  <- mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility),
                                              sp_dist  = sp_dist_traits,
                                              q        = 1,
                                              tau      = "min",
                                              beta_type = "Jaccard")

beta_hill_fonct_richess_sensibility <- mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility0_1),
                                              sp_dist  = sp_dist_traits,
                                              q        = 0,
                                              tau      = "mean",
                                              beta_type = "Jaccard")

beta_hill_fonct_entropy_sensibility <- mFD::beta.fd.hill (asb_sp_w = as.matrix(biomass_mat_sensibility),
                                              sp_dist  = sp_dist_traits,
                                              q        = 1,
                                              tau      = "mean",
                                              beta_type = "Jaccard")


beta_hill_taxo_richess_t_sensibility <- reshape::melt(as.matrix(beta_hill_taxo_richess_sensibility$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_taxo_richess_sensibility$beta_fd_q$q0)))$value,]
beta_hill_taxo_entropy_t_sensibility <- reshape::melt(as.matrix(beta_hill_taxo_entropy_sensibility$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_taxo_entropy_sensibility$beta_fd_q$q1)))$value,]
beta_hill_fonct_richess_t_sensibility <- reshape::melt(as.matrix(beta_hill_fonct_richess_sensibility$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_fonct_richess_sensibility$beta_fd_q$q0)))$value,]
beta_hill_fonct_entropy_t_sensibility <- reshape::melt(as.matrix(beta_hill_fonct_entropy_sensibility$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_fonct_entropy_sensibility$beta_fd_q$q1)))$value,]

beta_hill_sensibility <- data.frame(site1 = beta_hill_taxo_richess_t_sensibility[,1],
                        site2 = beta_hill_taxo_richess_t_sensibility[,2],
                        beta_hill_taxo_richess = beta_hill_taxo_richess_t_sensibility[,3],
                        beta_hill_taxo_entropy = beta_hill_taxo_entropy_t_sensibility[,3],
                        beta_hill_fonct_richess = beta_hill_fonct_richess_t_sensibility[,3],
                        beta_hill_fonct_entropy = beta_hill_fonct_entropy_t_sensibility[,3],
                        pairsID = paste0(beta_hill_taxo_richess_t_sensibility[,1], "__",
                                         beta_hill_taxo_richess_t_sensibility[,2]))


diff_depth <- data.frame(row.names = rownames(alpha_div_all),
                         depth=alpha_div_all$depth)


diff_depth  <- dist(diff_depth, method = "euclidean")                        
diff_depth <- reshape2::melt(as.matrix(diff_depth))[melt(upper.tri(as.matrix(diff_depth)))$value,]

diff_depth$pairsID <- paste0(diff_depth[,1], "__",
                             diff_depth[,2])          
diff_depth <- diff_depth[,-c(1,2)]     
beta_hill_sensibility <- merge(beta_hill_sensibility,diff_depth,by="pairsID",all.x = T)
rownames(beta_hill_sensibility) <- beta_hill_sensibility[,1]
beta_hill_sensibility <- beta_hill_sensibility[,-c(1)]
colnames(beta_hill_sensibility)[7] <- "diff_depth"
save(beta_hill_sensibility,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/beta_hill_sensibility.RData")

