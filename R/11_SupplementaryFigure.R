
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
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

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



#######################
a <- ggplot(alpha_div_sensibility, aes(x=depth, y=sp_richn)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab("Taxonomic")+xlab("")+ggtitle("Richness")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(alpha_div_sensibility, aes(x=depth, y=alpha_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab(" ")+xlab(" ")+ggtitle("Structure")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(alpha_div_sensibility, aes(x=depth, y=alpha_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab("Functional")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(alpha_div_sensibility, aes(x=depth, y=alpha_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_sensibility$depth))+
  theme_bw()+ylab("")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
grid.arrange(a,b,c,d,ncol=2)#,top = title)

