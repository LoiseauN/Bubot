
pkgs <- c('forestmodel','gdm')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))
#Model Alpha : 



mod_biomass <- glm(biomass ~  depth + PC1_hab + PC2_hab, data = alpha_div_all)
performance::check_normality(mod_biomass)
performance::check_heteroscedasticity(mod_biomass)
performance::check_model(mod_biomass)
performance::model_performance(mod_biomass)
Rsqr_mod_biomass <- 1 - (mod_biomass$deviance/mod_biomass$null.deviance )

mod_alphaS <- glm(sp_richn ~  depth + PC1_hab + PC2_hab, data = alpha_div_all)
mod_alphaentro <- glm(alpha_hill_taxo_entropy ~  depth + PC1_hab + PC2_hab, data = alpha_div_all)
performance::check_normality(mod_alphaS)
performance::check_heteroscedasticity(mod_alphaS)
performance::check_model(mod_alphaS)
performance::model_performance(mod_alphaS)
Rsqr_mod_alphaS <- 1 - (mod_alphaS$deviance/mod_alphaS$null.deviance )

performance::check_normality(mod_alphaentro)
performance::check_heteroscedasticity(mod_alphaentro)
performance::check_model(mod_alphaentro)
performance::model_performance(mod_alphaentro)
Rsqr_mod_alphaentro <- 1 - (mod_alphaentro$deviance/mod_alphaentro$null.deviance )

mod_alphaFct <- glm(alpha_hill_fonct_richess ~  depth + PC1_hab + PC2_hab, data = alpha_div_all)
mod_alphaFct_entro <- glm(alpha_hill_fonct_entropy ~  depth + PC1_hab + PC2_hab, data = alpha_div_all)
performance::check_normality(mod_alphaFct)
performance::check_heteroscedasticity(mod_alphaFct)
performance::check_model(mod_alphaFct)
performance::model_performance(mod_alphaFct)
Rsqr_mod_alphaFct <- 1 - (mod_alphaFct$deviance/mod_alphaFct$null.deviance )

performance::check_normality(mod_alphaFct_entro)
performance::check_heteroscedasticity(mod_alphaFct_entro)
performance::check_model(mod_alphaFct_entro)
performance::model_performance(mod_alphaFct_entro)
Rsqr_mod_alphaFct_entro <- 1 - (mod_alphaFct_entro$deviance/mod_alphaFct_entro$null.deviance )

##GDM

hab_pc_site_scale <- merge(hab_pc_site_scale,species.site.matrix$site.data[,c(2,6:7)],by.x="row.names",
                           by.y="Sample.code",all.x=T)

hab_pc_site_scale <- unique(hab_pc_site_scale)
rownames(hab_pc_site_scale)  <- hab_pc_site_scale[,1]
hab_pc_site_scale  <- hab_pc_site_scale[,-1]
hab_pc_site_scale <- hab_pc_site_scale[rownames(hab_pc_site_scale) %in% rownames(alpha_div_all),]


hab_selec<-cbind(rownames(hab_pc_site_scale),hab_pc_site_scale[,c(1,2,6,10,11)])

colnames(hab_selec)<-c("site","PC1","PC2","depth","Lat", "Long")

GDM_results<-matrix(NA,4,4)
rownames(GDM_results)<-c("beta_hill_taxo_richess","beta_hill_taxo_entropy",
                         "beta_hill_fonct_richess","beta_hill_fonct_entropy")
colnames(GDM_results)<-c("DevianceExplained","contrib_PC1","contrib_PC2","contrib_depth")


for(i in 1:4){ #nrow(GDM_results)
  print(i)
  if(i == 1) { mat = as.matrix(beta_hill_taxo_richess$beta_fd_q$q0)}
  if(i == 2) { mat = as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1)}
  if(i == 3) { mat = as.matrix(beta_hill_fonct_richess$beta_fd_q$q0)}
  if(i == 4) { mat = as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1)}

  dissim <-  as.data.frame(mat)
  site<- as.factor(rownames(dissim))
  dissim<- cbind(site, dissim)
  exFormat3 <- formatsitepair(dissim, 
                              bioFormat = 3, 
                              XColumn="Long", 
                              YColumn="Lat", 
                              predData=hab_selec, 
                              siteColumn="site")
  
  Mod <- gdm(exFormat3, geo=T)
  GDM_results[i,1]<-Mod$explained
  
  dissim <-  as.data.frame(mat)
  site<- as.factor(rownames(dissim))
  dissim<- cbind(site, dissim)
  exFormat3 <- formatsitepair(dissim, 
                              bioFormat = 3, 
                              XColumn="Long", 
                              YColumn="Lat", 
                              predData=hab_selec[,-2], 
                              siteColumn="site")
  
  Mod <- gdm(exFormat3, geo=T)
  GDM_results[i,2]<-((GDM_results[i,1]-Mod$explained)/GDM_results[i,1])*100

  
  
  dissim <-  as.data.frame(mat)
  site<- as.factor(rownames(dissim))
  dissim<- cbind(site, dissim)
  exFormat3 <- formatsitepair(dissim, 
                              bioFormat = 3, 
                              XColumn="Long", 
                              YColumn="Lat", 
                              predData=hab_selec[,-3], 
                              siteColumn="site")
  
  Mod <- gdm(exFormat3, geo=T)
  GDM_results[i,3]<-((GDM_results[i,1]-Mod$explained)/GDM_results[i,1])*100
  
  dissim <-  as.data.frame(mat)
  site<- as.factor(rownames(dissim))
  dissim<- cbind(site, dissim)
  exFormat3 <- formatsitepair(dissim, 
                              bioFormat = 3, 
                              XColumn="Long", 
                              YColumn="Lat", 
                              predData=hab_selec[,-4], 
                              siteColumn="site")
  
  Mod <- gdm(exFormat3, geo=T)
  GDM_results[i,4]<-((GDM_results[i,1]-Mod$explained)/GDM_results[i,1])*100
  
  
}
  
GDM_results2 <- data.frame(Component = c(rep("beta_hill_taxo_richess",3),
                 rep("beta_hill_taxo_entropy",3),
                 rep("beta_hill_fonct_richess",3),
                 rep("beta_hill_fonct_entropy",3)),
                 drivers = rep(c("PC1","PC2","depth"),4),
                 Contribution = c(GDM_results[1,2:4],
                             GDM_results[2,2:4],
                             GDM_results[3,2:4],
                             GDM_results[4,2:4]))
                 
p <- ggplot(GDM_results2, aes(x = Component, y = Contribution))+
  geom_col(aes(fill = drivers), width = 0.7)+
  coord_flip()+ theme_bw()

  
####################HILL SUGGESTION SEB
#Pour chaque video "profonde de D mètres" (D>10m), tu calcules ses beta avec toutes les vidéos "surfaces" (D<10m).
#Puis tu fais leur moyenne  (et sd) et tu représentes le graph Depth vs beta.
#tu as donc autant de points que de vidéos profondes (et donc aucun point entre 0 et 10 sur l'axe des X)

#Plot distance decay en fonction des profondeurs
coord_depth <- species.site.matrix$site.data[,c(2,5:8)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]


coord_depth <- coord_depth[rownames(coord_depth) %in% rownames(alpha_div_all),]
#coord_depth <- coord_depth
From1to10depth <- subset(coord_depth, coord_depth$depth<=10)
From10toInfdepth <- subset(coord_depth, coord_depth$depth>10)

ResHill <- matrix(NA,nrow(From10toInfdepth),12)
rownames(ResHill) <- rownames(From10toInfdepth) 
colnames(ResHill) <- c("taxo_rich_m","taxo_rich_sd",
                       "taxo_entro_m","taxo_entro_sd",
                       "fct_rich_m","fct_rich_sd",
                       "fct_entro_m","fct_entro_sd",
                       "phylo_rich_m","phylo_rich_sd",
                       "phylo_entro_m","phylo_entro_sd")

biomass_mat <- biomass_mat[apply(biomass_mat,1,sum)>4,]
biomass_mat <- biomass_mat[,apply(biomass_mat,1,sum)>0]


for(j in 1:nrow(From10toInfdepth)){
  print(j)
  
  biomasscompa <- biomass_mat[rownames(biomass_mat) %in% c(rownames(From10toInfdepth[j,]) , rownames(From1to10depth)),]
  biomasscompa <- biomasscompa[,apply(biomasscompa,2,sum) > 0]
  biomasscompa <- as.matrix(biomasscompa[,colnames(biomasscompa) %in% rownames(trait.dist_mat)])
  trait.dist_matcompa <- trait.dist_mat[,colnames(trait.dist_mat) %in% colnames(biomasscompa)]
  trait.dist_matcompa <- trait.dist_matcompa[rownames(trait.dist_matcompa) %in% colnames(biomasscompa),]
  
  biomasscompa0_1 <- biomasscompa
  biomasscompa0_1[biomasscompa0_1>0] <- 1
  
  biomass_compa_phylo <- biomasscompa[,colnames(biomasscompa) %in% names(tree_phylog$leaves)]
  biomass_compa_phylo <- biomass_compa_phylo/apply(biomass_compa_phylo,1,sum)
  
  if(sum(is.na(biomass_compa_phylo))>0) {next}


  #Compute HILL
  beta_hill_phylo <- chao_alpha_beta(matrix = biomass_compa_phylo,q=c(0,1,2), tree_phylog = tree_phylog)
  
  beta_hill_phylo_richess <- as.matrix(beta_hill_phylo$beta_phylo$q0)
  
  beta_hill_phylo_entropy <- as.matrix(beta_hill_phylo$beta_phylo$q1)
  
  beta_hill_taxo_richess  <- as.matrix(mFD::beta.fd.hill (asb_sp_w = biomass_mat0_1,
                                                sp_dist  = sp_dist_traits,
                                                q        = 0,
                                                tau      = "min",
                                                beta_type = "Jaccard")$beta_fd_q$q0)
  
  beta_hill_taxo_entropy  <- as.matrix(mFD::beta.fd.hill (asb_sp_w = biomass_mat,
                                                sp_dist  = sp_dist_traits,
                                                q        = 1,
                                                tau      = "min",
                                                beta_type = "Jaccard")$beta_fd_q$q1)
  
  beta_hill_fonct_richess <- as.matrix(mFD::beta.fd.hill (asb_sp_w = biomass_mat0_1,
                                                sp_dist  = sp_dist_traits,
                                                q        = 0,
                                                tau      = "mean",
                                                beta_type = "Jaccard")$beta_fd_q$q0)
  
  beta_hill_fonct_entropy <- as.matrix(mFD::beta.fd.hill (asb_sp_w = biomass_mat,
                                                sp_dist  = sp_dist_traits,
                                                q        = 1,
                                                tau      = "mean",
                                                beta_type = "Jaccard")$beta_fd_q$q1)
  
  
  ResHill[j,1] <- mean(beta_hill_taxo_richess[rownames(beta_hill_taxo_richess) %in% rownames(From10toInfdepth[j,]),
                                              colnames(beta_hill_taxo_richess) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,2] <- sd(beta_hill_taxo_richess[rownames(beta_hill_taxo_richess) %in% rownames(From10toInfdepth[j,]),
                                            colnames(beta_hill_taxo_richess) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,3] <- mean(beta_hill_taxo_entropy[rownames(beta_hill_taxo_entropy) %in% rownames(From10toInfdepth[j,]),
                                              colnames(beta_hill_taxo_entropy) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,4] <- sd(beta_hill_taxo_entropy[rownames(beta_hill_taxo_entropy) %in% rownames(From10toInfdepth[j,]),
                                            colnames(beta_hill_taxo_entropy) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,5] <- mean(beta_hill_fonct_richess[rownames(beta_hill_fonct_richess) %in% rownames(From10toInfdepth[j,]),
                                               colnames(beta_hill_fonct_richess) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,6] <- sd(beta_hill_fonct_richess[rownames(beta_hill_fonct_richess) %in% rownames(From10toInfdepth[j,]),
                                             colnames(beta_hill_fonct_richess) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,7] <- mean(beta_hill_fonct_entropy[rownames(beta_hill_fonct_entropy) %in% rownames(From10toInfdepth[j,]),
                                               colnames(beta_hill_fonct_entropy) %notin% rownames(From10toInfdepth[j,])])
  
  ResHill[j,8] <- sd(beta_hill_fonct_entropy[rownames(beta_hill_fonct_entropy) %in% rownames(From10toInfdepth[j,]),
                                             colnames(beta_hill_fonct_entropy) %notin% rownames(From10toInfdepth[j,])])
  
 }
Decay_Hill_10toInfdepth <- ResHill
save(Decay_Hill_10toInfdepth,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/Decay_Hill_10toInfdepth.RData")








############### do LMM or GLMM ########################
library(nlme)

hist(new.number.dat$number.ind)
hist(log10(new.number.dat$number.ind))

#create a model with all effects and interaction
individuals.full=lme(log10(number.ind)~depth*Island*habitat.substrat*habitat.bicenose, random= ~1|site/replicate,data=new.number.dat)

#create a model without interactions
individuals.full.no.inter=lme(log10(number.ind)~depth+Island+habitat.substrat+habitat.bicenose, random= ~1|site/replicate,data=new.number.dat)

# plot residuals of full model
plot(individuals.full.no.inter)

# plot qqplot
qqnorm(residuals(individuals.full.no.inter))
qqline(residuals(individuals.full.no.inter))

# plot hist of residuals
hist(residuals(individuals.full.no.inter))

# collect AIC
sort(c(full.inter=summary(individuals.full)$AICtab[1],
       full=summary(individuals.full.no.inter)$AICtab[1]))

# test for significance for interaction
anova(individuals.full,individuals.full.no.inter)

## do some plotting to check assumptions
# plot residuals of full model
plot(fitted(individuals.full),residuals(individuals.full))



new.number.dat$Island    <- as.factor(new.number.dat$Island  )
ggplot(data      = new.number.dat,
       aes(x     = depth,
           y     = number.ind,
           col   = island,
           group = island))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  #theme(legend.position = "none")+
  geom_smooth(method = "loess",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)





#At the site scale 
alpha_div <- merge(alpha_div, hab_pc_site_scale, by="row.names")
load("gravity.RData")
for (i in 2:ncol(gravity)){
  gravity[,i]<- as.numeric(gravity[,i])
  
}
alpha_div<- merge(alpha_div, gravity, by.x="Row.names",by.y="site",all.x=T)


ggplot(data      = alpha_div,
       aes(x     = depth,
           y     = fdiv,
           col   = island,
           group = island))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)

ggplot(data      = alpha_div,
       aes(x     = depth,
           y     = Abu,
           col   = island,
           group = island))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+ ylim(0,1500)+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)



ggplot(data      = alpha_div,
       aes(x     = grav_nearest_pop,
           y     = Abu,
           col   = classDepth,
           group = classDepth))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+ ylim(0,1500)+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)



ggplot(data      = alpha_div,
       aes(x     = gravtot100km,
           y     = sp_richn,
           col   = classDepth,
           group = classDepth))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)


