# Abundance of this grass is related to forest cover but not location
MRM(dist(LOAR10) ~ dist(sitelocation) + dist(forestpct), data=graze, nperm=10)

# Abundance of this legume is related to location but not forest cover
MRM(dist(TRRE3) ~ dist(sitelocation) + dist(forestpct), data=graze, nperm=10)

# Compare to presence/absence of grass LOAR10 using logistic regression
LOAR10.presence <- ifelse(graze$LOAR10 > 0, 1, 0)
MRM(dist(LOAR10.presence) ~ dist(sitelocation) + dist(forestpct), 
    data=graze, nperm=10, method="logistic")
# }

####################HILL SUGGESTION SEB
#Pour chaque video "profonde de D mètres" (D>10m), tu calcules ses beta avec toutes les vidéos "surfaces" (D<10m).
#Puis tu fais leur moyenne  (et sd) et tu représentes le graph Depth vs beta.
#tu as donc autant de points que de vidéos profondes (et donc aucun point entre 0 et 10 sur l'axe des X)


coord_depth_mayot <- coord_depth[rownames(coord_depth) %in% rownames(alpha_div_all),]
#coord_depth_mayot <- coord_depth
From1to10depth <- subset(coord_depth_mayot, coord_depth_mayot$depth<=16)
From10toInfdepth <- subset(coord_depth_mayot, coord_depth_mayot$depth>16)

ResHill <- matrix(NA,nrow(From10toInfdepth),12)
rownames(ResHill) <- rownames(From10toInfdepth) 
colnames(ResHill) <- c("taxo_rich_m","taxo_rich_sd",
                       "taxo_entro_m","taxo_entro_sd",
                       "fct_rich_m","fct_rich_sd",
                       "fct_entro_m","fct_entro_sd",
                       "phylo_rich_m","phylo_rich_sd",
                       "phylo_entro_m","phylo_entro_sd")


for(i in 1:nrow(From10toInfdepth)){
  print(i)
  
  biomasscompa <- biomass_mat[rownames(biomass_mat) %in% c(rownames(From10toInfdepth[i,]) , rownames(From1to10depth)),]
  biomasscompa <- biomasscompa[,apply(biomasscompa,2,sum) > 0]
  biomasscompa <- as.matrix(biomasscompa[,colnames(biomasscompa) %in% rownames(trait.dist_mat)])
  trait.dist_matcompa <- trait.dist_mat[,colnames(trait.dist_mat) %in% colnames(biomasscompa)]
  trait.dist_matcompa <- trait.dist_matcompa[rownames(trait.dist_matcompa) %in% colnames(biomasscompa),]
  
  biomasscompa0_1 <- biomasscompa
  biomasscompa0_1[biomasscompa0_1>0] <- 1
  
  biomass_compa_phylo <- biomasscompa[,colnames(biomasscompa) %in% names(tree_phylog$leaves)]
  


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
  
  ResHill[i,9] <- mean(beta_hill_phylo_richess[rownames(beta_hill_phylo_richess) %in% rownames(From10toInfdepth[i,]),
                                               colnames(beta_hill_phylo_richess) %notin% rownames(From10toInfdepth[i,])])
  
  ResHill[i,10] <- sd(beta_hill_phylo_richess[rownames(beta_hill_phylo_richess) %in% rownames(From10toInfdepth[i,]),
                                             colnames(beta_hill_phylo_richess) %notin% rownames(From10toInfdepth[i,])])
  
  ResHill[i,11] <- mean(beta_hill_phylo_entropy[rownames(beta_hill_phylo_entropy) %in% rownames(From10toInfdepth[i,]),
                                               colnames(beta_hill_phylo_entropy) %notin% rownames(From10toInfdepth[i,])])
  
  ResHill[i,12] <- sd(beta_hill_phylo_entropy[rownames(beta_hill_phylo_entropy) %in% rownames(From10toInfdepth[i,]),
                                             colnames(beta_hill_phylo_entropy) %notin% rownames(From10toInfdepth[i,])])
}
Decay_Hill <- ResHill
save(Decay_Hill,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/Decay_Hill.RData")

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


alpha_div_all



mod <- lm(sp_richn ~ PC1_hab + PC2_hab + depth,data = alpha_div_all)
mod <- lm(sp_richn ~ PC1_hab + PC2_hab + depth,data = alpha_div_all)



decay.model

beta_hill










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


