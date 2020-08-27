
pkgs <- c('ade4','ggplot2','betapart','harrypotter','dplyr','cluster','ape')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


# Taxonomique diversity 
          
          #--- All region
          deth.dist.all<-dist(data.frame(row.names=rownames(hab_pc),depth=hab_pc$depth))
          all_beta<-beta.pair(species,index.family = "jaccard")

          #--- Subset Mayotte
          
              depth_mayotte <- hab_pc[hab_pc$island %in% "Mayotte",]
              species_mayotte <- species[rownames(species) %in% rownames(depth_mayotte),]
              deth.dist.mayotte<-dist(data.frame(row.names=rownames(depth_mayotte),depth=depth_mayotte$depth))
              mayotte_beta<-beta.pair(species_mayotte,index.family = "jaccard")

          #--- Subset Juan_de_nova
              
              depth_Juan_de_nova <- hab_pc[hab_pc$island %in% "Juan_de_nova",]
              species_Juan_de_nova <- species[rownames(species) %in% rownames(depth_Juan_de_nova),]
              deth.dist.Juan_de_nova<-dist(data.frame(row.names=rownames(depth_Juan_de_nova),depth=depth_Juan_de_nova$depth))
              Juan_de_nova_beta<-beta.pair(species_Juan_de_nova,index.family = "jaccard")
  
          #--- Subset Europa
              
              depth_Europa <- hab_pc[hab_pc$island %in% "Europa",]
              species_Europa <- species[rownames(species) %in% rownames(depth_Europa),]
              deth.dist.Europa<-dist(data.frame(row.names=rownames(depth_Europa),depth=depth_Europa$depth))
              Europa_beta<-beta.pair(species_Europa,index.family = "jaccard")

          dist.decay.mat <- data.frame(beta.value = c(dist2list(all_beta$beta.jac,tri = T)[,3],
                                                      dist2list(all_beta$beta.jtu,tri = T)[,3],
                                                      dist2list(all_beta$beta.jne,tri = T)[,3],
                                                      dist2list(mayotte_beta$beta.jac,tri = T)[,3],
                                                      dist2list(mayotte_beta$beta.jtu,tri = T)[,3],
                                                      dist2list(mayotte_beta$beta.jne,tri = T)[,3],
                                                      dist2list(Juan_de_nova_beta$beta.jac,tri = T)[,3],
                                                      dist2list(Juan_de_nova_beta$beta.jtu,tri = T)[,3],
                                                      dist2list(Juan_de_nova_beta$beta.jne,tri = T)[,3],
                                                      dist2list(Europa_beta$beta.jac,tri = T)[,3],
                                                      dist2list(Europa_beta$beta.jtu,tri = T)[,3],
                                                      dist2list(Europa_beta$beta.jne,tri = T)[,3]),
                                       
                                       components = c(rep("Total",length(all_beta$beta.jac)),
                                                      rep("Turnover",length(all_beta$beta.jac)),
                                                      rep("Nestedness",length(all_beta$beta.jac)),
                                                      rep("Total",length(mayotte_beta$beta.jac)),
                                                      rep("Turnover",length(mayotte_beta$beta.jac)),
                                                      rep("Nestedness",length(mayotte_beta$beta.jac)),
                                                      rep("Total",length(Juan_de_nova_beta$beta.jac)),
                                                      rep("Turnover",length(Juan_de_nova_beta$beta.jac)),
                                                      rep("Nestedness",length(Juan_de_nova_beta$beta.jac)),
                                                      rep("Total",length(Europa_beta$beta.jac)),
                                                      rep("Turnover",length(Europa_beta$beta.jac)),
                                                      rep("Nestedness",length(Europa_beta$beta.jac))),
                                       
                                       island   = c(rep("All Islands",length(all_beta$beta.jac)*3),
                                                    rep("Mayotte",length(mayotte_beta$beta.jac)*3),
                                                      rep("Juan de Nova",length(Juan_de_nova_beta$beta.jac)*3),
                                                      rep("Europa",length(Europa_beta$beta.jac)*3)),
                                       
                                       dist.depth = c(rep(dist2list(deth.dist.all,tri = T)[,3],3),
                                                      rep(dist2list(deth.dist.mayotte,tri = T)[,3],3),
                                                      rep(dist2list(deth.dist.Juan_de_nova,tri = T)[,3],3),
                                                      rep(dist2list(deth.dist.Europa,tri = T)[,3],3)))
                                       
           
          dist.decay.mat$components<-factor(dist.decay.mat$components, levels = c("Nestedness","Turnover","Total"),order=TRUE)
          
          ggplot(dist.decay.mat, aes(x=dist.depth, y=beta.value, color=components)) +
          geom_point() + 
          scale_color_hp(discrete = TRUE, option = "Slytherin", name = "Components beta",
                                         labels = c("Nestedness","Turnover","Total")) +
          stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
          theme_bw() + facet_wrap(~island)+ theme(strip.background = element_rect(fill="white"))






# Functional diversity 
          # remove species with only NA
          fish_traits <- fish_traits[rowSums(is.na(fish_traits)) < 6, ]
          rownames(fish_traits) <- fish_traits[,1]
          fish_traits<- fish_traits[,-1]
          fish_traits<- fish_traits[,c(2:6,12,13,14)]
          
          # A discuter avec THOMAS ON SUPPRIME TROP D'ESPECE, FAIRE UN EXTRACT FISHBASE? PROBLE ORTHOGRAPH
          species_funct <- species[,colnames(species) %in% rownames(fish_traits)]
          fish_traits <- fish_traits[rownames(fish_traits)  %in% colnames(species), ]
          
          #Minimum 5 esp 
          species_funct <- species_funct[apply(species_funct,1,sum)>6,]
          
          # computing PCoA ----
          trait.dist <- daisy(fish_traits,metric ="gower")
          pco.traits <- ape::pcoa(trait.dist)

          # Work with 4 dimensions
          sp_pc_coord <- pco.traits$vectors[, 1:4]
          colnames(sp_pc_coord) <- paste("PC", 1:4, sep = "")
          
          #--- All region
          deth.dist.all<-dist(data.frame(row.names=rownames(hab_pc),depth=hab_pc$depth))
          all_beta_func<-functional.beta.pair(species_funct,sp_pc_coord,index.family = "jaccard")
          
          #--- Subset Mayotte
       
          depth_mayotte <- hab_pc[hab_pc$island %in% "Mayotte",]
          species_funct_mayotte <- species_funct[rownames(species_funct) %in% rownames(depth_mayotte),]
          sp_pc_coord_mayotte <- sp_pc_coord[rownames(sp_pc_coord) %in% colnames(species_funct_mayotte),]
          deth.dist.mayotte<-dist(data.frame(row.names=rownames(depth_mayotte),depth=depth_mayotte$depth))
          mayotte_beta_func<-functional.beta.pair(species_funct,sp_pc_coord_mayotte,index.family = "jaccard")
          
          #--- Subset Juan_de_nova
          
          depth_juan_de_nova <- hab_pc[hab_pc$island %in% "Juan_de_nova",]
          species_funct_juan_de_nova <- species_funct[rownames(species_funct) %in% rownames(depth_juan_de_nova),]
          sp_pc_coord_juan_de_nova <- sp_pc_coord[rownames(sp_pc_coord) %in% colnames(species_funct_juan_de_nova),]
          deth.dist.juan_de_nova<-dist(data.frame(row.names=rownames(depth_juan_de_nova),depth=depth_juan_de_nova$depth))
          juan_de_nova_beta_func<-functional.beta.pair(species_funct,sp_pc_coord_juan_de_nova,index.family = "jaccard")
          
          #--- Subset Europa
          
          depth_europa <- hab_pc[hab_pc$island %in% "Europa",]
          species_funct_europa <- species_funct[rownames(species_funct) %in% rownames(depth_europa),]
          sp_pc_coord_europa <- sp_pc_coord[rownames(sp_pc_coord) %in% colnames(species_funct_europa),]
          deth.dist.europa<-dist(data.frame(row.names=rownames(depth_europa),depth=depth_europa$depth))
          europa_beta_func<-functional.beta.pair(species_funct,sp_pc_coord_europa,index.family = "jaccard")
          
          
          dist.decay.mat <- data.frame(beta.value = c(dist2list(all_beta_func$beta.jac,tri = T)[,3],
                                                      dist2list(all_beta_func$beta.jtu,tri = T)[,3],
                                                      dist2list(all_beta_func$beta.jne,tri = T)[,3],
                                                      dist2list(mayotte_beta_func$beta.jac,tri = T)[,3],
                                                      dist2list(mayotte_beta_func$beta.jtu,tri = T)[,3],
                                                      dist2list(mayotte_beta_func$beta.jne,tri = T)[,3],
                                                      dist2list(Juan_de_nova_beta_func$beta.jac,tri = T)[,3],
                                                      dist2list(Juan_de_nova_beta_func$beta.jtu,tri = T)[,3],
                                                      dist2list(Juan_de_nova_bet_funca$beta.jne,tri = T)[,3],
                                                      dist2list(Europa_beta_func$beta.jac,tri = T)[,3],
                                                      dist2list(Europa_beta_func$beta.jtu,tri = T)[,3],
                                                      dist2list(Europa_beta_func$beta.jne,tri = T)[,3]),
                                       
                                       components = c(rep("Total",length(all_beta_func$beta.jac)),
                                                      rep("Turnover",length(all_beta_func$beta.jac)),
                                                      rep("Nestedness",length(all_beta_func$beta.jac)),
                                                      rep("Total",length(mayotte_beta_func$beta.jac)),
                                                      rep("Turnover",length(mayotte_beta_func$beta.jac)),
                                                      rep("Nestedness",length(mayotte_beta_func$beta.jac)),
                                                      rep("Total",length(Juan_de_nova_beta_func$beta.jac)),
                                                      rep("Turnover",length(Juan_de_nova_beta_func$beta.jac)),
                                                      rep("Nestedness",length(Juan_de_nova_beta_func$beta.jac)),
                                                      rep("Total",length(Europa_beta_func$beta.jac)),
                                                      rep("Turnover",length(Europa_beta_func$beta.jac)),
                                                      rep("Nestedness",length(Europa_beta_func$beta.jac))),
                                       
                                       island   = c(rep("All Islands",length(all_beta_func$beta.jac)*3),
                                                    rep("Mayotte",length(mayotte_beta_func$beta.jac)*3),
                                                    rep("Juan de Nova",length(Juan_de_nova_beta_func$beta.jac)*3),
                                                    rep("Europa",length(Europa_beta_func$beta.jac)*3)),
                                       
                                       dist.depth = c(rep(dist2list(deth.dist.all,tri = T)[,3],3),
                                                      rep(dist2list(deth.dist.mayotte,tri = T)[,3],3),
                                                      rep(dist2list(deth.dist.Juan_de_nova,tri = T)[,3],3),
                                                      rep(dist2list(deth.dist.Europa,tri = T)[,3],3)))
          
          
          dist.decay.mat$components<-factor(dist.decay.mat$components, levels = c("Nestedness","Turnover","Total"),order=TRUE)
          
          ggplot(dist.decay.mat, aes(x=dist.depth, y=beta.value, color=components)) +
            geom_point() + 
            scale_color_hp(discrete = TRUE, option = "Slytherin", name = "Components beta",
                           labels = c("Nestedness","Turnover","Total")) +
            stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
            theme_bw() + facet_wrap(~island)+ theme(strip.background = element_rect(fill="white"))

#--- Triangle plot NOT WORKING FOR NOW
beta.test <- data.frame(site1 = dist2list(mayotte_beta$beta.jac,tri = T)[,1],
                        site2 = dist2list(mayotte_beta$beta.jac,tri = T)[,2],
                        Tot =  dist2list(mayotte_beta$beta.jac,tri = T)[,3]*100,
                        Tur =  dist2list(mayotte_beta$beta.jtu,tri = T)[,3]*100,
                        Nes =  dist2list(mayotte_beta$beta.jne,tri = T)[,3]*100,
                        dist.depth =dist2list(deth.dist.mayotte,tri = T)[,3])

beta.test$classDepth.dist <- NA

for (i in 1: nrow(beta.test)){
  if(beta.test$dist.depth[i]<20){ beta.test$classDepth.dist[i] <- "[0-20["}
  if(beta.test$dist.depth[i]>=20 & beta.test$dist.depth[i]<40){ beta.test$classDepth.dist[i] <- "[20-40["}
  if(beta.test$dist.depth[i]>=40 & beta.test$dist.depth[i]<60){ beta.test$classDepth.dist[i] <- "[40-60["}
  if(beta.test$dist.depth[i]>=60 & beta.test$dist.depth[i]<80){ beta.test$classDepth.dist[i] <- "[60-80["}
  if(beta.test$dist.depth[i]>80){ beta.test$classDepth.dist[i] <- ">80"}
  }

base = ggtern(beta.test,aes(Tur+Nes,Tur,Nes,color=classDepth.dist,fill=classDepth.dist,shape=classDepth.dist)) + 
  theme_bw() + 
  theme_legend_position('tr') + 
  geom_encircle(alpha=0.5,size=1) + 
  geom_point() +
  labs(title    = "Example Plot",
       subtitle = "using geom_encircle")
print(base)






# Gsad
#The first distribution (green) is the observed number of observations per species for all species found in ecological plots.
#Each data point represents the total number of individuals observed for a given species
species_site_scale0_1 <- species_site_scale 
species_site_scale0_1[species_site_scale0_1>0] <- 1
NbS <- apply(species_site_scale0_1,1,sum)
NbObs_perSpecies <- apply(species_site_scale0_1,2,sum)
Abu_perSpecies <- apply(species_site_scale,2,sum)

plot(NbS~NbObs_perSpecies)


library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)
library(scales)
library(plyr)
library(dplyr)
library(conflicted)
library(gambin)
library(sads)
library(vegan)
library(bbmle)
library(nlreg)
library(MASS)
library(fitdistrplus)

# repertoire David

setwd("/Users/davidmouillot/Documents/articles/en cours/Global eDNA")


# fit log-log models

load("rarete_motu_station.rdata")
tab=as.data.frame(motu_station)

logn=log10(tab$n)
logmotus=log10(tab$n_motus)
mlogn=-log10(tab$n)

tab=cbind(tab,logn,logmotus,mlogn)

head(tab)

load("rarete_species_transects.rdata")
tab2 <- species_transects

logocc=log10(tab2$occ_RLS)
logfreq=log10(tab2$Freq)
mlogocc=-log10(tab2$occ_RLS)

tab2=cbind(tab2,logocc,logfreq,mlogocc)

head(tab2)

# distribution

hist(tab$n_motus)

motus.poisson=fitdist(tab$n_motus, "pois")

motus.nb=fitdist(tab$n_motus, "nbinom")

gofstat(list(motus.poisson, motus.nb),fitnames = c("Poisson", "Negative Binomial"))



hist(tab2$Freq)

rls.poisson=fitdist(tab2$Freq, "pois")

rls.nb=fitdist(tab2$Freq, "nbinom")

gofstat(list(rls.poisson, rls.nb),fitnames = c("Poisson", "Negative Binomial"))


# fit non linear regression by maximum likelihood

# power law

# initial parameters

linmod.motus <- lm(log10(n_motus) ~ log10(n), data = tab)

coef(linmod.motus)

edna.po <- mle2(n_motus ~ dnbinom(mu=b0*n^b1, size=exp(logdisp)),data=tab,
                start=list(b0 = 10^(coef(linmod.motus)[1]), b1 = coef(linmod.motus)[2],logdisp=0))

confint(edna.po,method="quad")

plot(log10(n_motus) ~ log10(n), tab)
lines(log10(tab$n), log10(predict(edna.po)), col = 'blue')



linmod.rls <- lm(log10(Freq) ~ log10(occ_RLS), data = tab2)

coef(linmod.rls)

rls.po <- mle2(Freq ~ dnbinom(mu=b0*occ_RLS^b1, size=exp(logdisp)),data=tab2,
               start=list(b0 = 10^(coef(linmod.rls)[1]), b1 = coef(linmod.rls)[2],logdisp=0))

confint(rls.po,method="quad")

plot(log10(Freq) ~ log10(occ_RLS), tab2)
lines(log10(tab2$occ_RLS), log10(predict(rls.po)), col = 'blue')


# log series

edna.ls <- mle2(n_motus ~ dnbinom(mu=b0*(1/n)*exp(-b2*n), size=exp(logdisp)),data=tab,control=list(maxit=1E5,trace=0),
                start=list(b0 = 426, b2 = 0,logdisp=0))

plot(log10(n_motus) ~ log10(n), tab)
lines(log10(tab$n), log10(predict(edna.ls)), col = 'red')

confint(edna.ls,method="quad")

rls.ls <- mle2(Freq ~ dnbinom(mu=b0*(1/occ_RLS)*exp(-b2*occ_RLS), size=exp(logdisp)),data=tab2,
               start=list(b0 = 256, b2 = 0,logdisp=0))

plot(log10(Freq) ~ log10(occ_RLS), tab2)
lines(log10(tab2$occ_RLS), log10(predict(rls.ls )), col = 'red')

confint(rls.ls,method="quad")



# power bended

edna.pb <- mle2(n_motus ~ dnbinom(mu=b0*(n^b1)*exp(-b2*n), size=exp(logdisp)),data=tab,control=list(maxit=1E5,trace=0),
                start=list(b0 = 10^(coef(linmod.motus)[1]), b1=coef(linmod.motus)[2],b2 = 0,logdisp=0))


confint(edna.pb,method="quad")

plot(log10(n_motus) ~ log10(n), tab)
lines(log10(tab$n), log10(predict(edna.pb)), col = 'green')


rls.pb <- mle2(Freq ~ dnbinom(mu=b0*(occ_RLS^b1)*exp(-b2*occ_RLS), size=exp(logdisp)),data=tab2,
               start=list(b0 = 10^(coef(linmod.rls)[1]), b1=coef(linmod.rls)[2],b2 = 0,logdisp=0))

confint(rls.pb,method="quad")

plot(log10(Freq) ~ log10(occ_RLS), tab2)
lines(log10(tab2$occ_RLS), log10(predict(rls.pb )), col = 'green')


# model comparisons

AICtab(edna.po, edna.ls, edna.pb, weights=TRUE)

anova(edna.ls,edna.pb)
anova(edna.po,edna.pb)

AICtab(rls.po, rls.ls, rls.pb, weights=TRUE)

anova(rls.po,rls.pb)
anova(rls.ls,rls.pb)


# Figures

# predict for each model
tab$pb <- predict(edna.pb)
tab$po <- predict(edna.po)
tab$ls <- predict(edna.ls)

tab2$pb <- predict(rls.pb)
tab2$po <- predict(rls.po)
tab2$ls <- predict(rls.ls)



# plot figure 4a edna
ggplot(tab, aes(x=log10(n), y=log10(n_motus)))+
  geom_point(colour="#d2981a", size=2, show.legend = TRUE)+
  geom_line(aes(x=log10(n), y=log10(po)), linetype = "dashed", size = 0.8)+
  geom_line(aes(x=log10(n), y=log10(pb)), linetype = "solid", size = 0.8)+
  geom_line(aes(x=log10(n), y=log10(ls)), linetype = "dotted", size = 0.8)+
  xlim(0,2)+
  ylim(0,3)+
  annotate(geom="text", x=2, y=3, label="eDNA MOTUs ~ stations", hjust=1, size=4, colour="#d2981a") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=12, face = "bold"))+
  labs(x="log10(Number of station)",y="log10(Number of MOTUs)")

ggsave("Figure4a.png")



# plot figure 4b rls
ggplot(tab2, aes(x=log10(occ_RLS), y=log10(Freq)))+
  geom_point(colour = "darkgrey", size=2, show.legend = TRUE)+
  geom_line(aes(x=log10(occ_RLS), y=log10(po)), linetype = "dashed", size = 0.8)+
  geom_line(aes(x=log10(occ_RLS), y=log10(pb)), linetype = "solid", size = 0.8)+
  geom_line(aes(x=log10(occ_RLS), y=log10(ls)), linetype = "dotted", size = 0.8)+
  xlim(0,3)+
  ylim(0,3)+
  annotate(geom="text", x=3, y=3, label="RLS species ~ transects", hjust=1, size=4) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=12, face = "bold"))+
  labs(x="log10(Number of transect)",y="log10(Number of species)")

ggsave("Figure4b.png")

