
pkgs <- c('ade4','ggplot2','betapart','harrypotter','dplyr','cluster','ape','bbmle')
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





 
# Gsad --- 
tab <- merge(species_video_scale,hab_pc_video_scale_video_scale[,c(2,9)],by.x="row.names",by.y="Row.names")
rownames(tab) <- tab[,1]
tab <- tab[,-1]

tab0_20   <- tab[tab$classDepth %in% "[0-20[",-319]
tab20_40  <- tab[tab$classDepth %in% "[20-40[",-319]
tab40_60  <- tab[tab$classDepth %in% "[40-60[",-319]
tab60_80  <- tab[tab$classDepth %in% "[60-80[",-319]
tab_sup80 <- tab[tab$classDepth %in% ">80",-319]


abu0_20   <- data.frame(abu=apply(tab0_20[,apply(tab0_20,2,sum)>0],2,sum))
abu20_40  <-data.frame(abu=apply(tab20_40[,apply(tab20_40,2,sum)>0],2,sum))
abu40_60  <- data.frame(abu=apply(tab40_60[,apply(tab40_60,2,sum)>0],2,sum))
abu60_80  <- data.frame(abu=apply(tab60_80[,apply(tab60_80,2,sum)>0],2,sum))
abu_sup80 <- data.frame(abu=apply(tab_sup80[,apply(tab_sup80,2,sum)>0],2,sum))



tab0_20   <- data.frame(abu = apply(tab0_20,2,sum))
tab20_40  <- data.frame(abu = apply(tab20_40,2,sum))
tab40_60  <- data.frame(abu = apply(tab40_60,2,sum))
tab60_80  <- data.frame(abu = apply(tab60_80,2,sum))
tab_sup80 <- data.frame(abu = apply(tab_sup80,2,sum))

tab0_20   <- tab0_20[tab0_20$abu>0, ]
tab20_40  <- tab20_40[tab20_40$abu>0, ]
tab40_60  <- tab40_60[tab40_60$abu>0, ]
tab60_80  <- tab60_80[tab60_80$abu>0, ]
tab_sup80 <- tab_sup80[tab_sup80$abu>0, ]
  
tab0_20   <- data.frame(table(tab0_20))
colnames(tab0_20)   <- c("Abu","Freq")
tab0_20$Abu <- as.numeric(as.character(tab0_20$Abu))
tab0_20$logAbu=log10(tab0_20$Abu)
tab0_20$logfreq=log10(tab0_20$Freq)
tab0_20$mlogAbu=-log10(tab0_20$Abu)
#Linear parameters
linmod0_20<- lm(log10(Freq) ~ log10(Abu), data = tab0_20)
# power bended model
mod.pb0_20 <- mle2(Freq ~ dnbinom(mu=b0*(Abu^b1)*exp(-b2*Abu), size=exp(logdisp)),data=tab0_20,control=list(maxit=1E5,trace=0),
               start=list(b0 = 10^(coef(linmod0_20)[1]), b1=coef(linmod0_20)[2],b2 = 0,logdisp=0))
tab0_20$pb <- predict(mod.pb0_20)

tab20_40  <- data.frame(table(tab20_40))
colnames(tab20_40)   <- c("Abu","Freq")
tab20_40$Abu <- as.numeric(as.character(tab20_40$Abu))
tab20_40$logAbu=log10(tab20_40$Abu)
tab20_40$logfreq=log10(tab20_40$Freq)
tab20_40$mlogAbu=-log10(tab20_40$Abu)
#Linear parameters
linmod20_40<- lm(log10(Freq) ~ log10(Abu), data = tab20_40)
# power bended model
mod.pb20_40 <- mle2(Freq ~ dnbinom(mu=b0*(Abu^b1)*exp(-b2*Abu), size=exp(logdisp)),data=tab20_40,control=list(maxit=1E5,trace=0),
                   start=list(b0 = 10^(coef(linmod20_40)[1]), b1=coef(linmod20_40)[2],b2 = 0,logdisp=0))
tab20_40$pb <- predict(mod.pb20_40)


tab40_60  <- data.frame(table(tab40_60))
colnames(tab40_60)   <- c("Abu","Freq")
tab40_60$Abu <- as.numeric(as.character(tab40_60$Abu))
tab40_60$logAbu=log10(tab40_60$Abu)
tab40_60$logfreq=log10(tab40_60$Freq)
tab40_60$mlogAbu=-log10(tab40_60$Abu)
#Linear parameters
linmod40_60<- lm(log10(Freq) ~ log10(Abu), data = tab40_60)
# power bended model
mod.pb40_60 <- mle2(Freq ~ dnbinom(mu=b0*(Abu^b1)*exp(-b2*Abu), size=exp(logdisp)),data=tab40_60,control=list(maxit=1E5,trace=0),
                   start=list(b0 = 10^(coef(linmod40_60)[1]), b1=coef(linmod40_60)[2],b2 = 0,logdisp=0))
tab40_60$pb <- predict(mod.pb40_60)


tab60_80  <- data.frame(table(tab60_80))
colnames(tab60_80)   <- c("Abu","Freq")
tab60_80$Abu <- as.numeric(as.character(tab60_80$Abu))
tab60_80$logAbu=log10(tab60_80$Abu)
tab60_80$logfreq=log10(tab60_80$Freq)
tab60_80$mlogAbu=-log10(tab60_80$Abu)
#Linear parameters
linmod60_80<- lm(log10(Freq) ~ log10(Abu), data = tab60_80)
# power bended model
mod.pb60_80 <- mle2(Freq ~ dnbinom(mu=b0*(Abu^b1)*exp(-b2*Abu), size=exp(logdisp)),data=tab60_80,control=list(maxit=1E5,trace=0),
                   start=list(b0 = 10^(coef(linmod60_80)[1]), b1=coef(linmod60_80)[2],b2 = 0,logdisp=0))
tab60_80$pb <- predict(mod.pb60_80)


tab_sup80 <- data.frame(table(tab_sup80))
colnames(tab_sup80)   <- c("Abu","Freq")
tab_sup80$Abu <- as.numeric(as.character(tab_sup80$Abu))
tab_sup80$logAbu=log10(tab_sup80$Abu)
tab_sup80$logfreq=log10(tab_sup80$Freq)
tab_sup80$mlogAbu=-log10(tab_sup80$Abu)
#Linear parameters
linmodsup80<- lm(log10(Freq) ~ log10(Abu), data = tab_sup80)
# power bended model
mod.pbsup80 <- mle2(Freq ~ dnbinom(mu=b0*(Abu^b1)*exp(-b2*Abu), size=exp(logdisp)),data=tab_sup80,control=list(maxit=1E5,trace=0),
                   start=list(b0 = 10^(coef(linmodsup80)[1]), b1=coef(linmodsup80)[2],b2 = 0,logdisp=0))
tab_sup80$pb <- predict(mod.pbsup80)

  
res<-rbind(tab0_20,tab20_40,
      tab40_60,tab60_80,
      tab_sup80)
      
res<- cbind(res,c(rep("[0-20[",nrow(tab0_20)),
                  rep("[20-40[",nrow(tab20_40)),
                  rep("[40-60[",nrow(tab40_60)),
                  rep("[60-80[",nrow(tab60_80)),
                  rep(">80",nrow(tab_sup80)))) 
colnames(res)[7]   <- "Depth"


confint(tab_sup80,method="quad")

plot(log10(Freq) ~ log10(occ_RLS), tab2)
lines(log10(tab2$occ_RLS), log10(predict(rls.pb )), col = 'green')
#n_motus = FREQ
#n = abu

# Plot 
ggplot(res, aes(x=log10(Abu), y=log10(Freq),color=Depth))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_line(aes(x=log10(Abu), y=log10(pb),color=Depth), linetype = "solid", size = 0.8)+
  annotate(geom="text", x=2, y=2, label="gSAD", hjust=1, size=5) +
  xlim(0,2)+ylim(0,2)+
  #annotate(geom="text", x=2, y=3, label="eDNA MOTUs ~ stations", hjust=1, size=4, colour="#d2981a") +
  theme_bw()+
  labs(x="log10(Abundance)",y="log10(Number of species)")



# Plot 
ggplot(res, aes(x=log10(Abu), y=log10(Freq),color=Depth))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_line(aes(x=log10(Abu), y=log10(pb),color=Depth), linetype = "solid", size = 0.8)+
  annotate(geom="text", x=2, y=2, label="gSAD", hjust=1, size=5) +
  xlim(0,2)+ylim(0,2)+
  #annotate(geom="text", x=2, y=3, label="eDNA MOTUs ~ stations", hjust=1, size=4, colour="#d2981a") +
  theme_bw()+
  labs(x="log10(Abundance)",y="log10(Number of species)")

res<-rbind(abu0_20,abu20_40,
           abu40_60,abu60_80,
           abu_sup80)

res<- cbind(res,c(rep("[0-20[",nrow(abu0_20)),
                  rep("[20-40[",nrow(abu20_40)),
                  rep("[40-60[",nrow(abu40_60)),
                  rep("[60-80[",nrow(abu60_80)),
                  rep(">80",nrow(abu_sup80)))) 
colnames(res)[2]   <- "Depth"

  
 
ggplot(res, aes(x=Depth, y=log10(abu),color=Depth)) + 
  geom_violin()+ scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1)+
  theme_bw()+ geom_jitter(position=position_jitter(0.2), cex=0.5,alpha=0.5,type=1)+
  theme(legend.position="none") + 
  ylab("Abundance") + xlab("Depth")




#violinplot des abondances pour chaque tranche de profondeur 
