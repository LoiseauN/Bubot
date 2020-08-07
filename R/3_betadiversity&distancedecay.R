
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

