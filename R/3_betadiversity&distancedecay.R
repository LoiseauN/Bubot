load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")

pkgs <- c('ade4','ggplot2','betapart','harrypotter','dplyr','cluster','ape','bbmle')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


# Taxonomique diversity 
 
#Site Scale         
          #--- All region
          deth.dist.all<-dist(data.frame(row.names=rownames(hab_pc_site_scale),depth=hab_pc_site_scale$depth))
          species_site_scale0_1 <- species_site_scale
          species_site_scale0_1[species_site_scale0_1>1] <- 1
          all_beta<-beta.pair(species_site_scale0_1,index.family = "jaccard")
          pco.jac_all<- ape::pcoa(all_beta$beta.jac)$vectors[,c(1:2)]
          pco.jac_all<- cbind(pco.jac_all,hab_pc_site_scale) 

          all_beta_bray<-beta.pair.abund(species_site_scale,index.family = "bray")
          pco.bray_all<- ape::pcoa(all_beta_bray$beta.bray)$vectors[,c(1:2)]
          pco.bray_all<- cbind(pco.bray_all,hab_pc_site_scale) 
          
          pco.plot_all <- ggplot(pco.jac_all, aes(x=Axis.1, y=Axis.2,color=classDepth))+
            geom_point(size=2, show.legend = TRUE)+
            #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
            scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
            theme_bw()+ 
            theme(legend.position = "right")+
            labs(x="PCoA1",y="PCoA2") +
            geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                          alpha = 0.7, show.legend = FALSE)
          
        ggplot(pco.bray_all, aes(x=Axis.1, y=Axis.2,color=classDepth))+
            geom_point(size=2, show.legend = TRUE)+
            #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
            scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
            theme_bw()+ 
            theme(legend.position = "right")+
            labs(x="PCoA1",y="PCoA2") +
            geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                          alpha = 0.7, show.legend = FALSE)
          
          #video Scale n'a pas de sens ici! Données trop bruité         
     
          
          #--- Subset Mayotte
          
              depth_mayotte <- hab_pc_site_scale[hab_pc_site_scale$island %in% "Mayotte",]
              species_site_scale0_1_mayotte <- species_site_scale0_1[rownames(species_site_scale0_1) %in% rownames(depth_mayotte),]
              deth.dist.mayotte<-dist(data.frame(row.names=rownames(depth_mayotte),depth=depth_mayotte$depth))
              mayotte_beta<-beta.pair(species_site_scale0_1_mayotte,index.family = "jaccard")
             
              pco.jac_mayotte<- ape::pcoa(mayotte_beta$beta.jac)$vectors[,c(1:2)]
              pco.jac_mayotte<- cbind(pco.jac_mayotte,depth_mayotte) 
              
              
              pco.plot_mayotte <- ggplot(pco.jac_mayotte, aes(x=Axis.1, y=Axis.2,color=classDepth))+
                geom_point(size=2, show.legend = TRUE)+
                #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
                scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
                theme_bw()+ 
                theme(legend.position = "right")+
                labs(x="PCoA1",y="PCoA2") +
                geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                              alpha = 0.7, show.legend = FALSE)
         
              #--- Subset Juan_de_nova
              
              depth_Juan_de_nova <- hab_pc_site_scale[hab_pc_site_scale$island %in% "Juan_de_nova",]
              species_site_scale0_1_Juan_de_nova <- species_site_scale0_1[rownames(species_site_scale0_1) %in% rownames(depth_Juan_de_nova),]
              deth.dist.Juan_de_nova<-dist(data.frame(row.names=rownames(depth_Juan_de_nova),depth=depth_Juan_de_nova$depth))
              Juan_de_nova_beta<-beta.pair(species_site_scale0_1_Juan_de_nova,index.family = "jaccard")
              
              
              pco.jac_Juan_de_nova<- ape::pcoa(Juan_de_nova_beta$beta.jac)$vectors[,c(1:2)]
              pco.jac_Juan_de_nova<- cbind(pco.jac_Juan_de_nova,depth_Juan_de_nova) 
              
              
              pco.plot_Juan_de_nova <- ggplot(pco.jac_Juan_de_nova, aes(x=Axis.1, y=Axis.2,color=classDepth))+
                geom_point(size=2, show.legend = TRUE)+
                #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
                scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
                theme_bw()+ 
                theme(legend.position = "right")+
                labs(x="PCoA1",y="PCoA2") +
                geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                              alpha = 0.7, show.legend = FALSE)
  
          #--- Subset Europa
              
              depth_Europa <- hab_pc_site_scale[hab_pc_site_scale$island %in% "Europa",]
              species_site_scale0_1_Europa <- species_site_scale0_1[rownames(species_site_scale0_1) %in% rownames(depth_Europa),]
              deth.dist.Europa<-dist(data.frame(row.names=rownames(depth_Europa),depth=depth_Europa$depth))
              Europa_beta<-beta.pair(species_site_scale0_1_Europa,index.family = "jaccard")
              
              
              pco.jac_Europa<- ape::pcoa(Europa_beta$beta.jac)$vectors[,c(1:2)]
              pco.jac_Europa<- cbind(pco.jac_Europa,depth_Europa) 
              
              
              pco.plot_Europa <- ggplot(pco.jac_Europa, aes(x=Axis.1, y=Axis.2,color=classDepth))+
                geom_point(size=2, show.legend = TRUE)+
                #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
                scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
                theme_bw()+ 
                theme(legend.position = "right")+
                labs(x="PCoA1",y="PCoA2") +
                geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                              alpha = 0.7, show.legend = FALSE)
              

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

#  Functional betadiversity with FE
          tab <- merge(dat_complet,hab_pc_video_scale[,c(1:2)],by.x="VideoID",by.y="Row.names",all.x=T)
          species_site_scale_FE<-as.data.frame.matrix(xtabs(value~Sample.code+FE,data= tab))
          species_site_scale_FE0_1 <- species_site_scale_FE
          species_site_scale_FE0_1[species_site_scale_FE0_1>1] <- 1
          all_FE_beta<-beta.pair(species_site_scale_FE0_1,index.family = "jaccard")
          pco.jac_all_FE<- ape::pcoa(all_FE_beta$beta.jac)$vectors[,c(1:2)]
          
          #ATTENTION VERIFIER POURQUOI DES SITES N Y SONT PLUS!!!!
          pco.jac_all_FE<- merge(pco.jac_all_FE,hab_pc_site_scale,by="row.names",all.x=T) 
          
          all_FE_beta_bray<-beta.pair.abund(species_site_scale_FE,index.family = "bray")
          pco.bray_all_FE<- ape::pcoa(all_FE_beta_bray$beta.bray)$vectors[,c(1:2)]
          #ATTENTION VERIFIER POURQUOI DES SITES N Y SONT PLUS!!!!
          pco.bray_all_FE<- merge(pco.bray_all_FE,hab_pc_site_scale,by="row.names",all.x=T) 
          
          pco.plot_all_FE <- ggplot(pco.jac_all_FE, aes(x=Axis.1, y=Axis.2,color=classDepth))+
            geom_point(size=2, show.legend = TRUE)+
            #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
            scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
            theme_bw()+ 
            theme(legend.position = "right")+
            labs(x="PCoA1",y="PCoA2") +
            geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                          alpha = 0.7, show.legend = FALSE)
          
          ggplot(pco.bray_all_FE, aes(x=Axis.1, y=Axis.2,color=classDepth))+
            geom_point(size=2, show.legend = TRUE)+
            #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
            scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
            theme_bw()+ 
            theme(legend.position = "right")+
            labs(x="PCoA1",y="PCoA2") +
            geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                          alpha = 0.7, show.legend = FALSE)
# TRUE Functional betadiversity 
         abumat0_1 <- abumat
         abumat0_1[abumat0_1>0] <-1
         abumat0_1 <- abumat0_1[apply(abumat0_1,1,sum)>4,]
         beta_div_fct <- beta.fd.multidim(
                            sp_faxes_coord = coord,
                            asb_sp_occ     = abumat0_1,
                            check.input    = TRUE,
                            beta.family    = "Jaccard"
                          )
          
         store_details  = TRUE,
         betapart.step  = FALSE,
         betapart.para  = FALSE,
         betapart.para.opt = betapart::beta.para.control(nc = 4, LB = TRUE)
          
          
  
      
          pco.plot_all_fct <- ggplot(pco.jac_all_fct, aes(x=Axis.1, y=Axis.2,color=classDepth))+
            geom_point(size=2, show.legend = TRUE)+
            #scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
            scale_color_hp(discrete = TRUE, option = "Ravenclaw", name = "Depth",direction = -1) +
            theme_bw()+ 
            theme(legend.position = "right")+
            labs(x="PCoA1",y="PCoA2") +
            geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                          alpha = 0.7, show.legend = FALSE)
          
            #--- Subset Mayotte
          mayotte_beta_func <- list(beta.jac=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Mayotte")$Row.names,],
                                    beta.jtu=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Mayotte")$Row.names,],
                                    beta.jne=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Mayotte")$Row.names,])
            #--- Subset Juan de Nova  
          Juan_de_nova_beta_func <- list(beta.jac=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Juan_de_nova")$Row.names,],
                                           beta.jtu=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Juan_de_nova")$Row.names,],
                                           beta.jne=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Juan_de_nova")$Row.names,])
          
            #--- Subset Europa
          Europa_beta_func <- list(beta.jac=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Europa")$Row.names,],
                                     beta.jtu=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Europa")$Row.names,],
                                     beta.jne=BetaFCTtot[rownames(BetaFCTtot) %in% subset(pco.jac_all_fct,pco.jac_all_fct$island=="Europa")$Row.names,])
          
          
          deth.dist.all <- deth.dist.all[,colnames(deth.dist.all) %in% rownames(BetaFCTtot)]
          deth.dist.all <- deth.dist.all[rownames(deth.dist.all) %in% rownames(BetaFCTtot),]
          
          deth.dist.mayotte <-deth.dist.mayotte[,colnames(deth.dist.mayotte) %in% rownames(mayotte_beta_func$beta.jac)]
          deth.dist.mayotte <-  deth.dist.mayotte[rownames(deth.dist.mayotte) %in% rownames(mayotte_beta_func$beta.jac),]
          
          deth.dist.Juan_de_nova <-deth.dist.Juan_de_nova[,colnames(deth.dist.Juan_de_nova) %in% rownames(Juan_de_nova_beta_func$beta.jac)]
          deth.dist.Juan_de_nova <-deth.dist.Juan_de_nova[rownames(deth.dist.Juan_de_nova) %in% rownames(Juan_de_nova_beta_func$beta.jac),]
            
          deth.dist.Europa <-deth.dist.Europa[,colnames(deth.dist.Europa) %in% rownames(Europa_beta_func$beta.jac)]
          deth.dist.Europa <-   deth.dist.Europa[rownames(deth.dist.Europa) %in% rownames(Europa_beta_func$beta.jac),]
          
            dist.decay.mat <- data.frame(beta.value = c(dist2list(as.dist(BetaFCTtot),tri = T)[,3],
                                                dist2list(as.dist(BetaFCTtur),tri = T)[,3],
                                                dist2list(as.dist(BetaFCTnes),tri = T)[,3],
                                                dist2list(as.dist(mayotte_beta_func$beta.jac),tri = T)[,3],
                                                dist2list(as.dist(mayotte_beta_func$beta.jtu),tri = T)[,3],
                                                dist2list(as.dist(mayotte_beta_func$beta.jne),tri = T)[,3],
                                                dist2list(as.dist(Juan_de_nova_beta_func$beta.jac),tri = T)[,3],
                                                dist2list(as.dist(Juan_de_nova_beta_func$beta.jtu),tri = T)[,3],
                                                dist2list(as.dist(Juan_de_nova_beta_func$beta.jne),tri = T)[,3],
                                                dist2list(as.dist(Europa_beta_func$beta.jac),tri = T)[,3],
                                                dist2list(as.dist(Europa_beta_func$beta.jtu),tri = T)[,3],
                                                dist2list(as.dist(Europa_beta_func$beta.jne),tri = T)[,3]),
                                       
                                       components = c(rep("Total",length(as.dist(BetaFCTtot))),
                                                      rep("Turnover",length(as.dist(BetaFCTtot))),
                                                      rep("Nestedness",length(as.dist(BetaFCTtot))),
                                                      rep("Total",length(as.dist(mayotte_beta_func$beta.jac))),
                                                      rep("Turnover",length(as.dist(mayotte_beta_func$beta.jac))),
                                                      rep("Nestedness",length(as.dist(mayotte_beta_func$beta.jac))),
                                                      rep("Total",length(as.dist(Juan_de_nova_beta_func$beta.jac))),
                                                      rep("Turnover",length(as.dist(Juan_de_nova_beta_func$beta.jac))),
                                                      rep("Nestedness",length(as.dist(Juan_de_nova_beta_func$beta.jac))),
                                                      rep("Total",length(as.dist(Europa_beta_func$beta.jac))),
                                                      rep("Turnover",length(as.dist(Europa_beta_func$beta.jac))),
                                                      rep("Nestedness",length(as.dist(Europa_beta_func$beta.jac)))),
                                       
                                       island   = c(rep("All Islands",length(as.dist(BetaFCTtot))*3),
                                                    rep("Mayotte",length(as.dist(mayotte_beta_func$beta.jac))*3),
                                                    rep("Juan de Nova",length(as.dist(Juan_de_nova_beta_func$beta.jac))*3),
                                                    rep("Europa",length(as.dist(Europa_beta_func$beta.jac))*3)),
                                       
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
tab <- merge(species_site_scale0_1_video_scale,hab_pc_site_scale_video_scale[,c(2,9)],by.x="row.names",by.y="Row.names")
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
  labs(x="log10(Abundance)",y="log10(Number of species_site_scale0_1)")



# Plot 
ggplot(res, aes(x=log10(Abu), y=log10(Freq),color=Depth))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_line(aes(x=log10(Abu), y=log10(pb),color=Depth), linetype = "solid", size = 0.8)+
  annotate(geom="text", x=2, y=2, label="gSAD", hjust=1, size=5) +
  xlim(0,2)+ylim(0,2)+
  #annotate(geom="text", x=2, y=3, label="eDNA MOTUs ~ stations", hjust=1, size=4, colour="#d2981a") +
  theme_bw()+
  labs(x="log10(Abundance)",y="log10(Number of species_site_scale0_1)")

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
