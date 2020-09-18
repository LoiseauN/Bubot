
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
          
          vecpco <- unique(dat_complet[,c("variable","PC1","PC2","PC3","PC4")])
          rownames(vecpco) <- vecpco[,1]
          vecpco <- vecpco[,-1]
          
          #Minimum 5 esp 
          species_site_scale0_1_funct <- species_site_scale0_1
          species_site_scale0_1_funct <- species_site_scale0_1_funct[apply(species_site_scale0_1_funct,1,sum)>4,]
          
          #--- All region
          deth.dist.all <- dist(data.frame(row.names=rownames(hab_pc_site_scale),depth=hab_pc_site_scale$depth))
        
         
          BetaFCTtot<-matrix(NA,nrow(species_site_scale0_1_funct),nrow(species_site_scale0_1_funct))
          BetaFCTtur<-matrix(NA,nrow(species_site_scale0_1_funct),nrow(species_site_scale0_1_funct))
          BetaFCTnes<-matrix(NA,nrow(species_site_scale0_1_funct),nrow(species_site_scale0_1_funct))
          
          for (i in 1:nrow(species_site_scale0_1_funct)){
            
            print(paste0("i = ",i ))
            
            for (j in 1:nrow(species_site_scale0_1_funct)){
              
              print(paste0("j = ",j ))
              
              mat<-rbind(species_site_scale0_1_funct[i,],species_site_scale0_1_funct[j,])
              mat<-mat[,apply(mat,2,sum)>0]
              
              tr  <- vecpco[rownames(vecpco) %in% colnames(mat),]
              mat <- mat[,colnames(mat) %in% rownames(tr)]
              
              mat <- mat[ , order(names(mat))]
              tr <- tr[order(rownames(tr)) , ]
              
              result<-tryCatch(
                fbc<-testbetag(x=mat, traits=tr, multi = TRUE, warning.time = TRUE, return.details = FALSE, 
                                fbc.step = FALSE, parallel = FALSE, opt.parallel = beta.para.control(), 
                                inter =  "geom", qhull.opt = list(geom = NULL)),
                error=function(err){result="NA"}
                )
              if((is(result)[1]=="character")=="TRUE") next
              
              funct.beta.jtu <- (2 * fbc$min.not.shared)/((2 * fbc$min.not.shared) + 
                                                            fbc$shared)
              
              funct.beta.jne <- ((fbc$max.not.shared - fbc$min.not.shared)/(fbc$shared + 
                                    fbc$sum.not.shared)) * (fbc$shared/((2 * fbc$min.not.shared) + 
                                    fbc$shared))
              
              funct.beta.jac <- fbc$sum.not.shared/(fbc$shared + fbc$sum.not.shared)
              
              BetaFCTtot[i,j] <- funct.beta.jac[2,1]
              BetaFCTtur[i,j] <- funct.beta.jtu[2,1]
              BetaFCTnes[i,j] <- funct.beta.jne[2,1]
              
              colnames(BetaFCTtot) <- rownames(species_site_scale0_1_funct)
              colnames(BetaFCTtur) <- colnames(BetaFCTtot)
              colnames(BetaFCTnes) <- colnames(BetaFCTtot)
              
              rownames(BetaFCTtot) <- colnames(BetaFCTtot)
              rownames(BetaFCTtur) <- colnames(BetaFCTtot)
              rownames(BetaFCTnes) <- colnames(BetaFCTtot)
              
              #save(BetaFCTtot,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/results/BetaFCTtot.RData")
              #save(BetaFCTtur,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/results/BetaFCTtur.RData")
              #save(BetaFCTnes,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/results/BetaFCTnes.RData")
              
            }
          }
          
          test <- BetaFCTtot[,colSums(is.na(BetaFCTtot)) < ncol(BetaFCTtot)/2 ]
          test <-na.omit(test)
          
          pco.jac_all_fct<- ape::pcoa(as.dist(test))$vectors[,c(1:2)]
          pco.jac_all_fct<- merge(pco.jac_all_fct,hab_pc_site_scale,by="row.names",all.x=T) 
      
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
testbeta <- function (x, traits, multi = TRUE, warning.time = TRUE, return.details = FALSE, 
                      fbc.step = FALSE, parallel = FALSE, opt.parallel = beta.para.control(), 
                      qhull.opt = NULL) 
{
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) 
    stop("The data in 'x' is not numeric.", call. = TRUE)
  xvals <- unique(as.vector(x))
  if (any(!is.element(xvals, c(0, 1)))) 
    stop("The 'x' table contains values other than 0 and 1: data should be presence/absence.", 
         call. = TRUE)
  if (!is.matrix(traits)) {
    traits <- as.matrix(traits)
  }
  if (is.null(row.names(x))) 
    stop("'x' should have row names with site names", call. = TRUE)
  if (is.null(colnames(x))) 
    stop("'x' should have column names with species names", 
         call. = TRUE)
  if (is.null(row.names(traits))) 
    stop("'traits' should have row names with species names", 
         call. = TRUE)
  if (is.null(colnames(traits))) 
    stop("'traits' should have columns names with traits names", 
         call. = TRUE)
  if (any(colnames(x) != row.names(traits))) 
    stop("Species names in 'x' and 'traits' must be identical (including order)", 
         call. = TRUE)
  if (!is.numeric(traits)) 
    stop("The data in 'traits' is not numeric.", call. = TRUE)
  if (any(is.na(traits))) 
    stop("NA are not allowed in 'traits'", call. = TRUE)
  if (ncol(x) != nrow(traits)) 
    stop("Number of species in 'x' and 'traits' must be identical", 
         call. = TRUE)
  if (!is.null(qhull.opt) && !is.character(qhull.opt))
    stop("qhull.opt is not character")
  if (!"FA" %in% qhull.opt)
    qhull.opt <- c("FA", qhull.opt)
  qhull.opt <- paste(qhull.opt, collapse = " ")
  if (parallel) {
    control.para <- beta.para.control()
    if (!missing(opt.parallel)) {
      control.para[names(opt.parallel)] <- opt.parallel
    }
    nc <- control.para$nc
    if (!is.numeric(nc)) 
      stop("nc must be numeric (integer)", call. = TRUE)
    nc <- as.integer(nc)
    type <- control.para$type
    if (!type %in% c("SOCK", "PSOCK", "FORK")) 
      stop("type only supoort (P)SOCK or FORK", call. = TRUE)
    if (type == "FORK" && Sys.info()["sysname"] == "Windows") 
      stop("Only SOCK clusters are enabled on Windows", 
           call. = TRUE)
    LB <- control.para$LB
    if (!is.logical(LB)) 
      stop("LB must be logical", call. = TRUE)
    size <- control.para$size
    if (!is.null(size) && !is.numeric(size)) 
      stop("size must be numeric (integer)", call. = TRUE)
  }
  f1 <- function(z, N, D) {
    comb_z <- combn(N, z, FUN = paste, collapse = "_")
    comb_inter_z2 <- comb_inter[[z - 2]]
    coord_vert_inter_e <- coord_vert_inter[[z - 2]]
    vol_inter_z <- rep(0, length(comb_z))
    coord_vert_inter_z <- list()
    n1 <- sub("_\\d+$", "", comb_z)
    n2 <- sub("^\\d+_", "", comb_z)
    n1 <- fmatch(n1, comb_inter_z2, nomatch = NA)
    n2 <- fmatch(n2, comb_inter_z2, nomatch = NA)
    for (k in 1:length(comb_z)) {
      seti <- coord_vert_inter_e[[n1[k]]]
      setj <- coord_vert_inter_e[[n2[k]]]
      coord_vert_inter_z[[k]] <- rep(NA, D)
      if (!is.na(sum(seti) + sum(setj))) {
        interij <- inter(seti, setj)
        vol_inter_z[k] <- interij$vol_inter
        coord_vert_inter_z[[k]] <- interij$coord_vert_inter
      }
    }
    return(list(comb_z = comb_z, coord_vert_inter_z = coord_vert_inter_z, 
                vol_inter_z = vol_inter_z))
  }
  D <- ncol(traits)
  Si <- rowSums(x)
  if (any(Si <= D)) 
    stop(paste("'community ", row.names(x)[which(Si <= D)], 
               " must contain at least ", D + 1, " species", sep = ""))
  N <- nrow(x)
  if (N < 2) 
    stop("Computing dissimilairty requires at least 2 communities", 
         call. = TRUE)
  if (multi) {
    if (fbc.step) {
      fbc.step <- FALSE
      warnings("As multi = TRUE, fbc.step was set to FALSE")
    }
    if (warning.time & N > 10) 
      stop(paste("Computing mulitple functional dissimilarity on more than 10 communities may take a long time. \n    \t\t\t\t\t\t\t\t\tSet 'multi' or 'warning.time' to FALSE"))
    if (warning.time & D > 4) 
      stop(paste("Computing mulitple functional dissimilarity in a", 
                 D, "-dimensions functional space may take a long time. \n    \t\t\t\t\t\t\t\t\tSet 'multi' or 'warning.time' to FALSE"))
  }
  nb.step <- 2
  if (multi) 
    nb.step <- N
  if (fbc.step) {
    step.fbc <- as.data.frame(matrix("", nb.step, 1, dimnames = list(c("           FRi", 
                                                                       paste("intersection", 2:nb.step, sep = "_")), c("iteration"))))
    step.fbc[, 1] <- as.character(step.fbc[, 1])
    step.fbc[1, 1] <- paste("0/", N, sep = "")
    for (k in 2:nb.step) step.fbc[k, 1] <- paste("0/", choose(N, 
                                                              k), sep = "")
  }
  FRi <- numeric(N)
  names(FRi) <- row.names(x)
  coord_vert_i <- vector(mode = "list", length = N)
  for (i in 1:N) {
    tr_i <- traits[which(x[i, ] == 1), ]
    ch_i <- convhulln(tr_i, options = "FA")
    FRi[i] <- ch_i$vol
    coord_vert_i[[i]] <- tr_i[unique(as.integer(ch_i$hull)), 
    ]
  }
  names(coord_vert_i) <- row.names(x)
  sumFRi <- sum(FRi)
  inter <- function(set1, set2, qhull.opt = "FA") {
    set1rep <- d2q(cbind(0, cbind(1, set1)))
    set2rep <- d2q(cbind(0, cbind(1, set2)))
    polytope1 <- redundant(set1rep, representation = "V")$output
    polytope2 <- redundant(set2rep, representation = "V")$output
    H_chset1 <- scdd(polytope1, representation = "V")$output
    H_chset2 <- scdd(polytope2, representation = "V")$output
    H_inter <- rbind(H_chset1, H_chset2)
    V_inter <- scdd(H_inter, representation = "H")$output
    vert_1n2 <- q2d(V_inter[, -c(1, 2)])
    coord_vert_inter <- rep(NA, ncol(set1))
    vol_inter <- 0
    if (is.matrix(vert_1n2)) 
      if (nrow(vert_1n2) > ncol(vert_1n2)) {
        coord_vert_inter <- vert_1n2
        vol_inter <- try(convhulln(vert_1n2, qhull.opt)$vol)
        if (inherits(vol_inter, "try-error")){
          save(set1, set2, file = sprintf("D:/test/betapart/Alexandria/%s.Rdata",
                                          round(runif(1)*1e6)))    
          vol_inter <- NA
        }
      }
    return(list(vol_inter = vol_inter, coord_vert_inter = coord_vert_inter))
  }
  comb2 <- combn(N, 2)
  vol_inter2_mat <- matrix(0, N, N, dimnames = list(row.names(x), 
                                                    row.names(x)))
  vol_inter2 <- numeric(ncol(comb2))
  coord_vert_inter2 <- vector(mode = "list", length = ncol(comb2))
  if (parallel) {
    combi <- function(x, y) {
      vol <- c(x$vol, y$vol)
      coord <- c(x$coord, y$coord)
      k <- c(x$k, y$k)
      return(list(vol = vol, coord = coord, k = k))
    }
    iter <- if (is.null(size)) 
      isplitIndices(ncol(comb2), chunks = nc)
    else isplitIndices(ncol(comb2), chunkSize = size)
    cl <- parallel::makeCluster(nc, type = type)
    doParallel::registerDoParallel(cl)
    if (type %in% c("SOCK", "PSOCK")) 
      parallel::clusterExport(cl, c("x", "traits", "comb2", 
                                    "inter", "qhull.opt"), envir = environment())
    interp <- foreach(i = iter, .packages = c("rcdd", "geometry"), 
                      .combine = combi, .inorder = LB) %dopar% {
                        seqs <- i
                        vol <- numeric(length(seqs))
                        coord <- vector(mode = "list", length = length(seqs))
                        u <- 1
                        for (k in seqs) {
                          i <- comb2[1, k]
                          j <- comb2[2, k]
                          seti <- traits[which(x[i, ] == 1), ]
                          setj <- traits[which(x[j, ] == 1), ]
                          interij <- inter(seti, setj, qhull.opt)
                          vol[u] <- interij$vol_inter
                          coord[[u]] <- interij$coord_vert_inter
                          u <- u + 1
                        }
                        res <- list(vol = vol, coord = coord, k = seqs)
                        res
                      }
    parallel::stopCluster(cl)
    ordo <- order(interp$k)
    vol_inter2 <- interp$vol
    vol_inter2 <- vol_inter2[ordo]
    coord_vert_inter2 <- interp$coord
    coord_vert_inter2 <- coord_vert_inter2[ordo]
    vol_inter2_mat[t(comb2[2:1, ])] <- vol_inter2
  }
  else {
    for (k in 1:ncol(comb2)) {
      i <- comb2[1, k]
      j <- comb2[2, k]
      seti <- traits[which(x[i, ] == 1), ]
      setj <- traits[which(x[j, ] == 1), ]
      interij <- inter(seti, setj, qhull.opt)
      vol_inter2_mat[j, i] <- interij$vol_inter
      vol_inter2[k] <- interij$vol_inter
      coord_vert_inter2[[k]] <- interij$coord_vert_inter
      if (fbc.step) {
        step.fbc["intersection_2", 1] <- paste(k, "/", 
                                               ncol(comb2), sep = "")
        write.table(step.fbc, file = "step.fbc.txt", 
                    row.names = T, col.names = F, sep = "\\t")
      }
    }
  }
  shared <- not.shared <- matrix(0, N, N, dimnames = list(row.names(x), 
                                                          row.names(x)))
  for (i in 1:(N - 1)) for (j in (i + 1):N) {
    shared[j, i] <- vol_inter2_mat[j, i]
    not.shared[i, j] <- FRi[i] - vol_inter2_mat[j, i]
    not.shared[j, i] <- FRi[j] - vol_inter2_mat[j, i]
  }
  sum.not.shared <- not.shared + t(not.shared)
  max.not.shared <- pmax(not.shared, t(not.shared))
  min.not.shared <- pmin(not.shared, t(not.shared))
  comb_inter <- list()
  comb_inter[[1]] <- combn(N, 2, paste, collapse = "_")
  coord_vert_inter <- list()
  coord_vert_inter[[1]] <- coord_vert_inter2
  vol_inter <- list()
  vol_inter[[1]] <- vol_inter2
  FRt <- NA
  a <- NA
  if (N > 2 & multi) {
    for (z in 3:N) {
      res <- f1(z, N, D)
      comb_inter[[z - 1]] <- res$comb_z
      coord_vert_inter[[z - 1]] <- res$coord_vert_inter_z
      vol_inter[[z - 1]] <- res$vol_inter_z
      if (fbc.step) {
        step.fbc[paste("intersection", z, sep = "_"), 
                 1] <- paste(ncol(res$comb_z), "/", ncol(res$comb_z), 
                             sep = "")
        write.table(step.fbc, file = "step.fbc.txt", 
                    row.names = T, col.names = F, sep = "\\t")
      }
    }
    sumvol_sign <- rep(NA, N - 1)
    for (k in 2:N) {
      sumvol_sign[k - 1] <- (-1)^(k - 1) * sum(vol_inter[[k - 
                                                            1]])
    }
    FRt <- sumFRi + sum(sumvol_sign)
    a <- sumFRi - FRt
  }
  details <- NA
  if (return.details) {
    names(coord_vert_i) <- names(FRi)
    CH <- list(FRi = FRi, coord_vertices = coord_vert_i)
    intersections <- list(combinations = comb_inter, volumes = vol_inter, 
                          coord_vertices = coord_vert_inter)
    details <- list(CH = CH, intersections = intersections)
  }
  functional.computations <- list(sumFRi = sumFRi, FRt = FRt, 
                                  a = a, shared = shared, not.shared = not.shared, sum.not.shared = sum.not.shared, 
                                  max.not.shared = max.not.shared, min.not.shared = min.not.shared, 
                                  details = details)
  class(functional.computations) <- "functional.betapart"
  return(functional.computations)
}


