library(betapart)

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

plot(deth.dist, dissim.mayotte_turnover$beta.jac, ylim=c(0,1), xlim=c(0, max(deth.dist)))


ggplot(data = dist.decay.mat, 
       aes(x = dist.depth, y = log(beta.value + 1), color = components)) +
  geom_point() +
  geom_smooth(se = T, method = "gam", formula = y ~ s(log(x)))+
  facet_wrap(~island)



mayotte.decay.exp<-decay.model(dissim.mayotte_turnover$beta.jtu, deth.dist, y.type="dissimilarities", model.type="exponential", perm=100)
mayotte.decay.<-powdecay.model(dissim.mayotte_turnover$beta.jac, deth.dist, y.type="dissimilarities", model.type="power", perm=100)


plot.decay(mayotte.decay.exp, col=rgb(0,0,0,0.5))

mod1 = glm(y ~ x, family = gaussian(link = "log"), 
           start = coef(lm(y ~ x)))


y <- as.vector(dissim.mayotte_turnover$beta.jac)
x <- as.vector(deth.dist)

mayotte.decay.pow<-decay.model(dissim.BCI, spat.dist, y.type="dissim", model.type="power", perm=100)

plot.decay(BCI.decay.exp, col=rgb(0,0,0,0.5))
plot.decay(BCI.decay.exp, col="red", remove.dots=TRUE, add=TRUE)
plot.decay(BCI.decay.pow, col="blue", remove.dots=TRUE, add=TRUE)



#--- Triangle plot