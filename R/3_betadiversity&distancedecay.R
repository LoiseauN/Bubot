library(betapart)

#--- All region



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
   
    
dist.decaY.mat <- data.frame(beta.value = c(dist2list(mayotte_beta$beta.jac,tri = T)[,3],
                                            dist2list(mayotte_beta$beta.jtu,tri = T)[,3],
                                            dist2list(mayotte_beta$beta.jne,tri = T)[,3],
                                            dist2list(Juan_de_nova_beta$beta.jac,tri = T)[,3],
                                            dist2list(Juan_de_nova_beta$beta.jtu,tri = T)[,3],
                                            dist2list(Juan_de_nova_beta$beta.jne,tri = T)[,3],
                                            dist2list(Europa_beta$beta.jac,tri = T)[,3],
                                            dist2list(Europa_beta$beta.jtu,tri = T)[,3],
                                            dist2list(Europa_beta$beta.jne,tri = T)[,3]),
                             
                             components = c(rep("Total",length(mayotte_beta$beta.jac)),
                                            rep("Turnover",length(mayotte_beta$beta.jac)),
                                            rep("Nestedness",length(mayotte_beta$beta.jac)),
                                            rep("Total",length(Juan_de_nova_beta$beta.jac)),
                                            rep("Turnover",length(Juan_de_nova_beta$beta.jac)),
                                            rep("Nestedness",length(Juan_de_nova_beta$beta.jac)),
                                            rep("Total",length(Europa_beta$beta.jac)),
                                            rep("Turnover",length(Europa_beta$beta.jac)),
                                            rep("Nestedness",length(Europa_beta$beta.jac))),
                             
                             island   = c(rep("Mayotte",length(mayotte_beta$beta.jac)*3),
                                            rep("Juan_de_nova_beta",length(Juan_de_nova_beta$beta.jac)*3),
                                            rep("Europa",length(Juan_de_nova_beta$beta.jac)*3)),
                             
                             dist.depth = c(rep(dist2list(deth.dist.mayotte,tri = T)[,3],3),
                                            rep(dist2list(deth.dist.Juan_de_nova,tri = T)[,3],3),
                                            rep(dist2list(deth.dist.Europa,tri = T)[,3],3)))
                             
    y <- as.vector(dissim.mayotte_turnover$beta.jac)
    x <- as.vector(deth.dist)
    

plot(deth.dist, dissim.mayotte_turnover$beta.jac, ylim=c(0,1), xlim=c(0, max(deth.dist)))



plt <- ggplot(dissim.mayotte_turnover, aes(deth.dist)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", , se = F, 
              method.args = list(family = "poisson"))

print(plt)



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