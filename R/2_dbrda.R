# libraries
pkgs <- c('ade4','ggplot2','vegan','harrypotter','dplyr','ggalt')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))




#--- At the video scale 
species <- species.site.matrix$species.matrix

hab_pc <- habit.score

rownames(hab_pc) <- rownames(species)

hab_pc <- merge(hab_pc,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth")]),by.x="row.names",by.y="Sample.name") 

hab_pc$classDepth <- NA

for (i in 1: nrow(hab_pc)){
  if(hab_pc$depth[i]<20){ hab_pc$classDepth[i] <- "[0-20["}
  if(hab_pc$depth[i]>=20 & hab_pc$depth[i]<40){ hab_pc$classDepth[i] <- "[20-40["}
  if(hab_pc$depth[i]>=40 & hab_pc$depth[i]<60){ hab_pc$classDepth[i] <- "[40-60["}
  if(hab_pc$depth[i]>=60 & hab_pc$depth[i]<80){ hab_pc$classDepth[i] <- "[60-80["}
  if(hab_pc$depth[i]>=80){ hab_pc$classDepth[i] <- ">80"}
  
}

hab_pc <- merge(hab_pc,sites,by.x="Sample.code",by.y="Sample_code")


### dbrda

dbrda.tot.pc <- capscale(species ~ classDepth + PC1 + PC2 + PC3 + PC4, data = hab_pc, distance = "jaccard")

# partial dbrda

dbrda.depth.pc <- capscale(species ~ classDepth + Condition(PC1 + PC2 + PC3 + PC4), data = hab_pc, distance = "bray")



# check results

summary(dbrda.tot.pc)

aov <- anova(dbrda.tot.pc, permutations = 100 )#9999

RsquareAdj(dbrda.tot.pc)

aov.axe <- anova(dbrda.tot.pc, by = "axis", permutations = 100)#9999

aov.merg <- anova(dbrda.tot.pc, by = "margin", permutations = 100)#9999



#### plotting

## getting the scores for plotting

dbrda <- dbrda.depth.pc

scores_dbrda <- scores(dbrda)  # getting the scores from the analysis

site_scores <- scores_dbrda$sites     # separating out the site scores

species_scores <- scores_dbrda$species   # separating out the species

species_scores <- data.frame(species_scores)

# percentage variability explained by axes

sumdbrda <- summary(dbrda)

CAP1 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP1"]*100, 1)

CAP2 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP2"]*100, 1)

R2 <- RsquareAdj(dbrda)$adj.r.squared

# combine in one df

site_scores_environment <- cbind(site_scores,hab_pc) %>%   # merge
  
  droplevels()

# set correct factor level order

site_scores_environment$classDepth <- factor(site_scores_environment$classDepth, levels=c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80"))

site_scores_environment$island <- factor(site_scores_environment$island, levels=c("Europa","Juan_de_nova","Mayotte"))



# visualize in ggplot

formula <- as.character(dbrda$call$formula)[3]

pal <- hp(n = 5, house = "ronweasley2")

ggplot(site_scores_environment, aes(x= CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  
  geom_point(data = species_scores, aes(x= CAP1,y = CAP2), cex = 0.5, col = "grey40") +  # species scores
  
  geom_point(aes(pch = classDepth, fill = classDepth), cex = 4) + # site scores
  
  scale_fill_hp(discrete = TRUE, option = "HarryPotter", name = "classDepth",
                labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  #scale_fill_manual(values = pal,      # purplescale
                    
  #name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  scale_shape_manual(values = c(23:19),
                     
                     name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  labs(x = paste0("CAP1 (", CAP1, "%)"), y = paste0("CAP2 (", CAP2, "%)"),
       
       title = paste0("species ~ ", formula)) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "black"),
        
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        
        panel.border = element_blank(),panel.background = element_blank())






#--- At the site scale 
species <- species.site.matrix$species.matrix


hab_pc <- habit.score
rownames(hab_pc) <- rownames(species)
hab_pc <- merge(hab_pc,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth")]),by.x="row.names",by.y="Sample.name") 
hab_pc <-hab_pc[,-1]
hab_pc <- aggregate(. ~ Sample.code, data = hab_pc, mean)

hab_pc$classDepth <- NA

for (i in 1: nrow(hab_pc)){
  if(hab_pc$depth[i]<20){ hab_pc$classDepth[i] <- "[0-20["}
  if(hab_pc$depth[i]>=20 & hab_pc$depth[i]<40){ hab_pc$classDepth[i] <- "[20-40["}
  if(hab_pc$depth[i]>=40 & hab_pc$depth[i]<60){ hab_pc$classDepth[i] <- "[40-60["}
  if(hab_pc$depth[i]>=60 & hab_pc$depth[i]<80){ hab_pc$classDepth[i] <- "[60-80["}
  if(hab_pc$depth[i]>=80){ hab_pc$classDepth[i] <- ">80"}
  
}

hab_pc <- merge(hab_pc,sites,by.x="Sample.code",by.y="Sample_code")
rownames(hab_pc) <- hab_pc[,1]
hab_pc <- hab_pc[,-1]

species <- merge(species,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code")]),by.x="row.names",by.y="Sample.name") 
species <- species[,-1]
species <- aggregate(. ~ Sample.code, data = species, sum)
rownames(species) <- species[,1]
species <- species[,-1]

species[species>1]<- 1

### dbrda

dbrda.tot.pc <- capscale(species ~ classDepth + PC1 + PC2 + PC3 + PC4, data = hab_pc, distance = "jaccard")

# partial dbrda

dbrda.depth.pc <- capscale(species ~ classDepth + Condition(PC1 + PC2 + PC3 + PC4 + island), data = hab_pc, distance = "jaccard")



# check results

summary(dbrda.tot.pc)

aov <- anova(dbrda.tot.pc, permutations = 100 )#9999

RsquareAdj(dbrda.tot.pc)

aov.axe <- anova(dbrda.tot.pc, by = "axis", permutations = 100)#9999

aov.merg <- anova(dbrda.tot.pc, by = "margin", permutations = 100)#9999



#### plotting

## getting the scores for plotting

dbrda <- dbrda.depth.pc

scores_dbrda <- scores(dbrda)  # getting the scores from the analysis

site_scores <- scores_dbrda$sites     # separating out the site scores

species_scores <- scores_dbrda$species   # separating out the species

species_scores <- data.frame(species_scores)

# percentage variability explained by axes

sumdbrda <- summary(dbrda)

CAP1 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP1"]*100, 1)

CAP2 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP2"]*100, 1)

R2 <- RsquareAdj(dbrda)$adj.r.squared

# combine in one df

site_scores_environment <- cbind(site_scores,hab_pc) %>%   # merge
  
  droplevels()

# set correct factor level order

site_scores_environment$classDepth <- factor(site_scores_environment$classDepth, levels=c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80"))

site_scores_environment$island <- factor(site_scores_environment$island, levels=c("Europa","Juan_de_nova","Mayotte"))



# visualize in ggplot

formula <- as.character(dbrda$call$formula)[3]

ggplot(site_scores_environment, aes(x= CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  
  geom_point(data = species_scores, aes(x= CAP1,y = CAP2), cex = 0.5, col = "grey40") +  # species scores
  
  geom_point(aes(pch = classDepth, fill = classDepth), cex = 4) + # site scores
  
  geom_encircle(aes(group = classDepth, linetype =classDepth,fill=classDepth), s_shape = 1, expand = 0,size=1,
                alpha = 0.2, show.legend = FALSE) +

  scale_fill_hp(discrete = TRUE, option = "HarryPotter", name = "classDepth",
                labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
 
  scale_shape_manual(values = c(25:21),
                     
                     name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  labs(x = paste0("CAP1 (", CAP1, "%)"), y = paste0("CAP2 (", CAP2, "%)"),
       
       title = paste0("species ~ ", formula)) +

  theme_bw() +
  
  theme(axis.line = element_line(colour = "black"),
        
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        
        panel.border = element_blank(),panel.background = element_blank())







