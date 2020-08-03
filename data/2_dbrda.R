# libraries

library(vegan)

library(dplyr)

library(ggplot2)



# plotting parameters

# purplescale

colres.purple   <-  "#3f007d"

colout5.purple  <-  "#6a51a3"

colout10.purple <-  "#9e9ac8"



# data

#species <- community_df (here: presence/absence)

#environment <- constraining_variables_df

species <- species.site.matrix$species.matrix

hab_pc <- habit.score
rownames(hab_pc) <- rownames(species)

hab_pc <- merge(hab_pc,data.frame(species.site.matrix$site.data[,c("Sample.name","depth")]),by.x="row.names",by.y="Sample.name") 

hab_pc 
hab_pc$classDepth <- NA

for (i in 1: nrow(hab_pc)){
  if(hab_pc$depth[i]<20){ hab_pc$classDepth[i] <- "[0-20["}
  if(hab_pc$depth[i]>=20 & hab_pc$depth[i]<40){ hab_pc$classDepth[i] <- "[20-40["}
  if(hab_pc$depth[i]>=40 & hab_pc$depth[i]<60){ hab_pc$classDepth[i] <- "[40-60["}
  if(hab_pc$depth[i]>=60 & hab_pc$depth[i]<80){ hab_pc$classDepth[i] <- "[60-80["}
  if(hab_pc$depth[i]>=80){ hab_pc$classDepth[i] <- ">80"}
}

### dbrda

dbrda.tot.pc <- capscale(species ~ classDepth + PC1 + PC2 + PC3 + PC4, data = hab_pc, distance = "jaccard")

# partial dbrda

dbrda.depth.pc <- capscale(species ~ classDepth + Condition(PC1 + PC2 + PC3 + PC4), data = hab_pc, distance = "jaccard")



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

CAP2 <- round(sumdbrda$cont$importance["Proportion Explained", "MDS1"]*100, 1)

R2 <- RsquareAdj(dbrda)$adj.r.squared

# combine in one df

site_scores_environment <- cbind(site_scores,hab_pc) %>%   # merge
  
  droplevels()

# set correct factor level order

site_scores_environment$protection_tot <- factor(site_scores_environment$protection_tot, levels=c("reserve", "outside5", "outside10"))

site_scores_environment$region_tot <- factor(site_scores_environment$region_tot, levels=c("Banyuls19", "Carry-le-Rouet", "Calanques", "Porquerolles", "Cap-Roux","Calvi"))



# visualize in ggplot

formula <- as.character(dbrda$call$formula)[3]



ggplot(site_scores_environment, aes(x= CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  
  geom_point(data = species_scores, aes(x= CAP1,y = CAP2), cex = 0.5, col = "grey40") +  # species scores
  
  geom_point(aes(pch = protection_tot, fill = protection_tot), cex = 4) + # site scores
  
  scale_fill_manual(values = c(colres.purple, colout5.purple, colout10.purple),      # purplescale
                    
                    name = "Protection", labels = c("reserve", "5km outside", "10km outside")) +
  
  scale_shape_manual(values = c(23:21),
                     
                     name = "Protection", labels = c("reserve", "5km outside", "10km outside")) +
  
  labs(x = paste0("CAP1 (", CAP1, "%)"), y = paste0("CAP2 (", CAP2, "%)"),
       
       title = paste0("species ~ ", formula)) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "black"),
        
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        
        panel.border = element_blank(),panel.background = element_blank())

