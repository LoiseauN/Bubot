# collect the habitat data
habitat=species.site.matrix$site.data[,c(11:18)]
row.names(habitat)=species.site.matrix$site.data$Sample.name
habitat[,1] <- as.factor(as.character(habitat[,1]))
habitat[,4] <- as.factor(as.character(habitat[,4]))
habitat[,5] <- as.factor(as.character(habitat[,5]))

habitat <- na.omit(habitat)
# give the row name


res.famd <- FAMD(habitat, graph = FALSE)
a<- fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

b<- fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

c<-fviz_contrib(res.famd, "var", axes = 1)

d<- fviz_famd_ind(res.famd, 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)




habit.score <- res.famd$ind$coord
colnames(habit.score) <- c("PC1","PC2","PC3","PC4","PC5")
famd_plot <- grid.arrange(a,
             b,
             c,d,nrow=2)


ggsave(filename=here::here("fig/famd_plot.png"), 
       plot = famd_plot, 
       width = 8, 
       height = 8, 
       units = "in",
       dpi=300)


##########################################################################################
#--- At the site scale ---

species_site_scale <- species.site.matrix$species.matrix
species_site_scale <- species_site_scale[,which(colnames(species_site_scale)!="unknown_fish")]

hab_pc_site_scale <- habit.score

hab_pc_site_scale<- merge(hab_pc_site_scale,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth","temperature")]),by.x="row.names",by.y="Sample.name",all.y=T) 
hab_pc_site_scale<-hab_pc_site_scale[,-1]
hab_pc_site_scale<- aggregate(. ~ Sample.code, data = hab_pc_site_scale, mean,na.action = NULL)
#
hab_pc_site_scale$classDepth <- NA

for (i in 1: nrow(hab_pc_site_scale)){
  if(hab_pc_site_scale$depth[i]<20){ hab_pc_site_scale$classDepth[i] <- "[0-20["}
  if(hab_pc_site_scale$depth[i]>=20 & hab_pc_site_scale$depth[i]<40){ hab_pc_site_scale$classDepth[i] <- "[20-40["}
  if(hab_pc_site_scale$depth[i]>=40 & hab_pc_site_scale$depth[i]<60){ hab_pc_site_scale$classDepth[i] <- "[40-60["}
  if(hab_pc_site_scale$depth[i]>=60) {hab_pc_site_scale$classDepth[i]  <- ">60"}
  
}

hab_pc_site_scale<- merge(hab_pc_site_scale,sites,by.x="Sample.code",by.y="Sample_code")
rownames(hab_pc_site_scale) <- hab_pc_site_scale[,1]
hab_pc_site_scale<- hab_pc_site_scale[,-1]


for(i in 8:10){hab_pc_site_scale[,i] <-as.factor(hab_pc_site_scale[,i]) } 

species_site_scale <- species.site.matrix$species.matrix
species_site_scale <- merge(species_site_scale,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code")]),by.x="row.names",by.y="Sample.name",all.y=T) 
species_site_scale <- species_site_scale[,-1]
species_site_scale <- aggregate(. ~ Sample.code, data = species_site_scale, sum)
rownames(species_site_scale) <- species_site_scale[,1]
species_site_scale <- species_site_scale[,-1]
species_site_scale <- species_site_scale[,which(colnames(species_site_scale)!="unknown_fish")]
species_site_scale <- species_site_scale[apply(species_site_scale,1,sum)>0,]
hab_pc_site_scale <- hab_pc_site_scale[rownames(hab_pc_site_scale) %in% rownames(species_site_scale) ,]
species_site_scale <- species_site_scale[rownames(species_site_scale) %in% rownames(hab_pc_site_scale) ,]



##########################################################################################
#--- At the video scale ---
species_video_scale <- species.site.matrix$species.matrix
species_video_scale <- species_video_scale[,which(colnames(species_video_scale)!="unknown_fish")]

hab_pc_video_scale <- habit.score

species_video_scale <- species_video_scale[apply(species_video_scale,1,sum)>0,]
hab_pc_video_scale <- hab_pc_video_scale[rownames(hab_pc_video_scale) %in% rownames(species_video_scale),]

hab_pc_video_scale <- merge(hab_pc_video_scale,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth","temperature")]),by.x="row.names",by.y="Sample.name",all.y=T) 



hab_pc_video_scale$classDepth <- NA

for (i in 1: nrow(hab_pc_video_scale)){
  if(hab_pc_video_scale$depth[i]<20){ hab_pc_video_scale$classDepth[i] <- "[0-20["}
  if(hab_pc_video_scale$depth[i]>=20 & hab_pc_video_scale$depth[i]<40){ hab_pc_video_scale$classDepth[i] <- "[20-40["}
  if(hab_pc_video_scale$depth[i]>=40 & hab_pc_video_scale$depth[i]<60){ hab_pc_video_scale$classDepth[i] <- "[40-60["}
  if(hab_pc_video_scale$depth[i]>=60) {hab_pc_video_scale$classDepth[i]  <- ">60"}
  
}

hab_pc_video_scale <- merge(hab_pc_video_scale,sites,by.x="Sample.code",by.y="Sample_code")
for(i in 10:12){hab_pc_video_scale[,i] <-as.factor(hab_pc_video_scale[,i]) } 

### dbrda

dbrda.tot.pc <- capscale(species_video_scale ~ classDepth + PC1 + PC2 + PC3 + PC4, data = hab_pc_video_scale, distance = "jaccard")

# partial dbrda

dbrda.depth.pc <- capscale(species_video_scale ~ classDepth + Condition(PC1 + PC2 + PC3 + PC4), data = hab_pc_video_scale, distance = "bray")



# check results

summary(dbrda.tot.pc)

#aov <- anova(dbrda.tot.pc, permutations = 100 )#9999

RsquareAdj(dbrda.tot.pc)

#aov.axe <- anova(dbrda.tot.pc, by = "axis", permutations = 100)#9999

#aov.merg <- anova(dbrda.tot.pc, by = "margin", permutations = 100)#9999



#### plotting

## getting the scores for plotting

dbrda <- dbrda.depth.pc

scores_dbrda <- scores(dbrda)  # getting the scores from the analysis

site_scores <- scores_dbrda$sites     # separating out the site scores

species_video_scale_scores <- scores_dbrda$species_video_scale   # separating out the species_video_scale

species_video_scale_scores <- data.frame(species_video_scale_scores)

# percentage variability explained by axes

sumdbrda <- summary(dbrda)

CAP1 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP1"]*100, 1)

CAP2 <- round(sumdbrda$cont$importance["Proportion Explained", "CAP2"]*100, 1)

R2 <- RsquareAdj(dbrda)$adj.r.squared

# combine in one df

site_scores_environment <- cbind(site_scores,hab_pc_video_scale) %>%   # merge
  
  droplevels()

# set correct factor level order

site_scores_environment$classDepth <- factor(site_scores_environment$classDepth, levels=c("[0-20[","[20-40[","[40-60[",">60"))

site_scores_environment$island <- factor(site_scores_environment$island, levels=c("Europa","Juan_de_nova","Mayotte"))



# visualize in ggplot

formula <- as.character(dbrda$call$formula)[3]

pal <- hp(n = 5, house = "ronweasley2")

ggplot(site_scores_environment, aes(x= CAP1, y = CAP2)) +
  
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  
  geom_point(data = species_video_scale_scores, aes(x= CAP1,y = CAP2), cex = 0.5, col = "grey40") +  # species_video_scale scores
  
  geom_point(aes(pch = classDepth, fill = classDepth), cex = 4) + # site scores
  
  scale_fill_hp(discrete = TRUE, option = "HarryPotter", name = "classDepth",
                labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  #scale_fill_manual(values = pal,      # purplescale
  
  #name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  scale_shape_manual(values = c(23:19),
                     
                     name = "classDepth", labels = c("[0-20[","[20-40[", "[40-60[", "[60-80[",">80")) +
  
  labs(x = paste0("CAP1 (", CAP1, "%)"), y = paste0("CAP2 (", CAP2, "%)"),
       
       title = paste0("species_video_scale ~ ", formula)) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "black"),
        
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        
        panel.border = element_blank(),panel.background = element_blank())

