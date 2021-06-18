sizeBUBOT <- read.table("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/extraction_tailles.txt",sep="\t",header=T)

fish_traits2 <- dat_complet[,c("species","genus","family", "reef_associated","mobility","activity","schooling","position","diet",
                           "clean_diet", "trophic.level","bodyShape_Fishbase","maxLengthTL_Fishbase","a","b","Infered")]
fish_traits2 <- unique(fish_traits2)

temporal_data <- merge(sizeBUBOT,fish_traits2,by.x="Species",by.y="species",all.x = T)

for (i in 1:nrow(temporal_data)){
  print(i)
  if(is.na(temporal_data$family[i])){
    
    if(nrow(fish_traits[fish_traits$Species %in% temporal_data$Species[i],])==0){next}
    
    temporal_data$family[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Familly 
    temporal_data$genus[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Genus 
    temporal_data$reef_associated[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Reef_associated
    temporal_data$mobility[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Mobility
    temporal_data$activity[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Activity
    temporal_data$schooling[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Schooling
    temporal_data$position[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Position
    temporal_data$diet[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Diet
    temporal_data$trophic.level[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$Trophic.level_Fishbase
    temporal_data$bodyShape_Fishbase[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$BodyShape_Fishbase
    temporal_data$maxLengthTL_Fishbase[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$MaxLengthTL_Fishbase
    temporal_data$a[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$a
    temporal_data$b[i] <- fish_traits[fish_traits$Species %in% temporal_data$Species[i],]$b
    temporal_data$Infered[i] <- 0
  }
  else{}
}

colnames(hab_pc_site_scale)[1:5] <- c("Hab_PC1","Hab_PC2","Hab_PC3","Hab_PC4","Hab_PC5")
temporal_data <-  merge(temporal_data,hab_pc_site_scale,by.x="sampling_code",by.y="row.names",all.x = T) 
temporal_data <- temporal_data[,-34]
colnames(temporal_data)[8] <- "depth"

for(i in 1: nrow(temporal_data)){
  print(i)
  if(temporal_data$Species[i]=="Acanthurus_nigrofuscus.or.Ctenochaetus_striatus.or.Ctenochaetus_binotatus"){
    temporal_data$family[i] <- "Acanthuridae"
    }
    
    if(temporal_data$Species[i]=="Plagiotremus"){
      temporal_data$genus[i] <- "Plagiotremus"
      temporal_data$family[i] <- "Blenniidae"}
    
  if(temporal_data$Species[i]=="Stethojulis"){
    temporal_data$genus[i] <- "Stethojulis"
    temporal_data$family[i] <- "Labridae"}
  
  if(temporal_data$Species[i]=="Canthigaster"){
    temporal_data$genus[i] <- "Canthigaster"
    temporal_data$family[i] <- "Canthigasterinae"}
}



temporal_data$Infered <- 1
for (i in 1:nrow(temporal_data)){
  
  if(!is.na(temporal_data$clean_diet[i])){
    
    temporal_data$Infered[i] <- 0
  }
}


for (i in 1:nrow(temporal_data)) {
  
  print(i)
  #i=19
  
  
  if(is.na(temporal_data$trophic.level[i])){
    
    #If scale of genus possible
    if(!is.na(temporal_data$genus[i])){
      
      #Trait 
      trait_selec <- temporal_data[temporal_data$genus==temporal_data$genus[i],]
      trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
      trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
      trait_selec<- trait_selec[trait_selec$Infered==0,]
      
      
      if(nrow(trait_selec)==0) {next}
      
      trait_selec <- unique(trait_selec[,c("Species","reef_associated","mobility","activity","schooling","position","diet","clean_diet",
                                           "trophic.level","bodyShape_Fishbase","maxLengthTL_Fishbase","a","b")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
      
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$reef_associated)),"reef_associated"]  <- names(sort(table(trait_selec[,"reef_associated"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$mobility)),"mobility"]  <- names(sort(table(trait_selec[,"mobility"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$activity)),"activity"] <- names(sort(table(trait_selec[,"activity"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$schooling)),"schooling"] <- names(sort(table(trait_selec[,"schooling"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$position)),"position"] <- names(sort(table(trait_selec[,"position"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$diet)),"diet"] <- names(sort(table(trait_selec[,"diet"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$clean_diet)),"clean_diet"] <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$bodyShape_Fishbase)),"bodyShape_Fishbase"] <- names(sort(table(trait_selec[,"bodyShape_Fishbase"]),decreasing = T)[1])
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$maxLengthTL_Fishbase)),"maxLengthTL_Fishbase"] <- mean(trait_selec[,"maxLengthTL_Fishbase"],na.rm=T)
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$a)),"a"] <- mean(trait_selec[,"a"],na.rm=T)
      temporal_data[which(temporal_data$genus==temporal_data$genus[i] & is.na(temporal_data$b)),"b"] <- mean(trait_selec[,"b"],na.rm=T)
    }
    
    #If scale of genus impossible, family level
    else if(is.na(temporal_data$genus[i])){
      
      #Trait 
      trait_selec <- temporal_data[temporal_data$family==temporal_data$family[i],]
      trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
      trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
      trait_selec<- trait_selec[trait_selec$Infered==0,]
      
      
      if(nrow(trait_selec)==0) {next}
      
      trait_selec <- unique(trait_selec[,c("Species","reef_associated","mobility","activity","schooling","position","diet","clean_diet",
                                           "trophic.level","bodyShape_Fishbase","maxLengthTL_Fishbase","a","b")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
      
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$reef_associated)),"reef_associated"]  <- names(sort(table(trait_selec[,"reef_associated"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$mobility)),"mobility"]  <- names(sort(table(trait_selec[,"mobility"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$activity)),"activity"] <- names(sort(table(trait_selec[,"activity"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$schooling)),"schooling"] <- names(sort(table(trait_selec[,"schooling"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$position)),"position"] <- names(sort(table(trait_selec[,"position"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$diet)),"diet"] <- names(sort(table(trait_selec[,"diet"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$clean_diet)),"clean_diet"] <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$bodyShape_Fishbase)),"bodyShape_Fishbase"] <- names(sort(table(trait_selec[,"bodyShape_Fishbase"]),decreasing = T)[1])
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$maxLengthTL_Fishbase)),"maxLengthTL_Fishbase"] <- mean(trait_selec[,"maxLengthTL_Fishbase"],na.rm=T)
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$a)),"a"] <- mean(trait_selec[,"a"],na.rm=T)
      temporal_data[which(temporal_data$family==temporal_data$family[i] & is.na(temporal_data$b)),"b"] <- mean(trait_selec[,"b"],na.rm=T)   
    } 
    
  }
}


#Check nature of traits
temporal_data$position <- factor(as.character(temporal_data$position), 
                               order=T)
temporal_data$schooling <- forcats::fct_rev(factor(as.character(temporal_data$schooling), 
                                                 order=T))
temporal_data$clean_diet <- factor(as.character(temporal_data$clean_diet), 
                                 levels=c("HM","HD","OM","PK","IS","IM","PI"),order=T)

temporal_data$diet <- factor(as.character(temporal_data$diet), levels=
                             c("herbivorous-detritivorous",
                               "macroalgal-herbivorous",
                               "Omnivore",
                               "omnivorous",
                               "planktivorous",
                               "invertivorous-targeting-sessile-invertebrates",
                               "invertivorous-targeting-mobile-invertebrate",
                               "piscivorous"),order=T)

temporal_data$mobility<- forcats::fct_rev(factor(as.character(temporal_data$mobility), 
                                               order=T))
temporal_data$activity <- factor(temporal_data$activity)   







temporal_data <- 

unique(temporal_data[is.na(temporal_data$family),]$Species)

  
save(temporal_data,file="temporal_data.RData")

