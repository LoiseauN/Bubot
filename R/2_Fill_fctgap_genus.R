#A CHECKER Forcipiger Scaridae Naso


library(reshape2)

pkgs <- c('ade4','ggplot2','betapart','harrypotter','dplyr','cluster','ape','bbmle',
          'doParallel','missForest','cowplot','grid','gridExtra','grid','taxize',
          "ggalt","GGally","tidyverse")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#Fill gap ---
#####################
tab <- merge(species_video_scale,hab_pc_video_scale[,c(2,8:10)],by.x="row.names",by.y="Row.names")
rownames(tab) <- tab[,1]

dat_complet<-melt(tab, id.vars = c(1,320:322))
dat_complet <- dat_complet[dat_complet$value >0,]
dat_complet  <- data.frame(merge(dat_complet,fish_traits,by.x="variable",by.y="Species",all.x=T))

colnames(dat_complet)[c(1,6)] <- c("Species", "Abundance")


dat_complet$clean_diet <- NA
for (i in 1:nrow(dat_complet)){
  
  print(i)
  if(is.na(dat_complet$Diet[i])) { dat_complet$clean_diet[i] <- NA }
  else if(dat_complet$Diet[i]=="herbivorous-detritivorous")  { dat_complet$clean_diet[i] <- "HD" }
  else if(dat_complet$Diet[i]=="macroalgal-herbivorous")  { dat_complet$clean_diet[i] <- "HM" }
  else if(dat_complet$Diet[i]=="omnivorous")  { dat_complet$clean_diet[i] <- "OM" }
  else if(dat_complet$Diet[i]=="planktivorous")  { dat_complet$clean_diet[i] <- "PK" }
  else if(dat_complet$Diet[i]=="invertivorous-targeting-mobile-invertebrate")  { dat_complet$clean_diet[i] <- "IM" }
  else if(dat_complet$Diet[i]=="invertivorous-targeting-sessile-invertebrates")   { dat_complet$clean_diet[i] <- "IS"}
  else if(dat_complet$Diet[i]=="piscivorous")   { dat_complet$clean_diet[i] <- "PI" }
  
}

taxo_correct=unique(dat_complet[,c("Species","Genus","Familly")])

#for (i in 1:nrow(taxo_correct)){
# print(i)
  
#  if(!grepl("_",as.character(taxo_correct$Species[i]), fixed = TRUE)){
    
#    classif <- classification(as.character(taxo_correct$Species[i]),db= 'itis')

#    if(sum(classif[[1]]$rank=="genus")>0){  
      
#      taxo_correct$Genus[i] <- classif[[1]][classif[[1]]$rank=="genus",]$name
      
#      if(is.na(taxo_correct$Familly[i])){taxo_correct$Familly[i] <- classif[[1]][classif[[1]]$rank=="family",]$name}
      
#    }
    
#    if(sum(classif[[1]]$rank=="family")>0){  
      
#      taxo_correct$Familly[i] <- classif[[1]][classif[[1]]$rank=="family",]$name
      
#   }
#  }
#}



taxo_correct$Species <- addLevel(taxo_correct$Species, "Blenniidae")
taxo_correct$Familly <- addLevel(taxo_correct$Familly, "Blenniidae")
dat_complet$Species <- addLevel(dat_complet$Species, "Blenniidae")

dat_complet[dat_complet$Species=="Bleniidae",]$Species<- "Blenniidae"
taxo_correct[taxo_correct$Species=="Bleniidae",]$Species<- "Blenniidae"
taxo_correct[taxo_correct$Species=="Blenniidae",]$Familly<- "Blenniidae"

# If offline--- 

taxo_correct[taxo_correct$Species =="Acanthurus",]$Genus<- "Acanthurus"
taxo_correct[taxo_correct$Species =="Acanthurus",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$Species =="Cheilinus",]$Genus<- "Cheilinus"
taxo_correct[taxo_correct$Species =="Cheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Lutjanus",]$Genus<- "Lutjanus"
taxo_correct[taxo_correct$Species =="Lutjanus",]$Familly<- "Lutjanidae"

taxo_correct[taxo_correct$Species =="Lethrinus",]$Genus<- "Lethrinus"
taxo_correct[taxo_correct$Species =="Lethrinus",]$Familly<- "Lethrinidae"

taxo_correct[taxo_correct$Species =="Siganus",]$Genus<- "Siganus"
taxo_correct[taxo_correct$Species =="Siganus",]$Familly<- "Siganidae"

taxo_correct[taxo_correct$Species =="Macropharyngodon",]$Genus<- "Macropharyngodon"
taxo_correct[taxo_correct$Species =="Macropharyngodon",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Pomacentrus",]$Genus<- "Pomacentrus"

taxo_correct[taxo_correct$Species =="Pomacentrus",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$Species =="Ostracion",]$Genus<- "Ostracion"
taxo_correct[taxo_correct$Species =="Ostracion",]$Familly<- "Ostraciidae"

taxo_correct[taxo_correct$Species =="Pomacanthus",]$Genus<- "Pomacanthus"
taxo_correct[taxo_correct$Species =="Pomacanthus",]$Familly<- "Pomacanthidae"

taxo_correct[taxo_correct$Species =="Rhinecanthus",]$Genus<- "Rhinecanthus"
taxo_correct[taxo_correct$Species =="Rhinecanthus",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$Species =="Pseudochromis",]$Genus<- "Pseudochromis"
taxo_correct[taxo_correct$Species =="Pseudochromis",]$Familly<- "Pseudochromidae"

taxo_correct[taxo_correct$Species =="Sargocentron",]$Genus<- "Sargocentron"
taxo_correct[taxo_correct$Species =="Sargocentron",]$Familly<- "Holocentridae"

taxo_correct[taxo_correct$Species =="Coris",]$Genus<- "Coris"
taxo_correct[taxo_correct$Species =="Coris",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Aphareus",]$Genus<- "Aphareus"
taxo_correct[taxo_correct$Species =="Aphareus",]$Familly<- "Lutjanidae"

taxo_correct[taxo_correct$Species =="Halichoeres",]$Genus<- "Halichoeres"
taxo_correct[taxo_correct$Species =="Halichoeres",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Cheilodipterus",]$Genus<- "Cheilodipterus"
taxo_correct[taxo_correct$Species =="Cheilodipterus",]$Familly<- "Apogonidae"

taxo_correct[taxo_correct$Species =="Anampses",]$Genus<- "Anampses"
taxo_correct[taxo_correct$Species =="Anampses",]$Familly<- "Labridae"

taxo_correct$Genus <- addLevel(taxo_correct$Genus, "Apogon")
taxo_correct[taxo_correct$Species =="Apogon",]$Genus<- "Apogon"
taxo_correct[taxo_correct$Species =="Apogon",]$Familly<- "Apogonidae"

taxo_correct[taxo_correct$Species =="Cirrhilabrus",]$Genus<- "Cirrhilabrus"
taxo_correct[taxo_correct$Species =="Cirrhilabrus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Sphyraena",]$Genus<- "Sphyraena"
taxo_correct[taxo_correct$Species =="Sphyraena",]$Familly<- "Scombridae"

taxo_correct[taxo_correct$Species =="Muraenidae",]$Familly<- "Muraenidae"
taxo_correct[taxo_correct$Species =="Siganidae",]$Familly<- "Siganidae"
taxo_correct[taxo_correct$Species =="Diodontidae",]$Familly<- "Diodontidae"
taxo_correct[taxo_correct$Species =="Gobiidae",]$Familly<- "Gobiidae"
taxo_correct[taxo_correct$Species =="Scorpaenidae",]$Familly<- "Scorpaenidae"
taxo_correct[taxo_correct$Species =="Apogonidae",]$Familly<- "Apogonidae"
taxo_correct[taxo_correct$Species =="Congridae",]$Familly<- "Congridae"
taxo_correct[taxo_correct$Species =="Nemipteridae",]$Familly<- "Nemipteridae"
taxo_correct[taxo_correct$Species =="Acanthuridae",]$Familly<- "Acanthuridae"
taxo_correct[taxo_correct$Species =="Pomacanthidae",]$Familly<- "Pomacanthidae"
taxo_correct[taxo_correct$Species =="Pomacentridae",]$Familly<- "Pomacentridae"
taxo_correct[taxo_correct$Species =="Priacanthidae",]$Familly<- "Priacanthidae"
taxo_correct[taxo_correct$Species =="Pseudochromidae",]$Familly<- "Pseudochromidae"
taxo_correct[taxo_correct$Species =="Scombridae",]$Familly<- "Scombridae"
taxo_correct[taxo_correct$Species =="Serranidae",]$Familly<- "Serranidae"
taxo_correct[taxo_correct$Species =="Tetraodontidae",]$Familly<- "Tetraodontidae"
taxo_correct[taxo_correct$Species =="Chaetodontidae",]$Familly<- "Chaetodontidae"
taxo_correct[taxo_correct$Species =="Cirrhitidae",]$Familly<- "Cirrhitidae"
taxo_correct[taxo_correct$Species =="Aulostomidae",]$Familly<- "Aulostomidae"
taxo_correct[taxo_correct$Species =="Balistidae",]$Familly<- "Balistidae"
taxo_correct[taxo_correct$Species =="Belonidae",]$Familly<- "Belonidae"
taxo_correct[taxo_correct$Species =="Caesionidae",]$Familly<- "Caesionidae"
taxo_correct[taxo_correct$Species =="Carangidae",]$Familly<- "Carangidae"
taxo_correct[taxo_correct$Species =="Cirrhitidae",]$Familly<- "Cirrhitidae"
taxo_correct[taxo_correct$Species =="Holocentridae",]$Familly<- "Holocentridae"
taxo_correct[taxo_correct$Species =="Labridae",]$Familly<- "Labridae"
taxo_correct[taxo_correct$Species =="Lethrinidae",]$Familly<- "Lethrinidae"
taxo_correct[taxo_correct$Species =="Lutjanidae",]$Familly<- "Lutjanidae"
taxo_correct[taxo_correct$Species =="Microdesmidae",]$Familly<- "Microdesmidae"
taxo_correct[taxo_correct$Species =="Monacanthidae",]$Familly<- "Monacanthidae"
taxo_correct[taxo_correct$Species =="Mullidae",]$Familly<- "Mullidae"
taxo_correct[taxo_correct$Species =="Ostraciidae",]$Familly<- "Ostraciidae"

taxo_correct[taxo_correct$Species =="Chaetodon",]$Genus<- "Chaetodon"
taxo_correct[taxo_correct$Species =="Chaetodon",]$Familly<- "Chaetodontidae"

taxo_correct[taxo_correct$Species =="Gomphosus",]$Genus<- "Gomphosus"
taxo_correct[taxo_correct$Species =="Gomphosus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Ctenochaetus",]$Genus<- "Ctenochaetus"
taxo_correct[taxo_correct$Species =="Ctenochaetus",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$Species =="Dascyllus",]$Genus<- "Dascyllus"
taxo_correct[taxo_correct$Species =="Dascyllus",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$Species =="Epinephelus",]$Genus<- "Epinephelus"
taxo_correct[taxo_correct$Species =="Epinephelus",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$Species =="Amblyeleotris",]$Genus<- "Amblyeleotris"
taxo_correct[taxo_correct$Species =="Amblyeleotris",]$Familly<- "Gobiidae"

taxo_correct[taxo_correct$Species =="Amphiprion",]$Genus<- "Amphiprion"
taxo_correct[taxo_correct$Species =="Amphiprion",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$Species =="Arothron",]$Genus<- "Arothron"
taxo_correct[taxo_correct$Species =="Arothron",]$Familly<- "Tetraodontidae"

taxo_correct[taxo_correct$Species =="Aulostomus",]$Genus<- "Aulostomus"
taxo_correct[taxo_correct$Species =="Aulostomus",]$Familly<- "Aulostomidae"

taxo_correct[taxo_correct$Species =="Caesio",]$Genus<- "Caesio"
taxo_correct[taxo_correct$Species =="Caesio",]$Familly<- "Caesionidae"

taxo_correct[taxo_correct$Species =="Centropyge",]$Genus<- "Centropyge"
taxo_correct[taxo_correct$Species =="Centropyge",]$Familly<- "Pomacanthidae"

taxo_correct[taxo_correct$Species =="Cephalopholis",]$Genus<- "Cephalopholis"
taxo_correct[taxo_correct$Species =="Cephalopholis",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$Species =="Heniochus",]$Genus<- "Heniochus"
taxo_correct[taxo_correct$Species =="Heniochus",]$Familly<- "Chaetodontidae"

taxo_correct[taxo_correct$Species =="Labroides",]$Genus<- "Labroides"
taxo_correct[taxo_correct$Species =="Labroides",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Myripristis",]$Genus<- "Myripristis"
taxo_correct[taxo_correct$Species =="Myripristis",]$Familly<- "Holocentridae"

taxo_correct[taxo_correct$Species =="Nemateleotris",]$Genus<- "Nemateleotris"
taxo_correct[taxo_correct$Species =="Nemateleotris",]$Familly<- "Gobiidae"

taxo_correct[taxo_correct$Species =="Oxycheilinus",]$Genus<- "Oxycheilinus"
taxo_correct[taxo_correct$Species =="Oxycheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Parupeneus",]$Genus<- "Parupeneus"
taxo_correct[taxo_correct$Species =="Parupeneus",]$Familly<- "Mullidae"

taxo_correct[taxo_correct$Species =="Melichthys",]$Genus<- "Melichthys"
taxo_correct[taxo_correct$Species =="Melichthys",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$Species =="Pseudanthias",]$Genus<- "Pseudanthias"
taxo_correct[taxo_correct$Species =="Pseudanthias",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$Species =="Pseudocheilinus",]$Genus<- "Pseudocheilinus"
taxo_correct[taxo_correct$Species =="Pseudocheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Pseudocoris",]$Genus<- "Pseudocoris"
taxo_correct[taxo_correct$Species =="Pseudocoris",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Ptereleotris",]$Genus<- "Ptereleotris"
taxo_correct[taxo_correct$Species =="Ptereleotris",]$Familly<- "Gobiidae"

taxo_correct[taxo_correct$Species =="Scarus",]$Genus<- "Scarus"
taxo_correct[taxo_correct$Species =="Scarus",]$Familly<- "Scaridae"

taxo_correct[taxo_correct$Species =="Sufflamen",]$Genus<- "Sufflamen"
taxo_correct[taxo_correct$Species =="Sufflamen",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$Species =="Thalassoma",]$Genus<- "Thalassoma"
taxo_correct[taxo_correct$Species =="Thalassoma",]$Familly<- "Labridae"

taxo_correct[taxo_correct$Species =="Variola",]$Genus<- "Variola"
taxo_correct[taxo_correct$Species =="Variola",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$Species =="Xanthichthys",]$Genus<- "Xanthichthys"
taxo_correct[taxo_correct$Species =="Xanthichthys",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$Species =="Zebrasoma",]$Genus<- "Zebrasoma"
taxo_correct[taxo_correct$Species =="Zebrasoma",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$Species =="Chromis",]$Genus<- "Chromis"
taxo_correct[taxo_correct$Species =="Chromis",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$Species =="Forcipiger",]$Genus<- "Forcipiger"
taxo_correct[taxo_correct$Species =="Forcipiger",]$Familly<- "Chaetodontidae"

taxo_correct[taxo_correct$Species =="Scaridae",]$Familly<- "Scaridae"

taxo_correct[taxo_correct$Species =="Naso",]$Genus<- "Naso"
taxo_correct[taxo_correct$Species =="Naso",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$Species =="Caranx",]$Genus<- "Caranx"
taxo_correct[taxo_correct$Species =="Caranx",]$Familly<- "Carangidae"




dat_complet <- dat_complet[,colnames(dat_complet) %notin% c("Familly","Genus")]
dat_complet <- merge(dat_complet,taxo_correct, by="Species",all.x= T)

#Clean order and names
dat_complet <- data.frame(surveys = dat_complet$Row.names,
                            depth = dat_complet$depth,
                            classDepth   = dat_complet$classDepth,
                            island = dat_complet$island,
                            species = dat_complet$Species,
                            genus = dat_complet$Genus,
                            family = dat_complet$Familly,
                            class = dat_complet$Class,
                            abundance = dat_complet$Abundance,
                            reef_associated = dat_complet$Reef_associated,
                            mobility = dat_complet$Mobility,
                            activity = dat_complet$Activity,
                            schooling = dat_complet$Schooling,
                            position = dat_complet$Position,
                            diet = dat_complet$Diet,
                            clean_diet = dat_complet$clean_diet,
                            trophic.level = dat_complet$Trophic.level_Fishbase,
                            bodyShape_Fishbase = dat_complet$BodyShape_Fishbase,
                            maxLengthTL_Fishbase = dat_complet$MaxLengthTL_Fishbase,
                            a= dat_complet$a,
                            b= dat_complet$b
                            )


dat_complet$Infered <- 1
for (i in 1:nrow(dat_complet)){
  
  if(!is.na(dat_complet$clean_diet[i])){
    
    dat_complet$Infered[i] <- 0
}
}


for (i in 1:nrow(dat_complet)) {
  
  print(i)
  #i=19
  
  
  if(is.na(dat_complet$trophic.level[i])){

    #If scale of genus possible
    if(!is.na(dat_complet$genus[i])){
    
      #Trait 
       trait_selec <- dat_complet[dat_complet$genus==dat_complet$genus[i],]
       trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
       trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
       trait_selec<- trait_selec[trait_selec$Infered==0,]
       
       
          if(nrow(trait_selec)==0) {next}
       
      trait_selec <- unique(trait_selec[,c("species","reef_associated","mobility","activity","schooling","position","diet","clean_diet",
                                           "trophic.level","bodyShape_Fishbase","maxLengthTL_Fishbase","a","b")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
      
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$reef_associated)),"reef_associated"]  <- names(sort(table(trait_selec[,"reef_associated"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$mobility)),"mobility"]  <- names(sort(table(trait_selec[,"mobility"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$activity)),"activity"] <- names(sort(table(trait_selec[,"activity"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$schooling)),"schooling"] <- names(sort(table(trait_selec[,"schooling"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$position)),"position"] <- names(sort(table(trait_selec[,"position"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$diet)),"diet"] <- names(sort(table(trait_selec[,"diet"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$clean_diet)),"clean_diet"] <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$bodyShape_Fishbase)),"bodyShape_Fishbase"] <- names(sort(table(trait_selec[,"bodyShape_Fishbase"]),decreasing = T)[1])
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$maxLengthTL_Fishbase)),"maxLengthTL_Fishbase"] <- mean(trait_selec[,"maxLengthTL_Fishbase"],na.rm=T)
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$a)),"a"] <- mean(trait_selec[,"a"],na.rm=T)
      dat_complet[which(dat_complet$genus==dat_complet$genus[i] & is.na(dat_complet$b)),"b"] <- mean(trait_selec[,"b"],na.rm=T)
    }
      
    #If scale of genus impossible, family level
    else if(is.na(dat_complet$genus[i])){
        
      #Trait 
      trait_selec <- dat_complet[dat_complet$family==dat_complet$family[i],]
      trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
      trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
      trait_selec<- trait_selec[trait_selec$Infered==0,]
      
      
      if(nrow(trait_selec)==0) {next}
      
      trait_selec <- unique(trait_selec[,c("species","reef_associated","mobility","activity","schooling","position","diet","clean_diet",
                                           "trophic.level","bodyShape_Fishbase","maxLengthTL_Fishbase","a","b")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
      
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$reef_associated)),"reef_associated"]  <- names(sort(table(trait_selec[,"reef_associated"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$mobility)),"mobility"]  <- names(sort(table(trait_selec[,"mobility"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$activity)),"activity"] <- names(sort(table(trait_selec[,"activity"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$schooling)),"schooling"] <- names(sort(table(trait_selec[,"schooling"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$position)),"position"] <- names(sort(table(trait_selec[,"position"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$diet)),"diet"] <- names(sort(table(trait_selec[,"diet"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$clean_diet)),"clean_diet"] <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$bodyShape_Fishbase)),"bodyShape_Fishbase"] <- names(sort(table(trait_selec[,"bodyShape_Fishbase"]),decreasing = T)[1])
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$maxLengthTL_Fishbase)),"maxLengthTL_Fishbase"] <- mean(trait_selec[,"maxLengthTL_Fishbase"],na.rm=T)
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$trophic.level)),"trophic.level"] <- mean(trait_selec[,"trophic.level"],na.rm=T)
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$a)),"a"] <- mean(trait_selec[,"a"],na.rm=T)
      dat_complet[which(dat_complet$family==dat_complet$family[i] & is.na(dat_complet$b)),"b"] <- mean(trait_selec[,"b"],na.rm=T)   
      } 
    
      }
  }


#Check nature of traits
dat_complet$position <- factor(as.character(dat_complet$position), 
                            order=T)
dat_complet$schooling <- forcats::fct_rev(factor(as.character(dat_complet$schooling), 
                             order=T))
dat_complet$clean_diet <- factor(as.character(dat_complet$clean_diet), 
                        levels=c("HM","HD","OM","PK","IS","IM","FC"),order=T)

dat_complet$diet <- factor(as.character(dat_complet$diet), levels=
                             c("herbivorous-detritivorous",
                               "macroalgal-herbivorous",
                               "Omnivore",
                               "omnivorous",
                               "planktivorous",
                               "invertivorous-targeting-sessile-invertebrates",
                               "invertivorous-targeting-mobile-invertebrate",
                               "piscivorous"),order=T)

dat_complet$mobility<- forcats::fct_rev(factor(as.character(dat_complet$mobility), 
                              order=T))

#Remove Apogonidae / Elasmobranchii / Gobiidae
    #Remove apogon
    dat_complet <- subset(dat_complet, dat_complet$family!="Apogonidae") #5 obs, 7 inds
   
    #Remove shark
    dat_complet <- subset(dat_complet, dat_complet$class=="Actinopterygii" |  is.na(dat_complet$class)) #17 obs, 30 inds
    
    #Remove gobiidae
    dat_complet <- subset(dat_complet, dat_complet$family!="Gobiidae")#23 obs, 33 inds

    
save(dat_complet,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")
    

# Start Working on Mayotte.
dat_complet_mayotte<- dat_complet[dat_complet$island=="Mayotte",]
totalabun_classDepth <- dat_complet_mayotte[,c("value","classDepth")]
totalabun_classDepth <- aggregate(. ~ classDepth, data = totalabun_classDepth, sum)
colnames(totalabun_classDepth)[2]<-"totalabun_classDepth"

totalabun_video <- dat_complet_mayotte[,c("VideoID","value")]
totalabun_video <- aggregate(. ~ VideoID, data = totalabun_video, sum)
colnames(totalabun_video)[2]<-"totalabun_video"

dat_complet_mayotte<- merge(dat_complet_mayotte,totalabun_video,by="VideoID",all.x=T)
dat_complet_mayotte<- merge(dat_complet_mayotte,totalabun_classDepth,by="classDepth",all.x=T)

dat_complet_mayotte$aburelatif <- dat_complet_mayotte$value/dat_complet_mayotte$totalabun_video


'%!in%' <- function(x,y)!('%in%'(x,y))
var <- c("Activity","Schooling","Position","clean_diet","Size","Diet")

for(j in 1:length(var)){
  
main.plot <- ggplot(dat_complet_mayotte, aes(x=depth, y=log10(value),color=dat_complet_mayotte[,var[j]]))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
  #scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_smooth(method = "loess")+
  theme_bw()+ 
  labs(colour = var[j]) + 
  theme(legend.position = "right")+
  ylim(0,4)+
  labs(x="Depth",y="Abundance (log)")+
  geom_vline(xintercept=20,  linetype="dotted",size=1)+
  geom_vline(xintercept=40,  linetype="dotted",size=1)+
  geom_vline(xintercept=60,  linetype="dotted",size=1)+
  geom_vline(xintercept=80,  linetype="dotted",size=1)
  
  #+
  #geom_label(label="0-20m", x=8, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="20-40m",x=30, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="40-60m", x=52, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="60-80m", x=74, y=4, size=3, hjust=0,color="black")+
  #geom_label(label=">80m", x=10, y=4, size=3, hjust=0,color="black")

    abun_classDepth <- dat_complet_mayotte[,c("value","classDepth",var[j])]
    colnames(abun_classDepth)[3] <- "trait"
    abun_classDepth <- aggregate(. ~ classDepth + trait, data = abun_classDepth, sum)
   
    ## pyramid charts are two barcharts with axes flipped
    abun_classDepth<- with(abun_classDepth, abun_classDepth[order(trait,classDepth),])
    #("Herbivore-Detritivore","Omnivore","Planktivore","Invertivore","Piscivore"))
    
for (i in 1:length(unique(abun_classDepth$classDepth))){
  print(i)
  sub <- subset(abun_classDepth , abun_classDepth$classDepth==unique(abun_classDepth$classDepth)[i])
  sub$perc <- round((sub$value/sum(sub$value))*100)
  sub$alpha <- sub$perc/2
  
  #Add modality that are absent with 0
  if(length(sub$trait %in% unique(abun_classDepth$trait)) < length(unique(abun_classDepth$trait))){
    notin <- abun_classDepth$trait
    notin <- data.frame(unique(notin[notin %!in% sub$trait]))
    notin <- data.frame(classDepth=rep(unique(abun_classDepth$classDepth)[i],nrow(notin)),
               trait=notin[,1],
               value=rep(0,nrow(notin)),
               perc=rep(0,nrow(notin)),
               alpha=rep(0,nrow(notin)))
    sub<-rbind(sub,notin)
  }
  
    
  sub2 <- rbind(sub,sub) 
  sub2$alpha[c((nrow(sub)+1):nrow(sub2))] <- -sub2$alpha[c((nrow(sub)+1):nrow(sub2))] 
  sub2$direction <-c(rep("pos",nrow(sub)),rep("neg",nrow(sub)))
  
  
  assign(paste0("inset.plot",i), ggplot(sub2, aes(x = trait, y = alpha, fill = trait)) + 
    geom_bar(data = subset(sub2, direction == "pos"), stat = "identity", width=1) + 
    geom_bar(data = subset(sub2, direction == "neg"), stat = "identity", width=1) + 
    scale_fill_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
    coord_flip()+
    theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    #plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
    plot.title = element_text(size = 14, hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.title=element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    legend.title=element_text(size=15),
    legend.text=element_text(size=15),
    legend.background = element_rect(fill = "transparent")))
}
  #main.plot<- 
    if (j==1 || j==5 || j==6){ 
      assign(paste0("main.plot",j), ggdraw() +
               draw_plot(main.plot) +
               draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot2, x = 0.20, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot3, x = 0.35, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot4, x = 0.52, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot5, x = 0.72, y = 0.81, width = 0.12, height = 0.12))
      }
    
    if (j==2){ 
      assign(paste0("main.plot",j), ggdraw() +
               draw_plot(main.plot) +
               draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot2, x = 0.19, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot3, x = 0.35, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot4, x = 0.51, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot5, x = 0.7, y = 0.81, width = 0.12, height = 0.12)
      )}
    
    if (j==3){ 
      assign(paste0("main.plot",j), ggdraw() +
               draw_plot(main.plot) +
               draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot2, x = 0.195, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot3, x = 0.35, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot4, x = 0.51, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot5, x = 0.71, y = 0.81, width = 0.12, height = 0.12)
      )}
    
    if (j==4){ 
      assign(paste0("main.plot",j), ggdraw() +
               draw_plot(main.plot) +
               draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot2, x = 0.17, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot3, x = 0.32, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot4, x = 0.46, y = 0.81, width = 0.12, height = 0.12)+
               draw_plot(inset.plot5, x = 0.65, y = 0.81, width = 0.12, height = 0.12)
      )}

}



ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Activity.png", 
       plot = main.plot1, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Schooling.png", 
       plot = main.plot2, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Position.png", 
       plot = main.plot3, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/clean_diet.png", 
       plot = main.plot4, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Size.png", 
       plot = main.plot5, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Diet.png", 
       plot = main.plot6, 
       width = 297, 
       height = 210, 
       units = "mm")


#Functional space
# PROBLEM SUR DES PC A 0,0, 0
# cluster_core = 1 === singleton
plotPCOA <- dat_complet_mayotte[,c("FE","value","classDepth","PC1","PC2")]
plotPCOA <- aggregate(. ~ classDepth + FE, data = plotPCOA, sum)

plotPCOA_1 <- merge(aggregate(value ~ FE + classDepth, plotPCOA, sum), 
                 aggregate(PC1 ~ FE , plotPCOA, mean),by="FE")

plotPCOA <- merge(plotPCOA_1, 
                    aggregate(PC2 ~ FE , plotPCOA, mean),by="FE")

FunctSpace<- ggplot(plotPCOA, aes(x=PC1, y=PC2)) + 
  geom_point(aes(size=log10(value+1),colour= classDepth,alpha=0.4))+ #
  scale_shape_manual(values=c(4, 16))+
  geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                alpha = 0.7, show.legend = FALSE)+
  theme_bw()+labs(x = "PCOA 1")+labs(y = "PCOA 2") +
  facet_wrap(~ classDepth,ncol = 6)  +
  scale_colour_hp_d(option = "LunaLovegood",direction = 1)+
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none",
        #axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks = element_blank()
        )
  
plotPCOA_2 <- dat_complet_mayotte[,c("PC1","PC2")]
colnames(plotPCOA_2)<-c("axis1","axis2")
 FunctSpace + geom_point(data = plotPCOA_2, aes(x = axis1, y = axis2),size=1,color="grey",alpha=0.2) +
   ggpubr::stat_chull(data = plotPCOA_2,aes(x = axis1, y = axis2), 
              alpha = 0.1, geom = "polygon")
 
 
