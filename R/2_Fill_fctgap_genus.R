
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


library(reshape2)
dat_complet<-melt(tab, id.vars = c(1,320:322))
dat_complet <- dat_complet[dat_complet$value >0,]
dat_complet  <- data.frame(merge(dat_complet,fish_traits,by.x="variable",by.y="Species",all.x=T))

dat_complet$clean_diet <- NA
for (i in 1:nrow(dat_complet)){
  
  print(i)
  if(is.na(dat_complet$Diet[i])) { dat_complet$clean_diet[i] <- NA }
  else if(dat_complet$Diet[i]=="HD")  { dat_complet$clean_diet[i] <- "Herbivore-Detritivore" }
  else if(dat_complet$Diet[i]=="PK")  { dat_complet$clean_diet[i] <- "Planktivore" }
  else if(dat_complet$Diet[i]=="FC")  { dat_complet$clean_diet[i] <- "Piscivore" }
  else if(dat_complet$Diet[i]=="IM")  { dat_complet$clean_diet[i] <- "Invertivore" }
  else if(dat_complet$Diet[i]=="IS")  { dat_complet$clean_diet[i] <- "Invertivore" }
  else if(dat_complet$Diet[i]=="OM")   { dat_complet$clean_diet[i] <- "Omnivore" }
  else { dat_complet$clean_diet[i] <- "Herbivore-Detritivore" }
  
}

taxo_correct=unique(dat_complet[,c("variable","Genus","Familly")])

for (i in 1:nrow(taxo_correct)){
  print(i)
  
  if(!grepl("_",as.character(taxo_correct$variable[i]), fixed = TRUE)){
    
    classif <- classification(as.character(taxo_correct$variable[i]),db= 'itis')

    if(sum(classif[[1]]$rank=="genus")>0){  
      
      taxo_correct$Genus[i] <- classif[[1]][classif[[1]]$rank=="genus",]$name
      
      if(is.na(taxo_correct$Familly[i])){taxo_correct$Familly[i] <- classif[[1]][classif[[1]]$rank=="family",]$name}
      
    }
    
    if(sum(classif[[1]]$rank=="family")>0){  
      
      taxo_correct$Familly[i] <- classif[[1]][classif[[1]]$rank=="family",]$name
      
    }
  }
}

addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

taxo_correct$variable <- addLevel(taxo_correct$variable, "Blenniidae")
taxo_correct$Familly <- addLevel(taxo_correct$Familly, "Blenniidae")


taxo_correct[taxo_correct$variable=="Bleniidae",]$variable<- "Blenniidae"
taxo_correct[taxo_correct$variable=="Blenniidae",]$Familly<- "Blenniidae"

#Finish hand:
taxo_correct[taxo_correct$variable =="Acanthurus",]$Genus<- "Acanthurus"
taxo_correct[taxo_correct$variable =="Acanthurus",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$variable =="Cheilinus",]$Genus<- "Cheilinus"
taxo_correct[taxo_correct$variable =="Cheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Lutjanus",]$Genus<- "Lutjanus"
taxo_correct[taxo_correct$variable =="Lutjanus",]$Familly<- "Lutjanidae"

taxo_correct[taxo_correct$variable =="Lethrinus",]$Genus<- "Lethrinus"
taxo_correct[taxo_correct$variable =="Lethrinus",]$Familly<- "Lethrinidae"

taxo_correct[taxo_correct$variable =="Siganus",]$Genus<- "Siganus"
taxo_correct[taxo_correct$variable =="Siganus",]$Familly<- "Siganidae"

taxo_correct[taxo_correct$variable =="Macropharyngodon",]$Genus<- "Macropharyngodon"
taxo_correct[taxo_correct$variable =="Macropharyngodon",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Pomacentrus",]$Genus<- "Pomacentrus"

taxo_correct[taxo_correct$variable =="Pomacentrus",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$variable =="Ostracion",]$Genus<- "Ostracion"
taxo_correct[taxo_correct$variable =="Ostracion",]$Familly<- "Ostraciidae"

taxo_correct[taxo_correct$variable =="Pomacanthus",]$Genus<- "Pomacanthus"
taxo_correct[taxo_correct$variable =="Pomacanthus",]$Familly<- "Pomacanthidae"

taxo_correct[taxo_correct$variable =="Rhinecanthus",]$Genus<- "Rhinecanthus"
taxo_correct[taxo_correct$variable =="Rhinecanthus",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$variable =="Pseudochromis",]$Genus<- "Pseudochromis"
taxo_correct[taxo_correct$variable =="Pseudochromis",]$Familly<- "Pseudochromidae"

taxo_correct[taxo_correct$variable =="Sargocentron",]$Genus<- "Sargocentron"
taxo_correct[taxo_correct$variable =="Sargocentron",]$Familly<- "Holocentridae"

taxo_correct[taxo_correct$variable =="Coris",]$Genus<- "Coris"
taxo_correct[taxo_correct$variable =="Coris",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Aphareus",]$Genus<- "Aphareus"
taxo_correct[taxo_correct$variable =="Aphareus",]$Familly<- "Lutjanidae"

taxo_correct[taxo_correct$variable =="Halichoeres",]$Genus<- "Halichoeres"
taxo_correct[taxo_correct$variable =="Halichoeres",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Cheilodipterus",]$Genus<- "Cheilodipterus"
taxo_correct[taxo_correct$variable =="Cheilodipterus",]$Familly<- "Apogonidae"

taxo_correct[taxo_correct$variable =="Anampses",]$Genus<- "Anampses"
taxo_correct[taxo_correct$variable =="Anampses",]$Familly<- "Labridae"

taxo_correct$Genus <- addLevel(taxo_correct$Genus, "Apogon")
taxo_correct[taxo_correct$variable =="Apogon",]$Genus<- "Apogon"
taxo_correct[taxo_correct$variable =="Apogon",]$Familly<- "Apogonidae"

taxo_correct[taxo_correct$variable =="Cirrhilabrus",]$Genus<- "Cirrhilabrus"
taxo_correct[taxo_correct$variable =="Cirrhilabrus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Sphyraena",]$Genus<- "Sphyraena"
taxo_correct[taxo_correct$variable =="Sphyraena",]$Familly<- "Scombridae"

taxo_correct[taxo_correct$variable =="Muraenidae",]$Familly<- "Muraenidae"
taxo_correct[taxo_correct$variable =="Siganidae",]$Familly<- "Siganidae"
taxo_correct[taxo_correct$variable =="Diodontidae",]$Familly<- "Diodontidae"
taxo_correct[taxo_correct$variable =="Gobiidae",]$Familly<- "Gobiidae"
taxo_correct[taxo_correct$variable =="Scorpaenidae",]$Familly<- "Scorpaenidae"
taxo_correct[taxo_correct$variable =="Apogonidae",]$Familly<- "Apogonidae"
taxo_correct[taxo_correct$variable =="Congridae",]$Familly<- "Congridae"
taxo_correct[taxo_correct$variable =="Nemipteridae",]$Familly<- "Nemipteridae"
taxo_correct[taxo_correct$variable =="Acanthuridae",]$Familly<- "Acanthuridae"
taxo_correct[taxo_correct$variable =="Pomacanthidae",]$Familly<- "Pomacanthidae"
taxo_correct[taxo_correct$variable =="Pomacentridae",]$Familly<- "Pomacentridae"
taxo_correct[taxo_correct$variable =="Priacanthidae",]$Familly<- "Priacanthidae"
taxo_correct[taxo_correct$variable =="Pseudochromidae",]$Familly<- "Pseudochromidae"
taxo_correct[taxo_correct$variable =="Scombridae",]$Familly<- "Scombridae"
taxo_correct[taxo_correct$variable =="Serranidae",]$Familly<- "Serranidae"
taxo_correct[taxo_correct$variable =="Tetraodontidae",]$Familly<- "Tetraodontidae"
taxo_correct[taxo_correct$variable =="Chaetodontidae",]$Familly<- "Chaetodontidae"
taxo_correct[taxo_correct$variable =="Cirrhitidae",]$Familly<- "Cirrhitidae"
taxo_correct[taxo_correct$variable =="Aulostomidae",]$Familly<- "Aulostomidae"
taxo_correct[taxo_correct$variable =="Balistidae",]$Familly<- "Balistidae"
taxo_correct[taxo_correct$variable =="Belonidae",]$Familly<- "Belonidae"
taxo_correct[taxo_correct$variable =="Bleniidae",]$Familly<- "Bleniidae"
taxo_correct[taxo_correct$variable =="Caesionidae",]$Familly<- "Caesionidae"
taxo_correct[taxo_correct$variable =="Carangidae",]$Familly<- "Carangidae"
taxo_correct[taxo_correct$variable =="Cirrhitidae",]$Familly<- "Cirrhitidae"
taxo_correct[taxo_correct$variable =="Holocentridae",]$Familly<- "Holocentridae"
taxo_correct[taxo_correct$variable =="Labridae",]$Familly<- "Labridae"
taxo_correct[taxo_correct$variable =="Lethrinidae",]$Familly<- "Lethrinidae"
taxo_correct[taxo_correct$variable =="Lutjanidae",]$Familly<- "Lutjanidae"
taxo_correct[taxo_correct$variable =="Microdesmidae",]$Familly<- "Microdesmidae"
taxo_correct[taxo_correct$variable =="Monacanthidae",]$Familly<- "Monacanthidae"
taxo_correct[taxo_correct$variable =="Mullidae",]$Familly<- "Mullidae"
taxo_correct[taxo_correct$variable =="Ostraciidae",]$Familly<- "Ostraciidae"

taxo_correct[taxo_correct$variable =="Gomphosus",]$Genus<- "Gomphosus"
taxo_correct[taxo_correct$variable =="Gomphosus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Ctenochaetus",]$Genus<- "Ctenochaetus"
taxo_correct[taxo_correct$variable =="Ctenochaetus",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$variable =="Dascyllus",]$Genus<- "Dascyllus"
taxo_correct[taxo_correct$variable =="Dascyllus",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$variable =="Epinephelus",]$Genus<- "Epinephelus"
taxo_correct[taxo_correct$variable =="Epinephelus",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$variable =="Amblyeleotris",]$Genus<- "Amblyeleotris"
taxo_correct[taxo_correct$variable =="Amblyeleotris",]$Familly<- "Gobiidae"

taxo_correct[taxo_correct$variable =="Amphiprion",]$Genus<- "Amphiprion"
taxo_correct[taxo_correct$variable =="Amphiprion",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$variable =="Arothron",]$Genus<- "Arothron"
taxo_correct[taxo_correct$variable =="Arothron",]$Familly<- "Tetraodontidae"

taxo_correct[taxo_correct$variable =="Aulostomus",]$Genus<- "Aulostomus"
taxo_correct[taxo_correct$variable =="Aulostomus",]$Familly<- "Aulostomidae"

taxo_correct[taxo_correct$variable =="Caesio",]$Genus<- "Caesio"
taxo_correct[taxo_correct$variable =="Caesio",]$Familly<- "Caesionidae"

taxo_correct[taxo_correct$variable =="Centropyge",]$Genus<- "Centropyge"
taxo_correct[taxo_correct$variable =="Centropyge",]$Familly<- "Pomacanthidae"

taxo_correct[taxo_correct$variable =="Cephalopholis",]$Genus<- "Cephalopholis"
taxo_correct[taxo_correct$variable =="Cephalopholis",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$variable =="Heniochus",]$Genus<- "Heniochus"
taxo_correct[taxo_correct$variable =="Heniochus",]$Familly<- "Chaetodontidae"

taxo_correct[taxo_correct$variable =="Labroides",]$Genus<- "Labroides"
taxo_correct[taxo_correct$variable =="Labroides",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Myripristis",]$Genus<- "Myripristis"
taxo_correct[taxo_correct$variable =="Myripristis",]$Familly<- "Holocentridae"

taxo_correct[taxo_correct$variable =="Nemateleotris",]$Genus<- "Nemateleotris"
taxo_correct[taxo_correct$variable =="Nemateleotris",]$Familly<- "Gobiidae"

taxo_correct[taxo_correct$variable =="Oxycheilinus",]$Genus<- "Oxycheilinus"
taxo_correct[taxo_correct$variable =="Oxycheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Parupeneus",]$Genus<- "Parupeneus"
taxo_correct[taxo_correct$variable =="Parupeneus",]$Familly<- "Mullidae"

taxo_correct[taxo_correct$variable =="Melichthys",]$Genus<- "Melichthys"
taxo_correct[taxo_correct$variable =="Melichthys",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$variable =="Pseudanthias",]$Genus<- "Pseudanthias"
taxo_correct[taxo_correct$variable =="Pseudanthias",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$variable =="Pseudocheilinus",]$Genus<- "Pseudocheilinus"
taxo_correct[taxo_correct$variable =="Pseudocheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Pseudocoris",]$Genus<- "Pseudocoris"
taxo_correct[taxo_correct$variable =="Pseudocoris",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Ptereleotris",]$Genus<- "Ptereleotris"
taxo_correct[taxo_correct$variable =="Ptereleotris",]$Familly<- "Gobiidae"

taxo_correct[taxo_correct$variable =="Scarus",]$Genus<- "Scarus"
taxo_correct[taxo_correct$variable =="Scarus",]$Familly<- "Scaridae"

taxo_correct[taxo_correct$variable =="Sufflamen",]$Genus<- "Sufflamen"
taxo_correct[taxo_correct$variable =="Sufflamen",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$variable =="Thalassoma",]$Genus<- "Thalassoma"
taxo_correct[taxo_correct$variable =="Thalassoma",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable =="Variola",]$Genus<- "Variola"
taxo_correct[taxo_correct$variable =="Variola",]$Familly<- "Serranidae"

taxo_correct[taxo_correct$variable =="Xanthichthys",]$Genus<- "Xanthichthys"
taxo_correct[taxo_correct$variable =="Xanthichthys",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$variable =="Zebrasoma",]$Genus<- "Zebrasoma"
taxo_correct[taxo_correct$variable =="Zebrasoma",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$variable =="Chromis",]$Genus<- "Chromis"
taxo_correct[taxo_correct$variable =="Chromis",]$Familly<- "Pomacentridae"


dat_complet <- dat_complet[,-c(7,8)]
dat_complet <- merge(dat_complet,taxo_correct, by="variable",all.x= T)
colnames(dat_complet)[c(2,28:29)] <- c("VideoID","Genus","Family")

#save(dat_complet,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data/Data_dump/dat_complet.RData")

# Test with random forest, values are false
#cov=dat_complet[,c("variable","Genus","Familly","Size","Mobility","Activity","Schooling","Position",
#                   "MaxLengthTL","Troph","clean_diet")]
#cov= unique(cov)
#rownames(cov) <- cov[,1]
#cov <- cov[,-1]


#registerDoParallel(cores = 4)
#cov.test <- cov[!is.na(cov$Genus),]
#cov.test[cov.test == "<NA>"] = "NA"  
#cov.test$clean_diet <-as.factor(as.character(cov.test$clean_diet))
#cov.imp = missForest(cov.test,parallelize = "forests", ntree = 100)
#cov.imp <- cov.imp$ximp

# Based on PCOA 

#


for (i in 1:nrow(dat_complet)) {
  
  print(i)
  
  if(is.na(dat_complet$clean_diet[i])){
    
    #If scale of genus possible
    if(!is.na(dat_complet$Genus[i])){
    
      #Trait 
      #ICI ATTENTION IL FAUT REMPLIR TOUT LES AUTRES AVEC LE MM
       trait_selec <- dat_complet[dat_complet$Genus==dat_complet$Genus[i],]
       trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
       trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
              if(nrow(trait_selec)==0) {next}
       
      trait_selec <- unique(trait_selec[,c("variable","Mobility","Activity","Schooling","Position","clean_diet",'Size',
                                  "MaxLengthTL","Troph","Diet")])#,"PC1","PC2","PC3","PC4")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
          
      #dat_complet[i,"Mobility"]    <- names(sort(table(trait_selec[,"Mobility"]),decreasing = T)[1])
      #dat_complet[i,"Activity"]    <- names(sort(table(trait_selec[,"Activity"]),decreasing = T)[1])
      #dat_complet[i,"Schooling"]   <- names(sort(table(trait_selec[,"Schooling"]),decreasing = T)[1])
      #dat_complet[i,"Position"]    <- names(sort(table(trait_selec[,"Position"]),decreasing = T)[1])
      #dat_complet[i,"clean_diet"]  <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      #dat_complet[i,"Size"]        <- names(sort(table(trait_selec[,"Size"]),decreasing = T)[1])
      #dat_complet[i,"Diet"]        <- names(sort(table(trait_selec[,"Diet"]),decreasing = T)[1])
      #dat_complet[i,"MaxLengthTL"] <- mean(trait_selec$MaxLengthTL,na.rm=T)
      #dat_complet[i,"Troph"]       <- mean(trait_selec$Troph,na.rm=T)
             
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Mobility"]  <- names(sort(table(trait_selec[,"Mobility"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Activity"] <- names(sort(table(trait_selec[,"Activity"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Schooling"] <- names(sort(table(trait_selec[,"Schooling"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Position"] <- names(sort(table(trait_selec[,"Position"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"clean_diet"] <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Size"] <- names(sort(table(trait_selec[,"Size"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Diet"] <- names(sort(table(trait_selec[,"Diet"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"MaxLengthTL"] <- mean(trait_selec$MaxLengthTL,na.rm=T)
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Troph"] <- mean(trait_selec$Troph,na.rm=T)
            
            
      #PC
      #bary <- na.omit(dat_complet[dat_complet$Genus=="Thalassoma",]) 
      
      #bary <- trait_selec
      #bary <- unique(bary[,c("PC1","PC2","PC3","PC4")])
      #dat_complet[i,c("PC1","PC2","PC3","PC4")] <- apply(bary[,c(1:4)],2,mean,na.rm=T)  
      #dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$PC1),c("PC1","PC2","PC3","PC4")] <- apply(bary[,c(1:4)],2,mean,na.rm=T)  
      }
    
      
      #If scale of genus impossible, family level
    if(is.na(dat_complet$Genus[i])){
      
      #Trait
      #Scaridae
      trait_selec <- dat_complet[dat_complet$Family==dat_complet$Family[i],]
      trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
      trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
  
      if(nrow(trait_selec)==0) {next}
      trait_selec <- unique(trait_selec[,c("variable","Mobility","Activity","Schooling","Position","clean_diet",'Size',
                                           "MaxLengthTL","Troph","Diet","PC1","PC2","PC3","PC4")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
      
      #dat_complet[i,"Mobility"]    <- names(sort(table(trait_selec[,"Mobility"]),decreasing = T)[1])
      #dat_complet[i,"Activity"]    <- names(sort(table(trait_selec[,"Activity"]),decreasing = T)[1])
      #dat_complet[i,"Schooling"]   <- names(sort(table(trait_selec[,"Schooling"]),decreasing = T)[1])
      #dat_complet[i,"Position"]    <- names(sort(table(trait_selec[,"Position"]),decreasing = T)[1])
      #dat_complet[i,"clean_diet"]  <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      #dat_complet[i,"Size"]        <- names(sort(table(trait_selec[,"Size"]),decreasing = T)[1])
      #dat_complet[i,"Diet"]        <- names(sort(table(trait_selec[,"Diet"]),decreasing = T)[1])
      #dat_complet[i,"MaxLengthTL"] <- mean(trait_selec$MaxLengthTL,na.rm=T)
      #dat_complet[i,"Troph"]       <- mean(trait_selec$Troph,na.rm=T)
      
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Mobility"]   <- names(sort(table(trait_selec[,"Mobility"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Activity"]   <- names(sort(table(trait_selec[,"Activity"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Schooling"]  <- names(sort(table(trait_selec[,"Schooling"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Position"]   <- names(sort(table(trait_selec[,"Position"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"clean_diet"] <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Size"]       <- names(sort(table(trait_selec[,"Size"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Diet"]       <- names(sort(table(trait_selec[,"Diet"]),decreasing = T)[1])
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"MaxLengthTL"]<- mean(trait_selec$MaxLengthTL,na.rm=T)
      dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$Troph),"Troph"]      <- mean(trait_selec$Troph,na.rm=T)
      
      
      #PC
      #bary <- trait_selec
      #bary <- unique(bary[,c("PC1","PC2","PC3","PC4")])
      #dat_complet[i,c("PC1","PC2","PC3","PC4")] <- apply(bary[,c(1:4)],2,mean,na.rm=T)  
      #dat_complet[dat_complet$Genus==dat_complet$Genus[i] | is.na(dat_complet$clean_diet),c("PC1","PC2","PC3","PC4")] <- apply(bary[,c(1:4)],2,mean,na.rm=T)  
      
      }
  }
  
}



dat_complet$FE <- paste0(dat_complet$Size,dat_complet$Mobility,dat_complet$Activity,dat_complet$Schooling,
                         dat_complet$Position,dat_complet$Diet) 

dat_complet[dat_complet$Size==1,]

dat_complet$Position <- factor(as.character(dat_complet$Position), 
                            order=T)
dat_complet$Schooling <- factor(as.character(dat_complet$Schooling), 
                             order=T)
dat_complet$Diet <- factor(as.character(dat_complet$Diet), 
                        levels=c("HM","HD","OM","PK","IS","IM","FC"),order=T)

dat_complet$clean_diet <- factor(as.character(dat_complet$clean_diet), levels=c("Herbivore-Detritivore","Omnivore","Planktivore","Invertivore","Piscivore"))

# Based on PCOA 
cov=dat_complet[,c("variable","Mobility","Activity","Schooling","Position",'Size',
                   "Diet")] #maxLength ,"clean_diet"
cov= unique(cov)
rownames(cov) <- cov[,1]
cov <- cov[,-1]
cov.pcoa <- na.omit(cov)


# computing PCoA ----
trait.dist <- daisy(cov.pcoa,metric ="gower")
pco.traits <- ape::pcoa(trait.dist)

# Work with 4 dimensions
sp_pc_coord <- pco.traits$vectors[, 1:4]
colnames(sp_pc_coord) <- paste("PC", 1:4, sep = "")
dat_complet <- merge(dat_complet,sp_pc_coord,by.x="variable",by.y="row.names",all.x=T)
#save(dat_complet,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data/Data_dump/dat_complet.RData")
#save(dat_complet,file="~/Documents/Bubot/Bubot_Analyse/data/Data_dump/dat_complet.RData")


load(file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data/Data_dump/dat_complet.RData")















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

plotPCOA <- plotPCOA[plotPCOA$PC1>-100,]

FunctSpace<- ggplot(plotPCOA, aes(x=PC1, y=PC2)) + 
  geom_point(aes(size=log10(value+1),colour= classDepth))+ #
  scale_shape_manual(values=c(4, 16))+
  scale_alpha_manual(values=c(0.3, 0.8))+
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
  

 FunctSpace + geom_point(data = plotPCOA2, aes(x = PC1, y = PC2),size=1,color="grey",alpha=0.2) +
   stat_chull(data = plotPCOA2,aes(x = PC1, y = PC2), 
              alpha = 0.1, geom = "polygon")
 
 
