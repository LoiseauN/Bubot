library(reshape2)
library(stringr)




sizeBUBOT <- read.table("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/extraction_tailles.txt",sep="\t",header=T)
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/sizeRLS.RData")
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/coefRLS.RData")
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")

colnames(dat_complet)[c(1,6)] <- c("Species", "Abundance")

dat_complet <- dat_complet[,colnames(dat_complet) %notin% c("TempPrefMin","TempPrefMean","TempPrefMax",
                                                            "BodyShapeI","FoodI","FoodII","FoodIII","swimtype","FE")]

dat_complet[is.na(dat_complet$Activity),]
#same format species
sizeRLS$species <-  gsub(" ", "_", sizeRLS$species )
coefRLS$species <-  gsub(" ", "_", coefRLS$species )
#Add info genus for size


sizeBUBOT$genus <-  str_split_fixed(sizeBUBOT$Species, "_", 2)[,1]
sizeRLS$genus   <-  str_split_fixed(sizeRLS$species, "_", 2)[,1]


dat_complet[is.na(dat_complet$Size),]




species_site_scale2 <- data.frame(id=rownames(species_site_scale),species_site_scale)
species_site_scale_longmat <- melt(species_site_scale2)
species_site_scale_longmat <- species_site_scale_longmat[species_site_scale_longmat$value!=0,]
colnames(species_site_scale_longmat) <- c("surveys","species","abundance")

#spelling mistake
species_site_scale_longmat$species <- addLevel(species_site_scale_longmat$species, "Blenniidae")
species_site_scale_longmat[species_site_scale_longmat$species=="Bleniidae",]$species <- "Blenniidae"




#Add info Family  and genus for surveys
species_site_scale_longmat$genus <- NA
species_site_scale_longmat$family <- NA

for (i in 1:nrow(species_site_scale_longmat)){
  
  print(i)
  
  if(species_site_scale_longmat$species[i] %in%  Species_info$Genus_species){
     
    species_site_scale_longmat$genus[i] <- Species_info[Species_info$Genus_species %in% species_site_scale_longmat$species[i],]$Genus
     
     species_site_scale_longmat$family[i] <- Species_info[Species_info$Genus_species %in% species_site_scale_longmat$species[i],]$Familly
    
  }
  
  else if(species_site_scale_longmat$species[i] %in%  Species_info$Genus){
    
    species_site_scale_longmat$genus[i] <- Species_info[Species_info$Genus %in% species_site_scale_longmat$species[i],]$Genus
    
    species_site_scale_longmat$family[i] <- Species_info[Species_info$Genus %in% species_site_scale_longmat$species[i],]$Familly
    
  }
  
  else if(species_site_scale_longmat$species[i] %in%  Species_info$Familly){
   
    species_site_scale_longmat$genus[i] <- NA
    
    species_site_scale_longmat$family[i] <- Species_info[Species_info$Familly %in% species_site_scale_longmat$species[i],]$Familly
    
  }
  
  else {
    
    species_site_scale_longmat$genus[i] <- NA
    
    species_site_scale_longmat$family[i] <- NA
  
    }

}


#Add family info for size
sizeBUBOT$family <- NA

for (i in 1:nrow(sizeBUBOT)){
  
     if(sizeBUBOT$Species[i] %in%  Species_info$Genus_species){
    
       sizeBUBOT$family[i] <- Species_info[Species_info$Genus_species %in% sizeBUBOT$Species[i],]$Familly
       
     }
     
     else if(sizeBUBOT$Species[i] %in%  Species_info$Genus){

       sizeBUBOT$family[i] <- Species_info[Species_info$Genus %in% sizeBUBOT$Species[i],]$Familly
       
     }
     
    else if(sizeBUBOT$Species[i] %in%  Species_info$Familly){
   
       sizeBUBOT$family[i] <- Species_info[Species_info$Familly %in% sizeBUBOT$Species[i],]$Familly
       
     }
     
     else {
       
       sizeBUBOT$family[i] <- NA
       
     }
     
}

#Remove some level family in genus
for (i in 1:nrow(sizeBUBOT)){
  print(i)
  
if(sizeBUBOT$genus[i] %in% sizeBUBOT$family[i]) sizeBUBOT$genus[i] <- NA
   
}

#Add coef info
sizeRLS$a <- NA
sizeRLS$b <- NA
for (i in 383858:nrow(sizeRLS)){
  print(i)
  
  if(sizeRLS$species[i] %in% coefRLS$species) {
    sizeRLS$a[i] <- coefRLS[coefRLS$species %in% sizeRLS$species[i],]$a
    sizeRLS$b[i] <- coefRLS[coefRLS$species %in% sizeRLS$species[i],]$b
}
}
sizeRLS_clean <- sizeRLS 
save(sizeRLS_clean,file="sizeRLS_clean.RData")

sizeBUBOT$a <- NA
sizeBUBOT$b <- NA
for (i in 1:nrow(sizeBUBOT)){
  print(i)
  
  if(sizeBUBOT$Species[i] %in% Species_info$Genus_species) {
    sizeBUBOT$a[i] <- Species_info[Species_info$Genus_species %in% sizeBUBOT$Species[i],]$a
    sizeBUBOT$b[i] <- Species_info[Species_info$Genus_species %in% sizeBUBOT$Species[i],]$b
  }
}
sizeBUBOT_clean <- sizeBUBOT 

save(sizeBUBOT_clean,file="sizeBUBOT_clean.RData")









species_site_scale_longmat$size <- NA
species_site_scale_longmat$a <- NA
species_site_scale_longmat$b <- NA

for (i in 1:nrow(species_site_scale_longmat)){
  
  print(i)
  
  #Species in bubot
  if(species_site_scale_longmat$species[i] %in%  sizeBUBOT_clean$species){
    
    species_site_scale_longmat$size[i] <- mean(sizeBUBOT_clean[sizeBUBOT_clean$species %in% species_site_scale_longmat$species[i],]$size, na.rm = T)
    species_site_scale_longmat$a <- mean(sizeBUBOT_clean[sizeBUBOT_clean$species %in% species_site_scale_longmat$species[i],]$a, na.rm = T)
    species_site_scale_longmat$b <- mean(sizeBUBOT_clean[sizeBUBOT_clean$species %in% species_site_scale_longmat$species[i],]$b, na.rm = T)
  }
  
  # If not Species in RLS
  else if(species_site_scale_longmat$species[i] %in%  sizeRLS$species){
    
    species_site_scale_longmat$size[i] <- mean(sizeRLS[sizeRLS$species %in% species_site_scale_longmat$species[i],]$size, na.rm = T)
    species_site_scale_longmat$a <- mean(sizeRLS[sizeRLS$species %in% species_site_scale_longmat$species[i],]$a, na.rm = T)
    species_site_scale_longmat$b <- mean(sizeRLS[sizeRLS$species %in% species_site_scale_longmat$species[i],]$b, na.rm = T)
  }
  
  # If not species, genus in bubot
  else if(species_site_scale_longmat$genus[i] %in%  sizeBUBOT_clean$genus){
    
    species_site_scale_longmat$size[i] <- mean(sizeBUBOT_clean[sizeBUBOT_clean$genus %in% species_site_scale_longmat$genus[i],]$size, na.rm = T)
    species_site_scale_longmat$a <- mean(sizeBUBOT_clean[sizeBUBOT_clean$genus %in% species_site_scale_longmat$genus[i],]$a, na.rm = T)
    species_site_scale_longmat$b <- mean(sizeBUBOT_clean[sizeBUBOT_clean$genus %in% species_site_scale_longmat$genus[i],]$b, na.rm = T)
  }
  
  # If not species an genus, genus in RLS
  else if(species_site_scale_longmat$genus[i] %in%  sizeRLS$genus){
    
    species_site_scale_longmat$size[i] <- mean(sizeRLS[sizeRLS$genus %in% species_site_scale_longmat$genus[i],]$size, na.rm = T)
    species_site_scale_longmat$a <- mean(sizeRLS[sizeRLS$genus %in% species_site_scale_longmat$genus[i],]$a, na.rm = T)
    species_site_scale_longmat$b <- mean(sizeRLS[sizeRLS$genus %in% species_site_scale_longmat$genus[i],]$b, na.rm = T)
  }
  
  # If not, family in bubot
  else if(species_site_scale_longmat$family[i] %in%  sizeRLS$family){
    
    species_site_scale_longmat$size[i] <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$family[i],]$size, na.rm = T)
    species_site_scale_longmat$a <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$family[i],]$a, na.rm = T)
    species_site_scale_longmat$b <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$family[i],]$b, na.rm = T)
  }
  
  # If not species family, family in RLS
  else if (species_site_scale_longmat$family[i] %in%  sizeRLS$family){
    
    species_site_scale_longmat$size[i] <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$family[i],]$size)
    species_site_scale_longmat$a <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$family[i],]$b)
    species_site_scale_longmat$b <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$family[i],]$a)
  }
  
}


#Remove Rhincodon_typus 
species_site_scale_longmat <- species_site_scale_longmat[!is.na(species_site_scale_longmat$size),]


#Compute weigth  W = a x L^b
species_site_scale_longmat$Indweigth <- species_site_scale_longmat$a * species_site_scale_longmat$size^species_site_scale_longmat$b 
species_site_scale_longmat$Groupweigth <- species_site_scale_longmat$Indweigth * species_site_scale_longmat$abundance

species_site_scale_biomass = melt( species_site_scale_longmat , id.vars = c( "surveys" , "species" ) , measure.vars = "Groupweigth" )
species_site_scale_biomass = dcast( molten , surveys~species,mean,na.rm=T )

species_site_scale_biomass[is.na(species_site_scale_biomass)] <- 0

