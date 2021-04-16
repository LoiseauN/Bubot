library(reshape2)
library(stringr)
sizeBUBOT <- read.table("extraction_tailles.txt",sep="\t",header=T)
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/sizeRLS.RData")

#same format species
sizeRLS$species <-  gsub(" ", "_", sizeRLS$species )

#Add info genus for size


sizeBUBOT$genus <-  str_split_fixed(sizeBUBOT$Species, "_", 2)[,1]
sizeRLS$genus   <-  str_split_fixed(sizeRLS$Species, "_", 2)[,1]

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

# RAJOUTER QUE SI Y A UNE FAMILY DANS GENUS A ENLEVER

if(sizeBUBOT$genus[i] %in sizeBUBOT$genus[i]
   






species_site_scale_longmat$size <- NA

for (i in 1:nrow(species_site_scale_longmat)){
  
  
  if(species_site_scale_longmat$species[i] %in%  sizeBUBOT$Species){
    
    species_site_scale_longmat$size[i] <- mean(sizeBUBOT[sizeBUBOT$Species %in% species_site_scale_longmat$species[i],]$Size)
    
  }
  
  else if(species_site_scale_longmat$species[i] %in%  sizeRLS$species){
    
    species_site_scale_longmat$size[i] <- mean(sizeRLS[sizeRLS$species %in% species_site_scale_longmat$species[i],]$size)
    
  }
  
  else if(species_site_scale_longmat$family[i] %in%  sizeRLS$family){
    
    species_site_scale_longmat$size[i] <- mean(sizeRLS[sizeRLS$family %in% species_site_scale_longmat$species[i],]$size)
    
  }
  
  
}


sum(unique(size$Species) %in% colnames(species_site_scale0_1_mayotte))


species_site_scale0_1_mayotte <- species_site_scale0_1_mayotte[,apply(species_site_scale0_1_mayotte,2,sum)>0]
`%notin%` <- Negate(`%in%`)

NotSize <- species_site_scale0_1_mayotte[,colnames(species_site_scale0_1_mayotte) %notin%  unique(size$Species)]
NotSize <- colnames(NotSize)

size