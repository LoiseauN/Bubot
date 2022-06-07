library(reshape2)
library(stringr)


sizeBUBOT <- read.table("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/extraction_tailles.txt",sep="\t",header=T)
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/sizeRLS.RData")
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Size/coefRLS.RData")
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")

#same format species
sizeRLS$species <-  gsub(" ", "_", sizeRLS$species )
coefRLS$species <-  gsub(" ", "_", coefRLS$species )

#Add info genus for size
sizeBUBOT$genus <-  str_split_fixed(sizeBUBOT$Species, "_", 2)[,1]
sizeRLS$genus   <-  str_split_fixed(sizeRLS$species, "_", 2)[,1]

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
for (i in 1:nrow(sizeRLS)){
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
colnames(sizeBUBOT_clean)[10] <- "species"
colnames(sizeBUBOT_clean)[12] <- "size"
#remove family and genus in species names in sizeBUBOT_clean to avoid mistake when preparing data
for (i in 1: nrow(sizeBUBOT_clean)){
  
  if(sizeBUBOT_clean$species[i] %in% sizeBUBOT_clean$family){
    sizeBUBOT_clean$species[i] <- NA
  }
  
  if(sizeBUBOT_clean$species[i]  %in% sizeBUBOT_clean$genus){
    sizeBUBOT_clean$species[i]  <- NA
  }
  
}

save(sizeBUBOT_clean,file=file.path(outputs_dir,"sizeBUBOT_clean.RData"))


#CHECK THE RELATIONSHIP BETWEEN BUBOT AND RLS SIZE




load(file= file.path(outputs_dir,"sizeBUBOT_clean.RData"))
load(file= file.path(outputs_dir,"sizeRLS_clean.RData"))
df_size_BUBOT <- sizeBUBOT_clean[,c("species","size")]
df_size_RLS <- data.frame(species = sizeRLS_clean$species,
                          size_RLS = sizeRLS_clean$size)

df_size_BUBOT_mean <- aggregate(. ~ species, data = df_size_BUBOT, mean)
df_size_RLS_mean <-  aggregate(. ~ species, data = df_size_RLS, mean)
df_check <- merge(df_size_BUBOT_mean,df_size_RLS_mean,by="species")

plot(df_check$size,df_check$size_RLS)

ggplot(df_check, aes(x=size_RLS, y=size)) + 
  geom_point(fill ="cadetblue3",pch=21)+
  ylim(min(c(df_check$size_RLS,df_check$size)),max(c(df_check$size_RLS,df_check$size)))+
  xlim(min(c(df_check$size_RLS,df_check$size)),max(c(df_check$size_RLS,df_check$size)))+
  theme_bw()+ylab("Size BUBOT")+xlab("Size RLS")+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 1, hjust = 1)
# add size and replace by RLS size if missing for BUBOT
dat_complet$size <- NA
dat_complet$sizeINBUBOT <- NA

for (i in 1:nrow(dat_complet)){
  
  print(i)
  
  #Species in bubot
  if(dat_complet$species[i] %in%  sizeBUBOT_clean$species){
    dat_complet$sizeINBUBOT[i] <- 1
    dat_complet$size[i] <- mean(sizeBUBOT_clean[sizeBUBOT_clean$species %in% dat_complet$species[i],]$size, na.rm = T)
    
    if(is.na(dat_complet$a[i])){
      dat_complet$a <- mean(sizeBUBOT_clean[sizeBUBOT_clean$species %in% dat_complet$species[i],]$a, na.rm = T)
      dat_complet$b <- mean(sizeBUBOT_clean[sizeBUBOT_clean$species %in% dat_complet$species[i],]$b, na.rm = T)
        }
      }
  
  # If not Species in RLS
  else if(dat_complet$species[i] %in%  sizeRLS$species){
    
    dat_complet$size[i] <- mean(sizeRLS[sizeRLS$species %in% dat_complet$species[i],]$size, na.rm = T)
    if(is.na(dat_complet$a[i])){
    dat_complet$a <- mean(sizeRLS[sizeRLS$species %in% dat_complet$species[i],]$a, na.rm = T)
    dat_complet$b <- mean(sizeRLS[sizeRLS$species %in% dat_complet$species[i],]$b, na.rm = T)
  }
  } 
  # If not species, genus in bubot
  else if(dat_complet$genus[i] %in%  sizeBUBOT_clean$genus){
    
    dat_complet$size[i] <- mean(sizeBUBOT_clean[sizeBUBOT_clean$genus %in% dat_complet$genus[i],]$size, na.rm = T)
    if(is.na(dat_complet$a[i])){
     dat_complet$a <- mean(sizeBUBOT_clean[sizeBUBOT_clean$genus %in% dat_complet$genus[i],]$a, na.rm = T)
    dat_complet$b <- mean(sizeBUBOT_clean[sizeBUBOT_clean$genus %in% dat_complet$genus[i],]$b, na.rm = T)
  }
  }
  # If not species an genus, genus in RLS
  else if(dat_complet$genus[i] %in%  sizeRLS$genus){
    
    dat_complet$size[i] <- mean(sizeRLS[sizeRLS$genus %in% dat_complet$genus[i],]$size, na.rm = T)
    if(is.na(dat_complet$a[i])){
    dat_complet$a <- mean(sizeRLS[sizeRLS$genus %in% dat_complet$genus[i],]$a, na.rm = T)
    dat_complet$b <- mean(sizeRLS[sizeRLS$genus %in% dat_complet$genus[i],]$b, na.rm = T)
    }
  }  
  
  # If not, family in bubot
  else if(dat_complet$family[i] %in%  sizeRLS$family){
    
    dat_complet$size[i] <- mean(sizeRLS[sizeRLS$family %in% dat_complet$family[i],]$size, na.rm = T)
    if(is.na(dat_complet$a[i])){
    dat_complet$a <- mean(sizeRLS[sizeRLS$family %in% dat_complet$family[i],]$a, na.rm = T)
    dat_complet$b <- mean(sizeRLS[sizeRLS$family %in% dat_complet$family[i],]$b, na.rm = T)
  }
  } 
  # If not species family, family in RLS
  else if (dat_complet$family[i] %in%  sizeRLS$family){
    
    dat_complet$size[i] <- mean(sizeRLS[sizeRLS$family %in% dat_complet$family[i],]$size)
    if(is.na(dat_complet$a[i])){
    dat_complet$a <- mean(sizeRLS[sizeRLS$family %in% dat_complet$family[i],]$b)
    dat_complet$b <- mean(sizeRLS[sizeRLS$family %in% dat_complet$family[i],]$a)
  }
  } 
}

#Compute weigth  W = a x L^b
dat_complet$Indweigth <- dat_complet$a * dat_complet$size^dat_complet$b 
dat_complet$Groupweigth <- dat_complet$Indweigth * dat_complet$abundance

dat_complet$site <- str_split_fixed(dat_complet$surveys, "-", 2)[,1]

#Working on Mayotte only
dat_complet<- dat_complet[dat_complet$island=="Mayotte",]

dat_complet <- dat_complet[!is.na(dat_complet$diet),]

save(dat_complet,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")

