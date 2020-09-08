
pkgs <- c('ade4','ggplot2','betapart','harrypotter','dplyr','cluster','ape','bbmle','doParallel','missForest')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

cov=unique(dat_complet[,c("variable","Genus","Familly")])

for (i in 1:nrow(cov)){
  print(i)
  
  if(!grepl("_",as.character(cov$variable[i]), fixed = TRUE)){
    
    classif <- classification(as.character(cov$variable[i]),db= 'itis')

    if(sum(classif[[1]]$rank=="genus")>0){  
      
      cov$Genus[i] <- classif[[1]][classif[[1]]$rank=="genus",]$name
      
      if(is.na(cov$Familly[i])){cov$Familly[i] <- classif[[1]][classif[[1]]$rank=="family",]$name}
      
    }
    
    if(sum(classif[[1]]$rank=="family")>0){  
      
      cov$Familly[i] <- classif[[1]][classif[[1]]$rank=="family",]$name
      
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

cov$variable <- addLevel(cov$variable, "Blenniidae")
cov$Familly <- addLevel(cov$Familly, "Blenniidae")


cov[cov$variable=="Bleniidae",]$variable<- "Blenniidae"
cov[cov$variable=="Blenniidae",]$Familly<- "Blenniidae"

#COMBLE LES NA!!!!

dat_complet <- merge(dat_complet,cov, by="variable",all.x= T)
dat_complet <- dat_complet[,-c(6,7)]
colnames(dat_complet)[27:28] <- c("Genus","Familly")

save(dat_complet,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data/Data_dump/dat_complet.RData")

# Test with random forest, values are false
cov=dat_complet[,c("variable","Genus","Familly","Size","Mobility","Activity","Schooling","Position",
                   "MaxLengthTL","Troph","clean_diet")]
cov= unique(cov)
rownames(cov) <- cov[,1]
cov <- cov[,-1]


registerDoParallel(cores = 4)
cov.test <- cov[!is.na(cov$Genus),]
cov.test[cov.test == "<NA>"] = "NA"  
cov.test$clean_diet <-as.factor(as.character(cov.test$clean_diet))
cov.imp = missForest(cov.test,parallelize = "forests", ntree = 100)
#cov.imp <- cov.imp$ximp

# Based on PCOA 
cov=dat_complet[,c("variable","Mobility","Activity","Schooling","Position",
                   "MaxLengthTL","Troph","clean_diet")]
cov= unique(cov)
rownames(cov) <- cov[,1]
cov <- cov[,-1]
cov.pcoa <- na.omit(cov)
cov.pcoa$Position  <- factor(cov.pcoa$Position,order=T)
cov.pcoa$Schooling <- factor(cov.pcoa$Position,order=T)
cov.pcoa$clean_diet <- as.factor(as.character(cov.pcoa$clean_diet))

# computing PCoA ----
trait.dist <- daisy(cov.pcoa,metric ="gower")
pco.traits <- ape::pcoa(trait.dist)

# Work with 4 dimensions
sp_pc_coord <- pco.traits$vectors[, 1:4]
colnames(sp_pc_coord) <- paste("PC", 1:4, sep = "")


dat_complet <- merge(dat_complet,sp_pc_coord,by.x="variable",by.y="row.names",all.x=T)

dat_complet[is.na(dat_complet$Familly),]


for (i in 1:nrow(dat_complet)) {
  
  print(i)
  
  if(is.na(dat_complet$clean_diet[i])){
    
    #If scale of genus possible
    if(!is.na(dat_complet$Genus[i])){
    
      #Trait 
       trait_selec <- dat_complet[dat_complet$Genus==dat_complet$Genus[i],]
       trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
       trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
              if(nrow(trait_selec)==0) {next}
       
      trait_selec <- unique(trait_selec[,c("variable","Mobility","Activity","Schooling","Position","clean_diet",
                                  "MaxLengthTL","Troph")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
          
      dat_complet[i,"Mobility"]    <- names(sort(table(trait_selec[,"Mobility"]),decreasing = T)[1])
      dat_complet[i,"Activity"]    <- names(sort(table(trait_selec[,"Activity"]),decreasing = T)[1])
      dat_complet[i,"Schooling"]   <- names(sort(table(trait_selec[,"Schooling"]),decreasing = T)[1])
      dat_complet[i,"Position"]    <- names(sort(table(trait_selec[,"Position"]),decreasing = T)[1])
      dat_complet[i,"clean_diet"]  <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      dat_complet[i,"MaxLengthTL"] <- mean(trait_selec$MaxLengthTL,na.rm=T)
      dat_complet[i,"Troph"]       <- mean(trait_selec$Troph,na.rm=T)
             
         
      #PC
      bary <- na.omit(dat_complet[dat_complet$Genus==dat_complet$Genus[i],]) 
      bary <- unique(bary[,c("variable","PC1","PC2","PC3","PC4")])
      dat_complet[i,29:32] <- apply(bary[,c(2:5)],2,sum)  }
    
      
      #If scale of genus impossible, family level
    if(is.na(dat_complet$Genus[i])){
      
      #Trait
      #Scaridae
      trait_selec <- dat_complet[dat_complet$Familly==dat_complet$Familly[i],]
      trait_selec <- trait_selec[rowSums(is.na(trait_selec)) != ncol(trait_selec), ]
      trait_selec <- trait_selec[!is.na(trait_selec$clean_diet),]
  
      if(nrow(trait_selec)==0) {next}
      trait_selec <- unique(trait_selec[,c("variable","Mobility","Activity","Schooling","Position","clean_diet",
                                           "MaxLengthTL","Troph")])
      rownames(trait_selec) <- trait_selec[,1]
      trait_selec <- trait_selec[,-1]
      
      dat_complet[i,"Mobility"]    <- names(sort(table(trait_selec[,"Mobility"]),decreasing = T)[1])
      dat_complet[i,"Activity"]    <- names(sort(table(trait_selec[,"Activity"]),decreasing = T)[1])
      dat_complet[i,"Schooling"]   <- names(sort(table(trait_selec[,"Schooling"]),decreasing = T)[1])
      dat_complet[i,"Position"]    <- names(sort(table(trait_selec[,"Position"]),decreasing = T)[1])
      dat_complet[i,"clean_diet"]  <- names(sort(table(trait_selec[,"clean_diet"]),decreasing = T)[1])
      dat_complet[i,"MaxLengthTL"] <- mean(trait_selec$MaxLengthTL,na.rm=T)
      dat_complet[i,"Troph"]       <- mean(trait_selec$Troph,na.rm=T)
      
      #PC
      bary <- na.omit(dat_complet[dat_complet$Familly==dat_complet$Familly[i],]) 
      bary <- unique(bary[,c("variable","PC1","PC2","PC3","PC4")])
      dat_complet[i,29:32] <- apply(bary[,c(2:5)],2,sum)  }
  }
  
  }

#Add some info for scaridae


ggplot(dat_complet, aes(x=depth, y=log10(value),color=clean_diet))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_smooth()+theme_bw()+ ylim(0,4)+
  facet_wrap(~ island,nrow = 3)  +
  labs(x="Troph",y="log10(abu)")
