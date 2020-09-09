
pkgs <- c('ade4','ggplot2','betapart','harrypotter','dplyr','cluster','ape','bbmle','doParallel','missForest','cowplot')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

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
taxo_correct[taxo_correct$variable=="Acanthurus",]$Genus<- "Acanthurus"
taxo_correct[taxo_correct$variable=="Acanthurus",]$Familly<- "Acanthuridae"

taxo_correct[taxo_correct$variable=="Cheilinus",]$Genus<- "Cheilinus"
taxo_correct[taxo_correct$variable=="Cheilinus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable=="Lutjanus",]$Genus<- "Lutjanus"
taxo_correct[taxo_correct$variable=="Lutjanus",]$Familly<- "Lutjanidae"

taxo_correct[taxo_correct$variable=="Lethrinus",]$Genus<- "Lethrinus"
taxo_correct[taxo_correct$variable=="Lethrinus",]$Familly<- "Lethrinidae"

taxo_correct[taxo_correct$variable=="Siganus",]$Genus<- "Siganus"
taxo_correct[taxo_correct$variable=="Siganus",]$Familly<- "Siganidae"

taxo_correct[taxo_correct$variable=="Macropharyngodon",]$Genus<- "Macropharyngodon"
taxo_correct[taxo_correct$variable=="Macropharyngodon",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable=="Pomacentrus",]$Genus<- "Pomacentrus"

taxo_correct[taxo_correct$variable=="Pomacentrus",]$Familly<- "Pomacentridae"

taxo_correct[taxo_correct$variable=="Ostracion",]$Genus<- "Ostracion"
taxo_correct[taxo_correct$variable=="Ostracion",]$Familly<- "Ostraciidae"

taxo_correct[taxo_correct$variable=="Pomacanthus",]$Genus<- "Pomacanthus"
taxo_correct[taxo_correct$variable=="Pomacanthus",]$Familly<- "Pomacanthidae"

taxo_correct[taxo_correct$variable=="Rhinecanthus",]$Genus<- "Rhinecanthus"
taxo_correct[taxo_correct$variable=="Rhinecanthus",]$Familly<- "Balistidae"

taxo_correct[taxo_correct$variable=="Pseudochromis",]$Genus<- "Pseudochromis"
taxo_correct[taxo_correct$variable=="Pseudochromis",]$Familly<- "Pseudochromidae"

taxo_correct[taxo_correct$variable=="Sargocentron",]$Genus<- "Sargocentron"
taxo_correct[taxo_correct$variable=="Sargocentron",]$Familly<- "Holocentridae"

taxo_correct[taxo_correct$variable=="Coris",]$Genus<- "Coris"
taxo_correct[taxo_correct$variable=="Coris",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable=="Aphareus",]$Genus<- "Aphareus"
taxo_correct[taxo_correct$variable=="Aphareus",]$Familly<- "Lutjanidae"

taxo_correct[taxo_correct$variable=="Halichoeres",]$Genus<- "Halichoeres"
taxo_correct[taxo_correct$variable=="Halichoeres",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable=="Cheilodipterus",]$Genus<- "Cheilodipterus"
taxo_correct[taxo_correct$variable=="Cheilodipterus",]$Familly<- "Apogonidae"

taxo_correct[taxo_correct$variable=="Anampses",]$Genus<- "Anampses"
taxo_correct[taxo_correct$variable=="Anampses",]$Familly<- "Labridae"

taxo_correct$Genus <- addLevel(taxo_correct$Genus, "Apogon")
taxo_correct[taxo_correct$variable=="Apogon",]$Genus<- "Apogon"
taxo_correct[taxo_correct$variable=="Apogon",]$Familly<- "Apogonidae"

taxo_correct[taxo_correct$variable=="Cirrhilabrus",]$Genus<- "Cirrhilabrus"
taxo_correct[taxo_correct$variable=="Cirrhilabrus",]$Familly<- "Labridae"

taxo_correct[taxo_correct$variable=="Sphyraena",]$Genus<- "Sphyraena"
taxo_correct[taxo_correct$variable=="Sphyraena",]$Familly<- "Scombridae"

taxo_correct[taxo_correct$variable=="Muraenidae",]$Familly<- "Muraenidae"
taxo_correct[taxo_correct$variable=="Siganidae",]$Familly<- "Siganidae"
taxo_correct[taxo_correct$variable=="Diodontidae",]$Familly<- "Diodontidae"
taxo_correct[taxo_correct$variable=="Gobiidae",]$Familly<- "Gobiidae"
taxo_correct[taxo_correct$variable=="Scorpaenidae",]$Familly<- "Scorpaenidae"
taxo_correct[taxo_correct$variable=="Apogonidae",]$Familly<- "Apogonidae"
taxo_correct[taxo_correct$variable=="Congridae",]$Familly<- "Congridae"
taxo_correct[taxo_correct$variable=="Nemipteridae",]$Familly<- "Nemipteridae"


dat_complet <- merge(dat_complet,taxo_correct, by="variable",all.x= T)
dat_complet <- dat_complet[,-c(7,8)]
colnames(dat_complet)[c(2,28:29)] <- c("VideoID","Genus","Familly")

save(dat_complet,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data/Data_dump/dat_complet.RData")

# Test with random forest, values are false
#cov=dat_complet[,c("variable","Genus","Familly","Size","Mobility","Activity","Schooling","Position",
                   "MaxLengthTL","Troph","clean_diet")]
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



dat_complet_mayotte$clean_diet <- factor(dat_complet_mayotte$clean_diet, levels=c("Herbivore-Detritivore","Omnivore","Planktivore","Invertivore","Piscivore"))
dat_complet_mayotte <- dat_complet_mayotte[!is.na(dat_complet_mayotte$clean_diet),]

main.plot <- ggplot(dat_complet_mayotte, aes(x=depth, y=log10(value),color=clean_diet))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4"))+
  #scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_smooth(method = "loess")+
  theme_bw()+ 
  theme(legend.position = "none")+
  ylim(0,4)+
  labs(x="Depth",y="log10(abu)")+
  geom_vline(xintercept=20,  linetype="dotted",size=1)+
  geom_vline(xintercept=40,  linetype="dotted",size=1)+
  geom_vline(xintercept=60,  linetype="dotted",size=1)+
  geom_vline(xintercept=80,  linetype="dotted",size=1)#+
  #geom_label(label="0-20m", x=8, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="20-40m",x=30, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="40-60m", x=52, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="60-80m", x=74, y=4, size=3, hjust=0,color="black")+
  #geom_label(label=">80m", x=10, y=4, size=3, hjust=0,color="black")

  
    abun_classDepth <- dat_complet_mayotte[,c("value","classDepth","clean_diet")]
    abun_classDepth <- aggregate(. ~ classDepth + clean_diet, data = abun_classDepth, sum)
    abun_classDepth$clean_diet <- factor(abun_classDepth$clean_diet, levels=c("Herbivore-Detritivore","Omnivore","Planktivore","Invertivore","Piscivore"))
    
    ## pyramid charts are two barcharts with axes flipped
    abun_classDepth<- with(abun_classDepth, abun_classDepth[order(clean_diet,classDepth),])
    #("Herbivore-Detritivore","Omnivore","Planktivore","Invertivore","Piscivore"))
    
for (i in 1:unique(abun_classDepth$classDepth)){
  
  sub <- subset(abun_classDepth , abun_classDepth$classDepth==unique(abun_classDepth$classDepth)[i])
  sub$alpha <- sub$value/2
  sub2 <- rbind(sub,sub) 
  sub2$alpha[c(nrow(sub):nrow(sub2))] <- -  sub2$alpha[c(nrow(sub):nrow(sub2))] 
  sub2$direction <-c(rep("pos",nrow(sub)),rep("neg",nrow(sub)))
  
  
  inset.plot <- ggplot(test, aes(x = clean_diet, y = alpha, fill = clean_diet)) + 
    geom_bar(data = subset(test, direction == "pos"), stat = "identity", width=1) + 
    geom_bar(data = subset(test, direction == "neg"), stat = "identity", width=1) + 
    scale_fill_manual(values=c("chartreuse3","gold","blue","red","brown4"))+
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
      legend.background = element_rect(fill = "transparent")
    )
  
  main.plot <-
    ggdraw() +
    draw_plot(main.plot) +
    draw_plot(inset.plot, x = 60, y = 2, width = 13, height = 13)
  
  
}



  
 
  
