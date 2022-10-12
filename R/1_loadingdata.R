#########################################################################################
################## this is an exemple of script to load UVC data and make it ############
############################# ready to use for analyses #################################
#########################################################################################

#  library 
pkgs <- c('rfishbase','leaflet','pbmcapply','parallel','tidyverse','plyr',
          'effects','lme4','lmerTest','broom','broom.mixed','cowplot','gridExtra','ade4','ggplot2','vegan','DBI','dplyr','RMySQL',
          'FactoMineR',"factoextra")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


#########################################################################################
################################ work on fish numbers ###################################
#########################################################################################

# first set a working directory. whatch out a new folder "Data_dump" will be created 
# to save locally the table from the MySQL database
work.folder= here::here("data") 
#If working from home
#work.folder="~/Documents/Bubot/Bubot_Analyse/Bubot/data"

# run the first function to select the data that are wanted
selected.event=Event.list(DB.connection="no",work.folder=work.folder,measurement.method=c("wave","webfish"),
                          island=c("Europa","Juan_de_Nova","Mayotte"))

# calculate the Max N table
Max.N=MaxN.calculation(DB.connection="no",work.folder=work.folder,selected.event=selected.event,max.dist=7)

# compute the species matrix. this function is particularly slow, I made it fast, so take your time...
species.site.matrix=species.matrix(DB.connection="no",work.folder=work.folder,Max.N=Max.N,biomass.calc="no") 

# get a list of species
sp.list=colnames(species.site.matrix$species.matrix)

# extract entire sp list
#write.table(sp.list,file="all.species.list.txt",row.names = F,col.names = F)

# create a vector with the name of sites for analyses
# open the site file to get a site name
sites=as.data.frame(read.csv(file.path(work.folder,"Data_Dump","site.code.csv")))

# make a vector with site name
site.name=sites$Site[match(species.site.matrix$site.data$Sample.code,sites$Sample_code)]

# make a vector with island name
island.name=sites$island[match(species.site.matrix$site.data$Sample.code,sites$Sample_code)]

# compute total number of specimen
number.ind=unlist(apply(species.site.matrix$species.matrix,1,sum))

# compute species number
species.div=specnumber(species.site.matrix$species.matrix)

# compute shannon
shannon=diversity(species.site.matrix$species.matrix, index="shannon")

# compute pielou
# calculate Pielou index (J) for each transect
J=shannon/log(specnumber(species.site.matrix$species.matrix))

# just check the total density per site and plot it against depth to check
tot.density=apply(species.site.matrix$species.matrix,1,sum)
plot(species.site.matrix$site.data$depth,tot.density,pch=16,cex=0.5,ylim=c(0,250))

####### make some boxplot ###########
# make some generic depth class
depth.class=cut(species.site.matrix$site.data$depth, c(0,15,35,55,120), labels = FALSE)
# make a boxplot
boxplot(number.ind[island.name=="Mayotte"]~depth.class[island.name=="Mayotte"]*site.name[island.name=="Mayotte"],
        col=c("blue","green","orange","red"),ylim=c(0,250))

#### exemple to select a given familly
# check just carangidae
select.fam=species.site.matrix$species.data$familly.select=="LUTJANIDAE"

carang.data=species.site.matrix$species.matrix[,select.fam]

# compute total number of specimen
number.ind.carang=unlist(apply(species.site.matrix$species.matrix,1,sum))
# make a boxplot
boxplot(number.ind.carang[island.name=="Juan_de_nova"]~depth.class[island.name=="Juan_de_nova"]*site.name[island.name=="Juan_de_nova"],
        col=c("blue","green","orange","red"))#,ylim=c(0,250))



###########################################################################################################
##################################### get the functional traits ###########################################
###########################################################################################################

# set working directory
setwd(work.folder)

# save the data locally in case of offline work
setwd(paste(work.folder,"/Data_dump",sep=""))
# write the table
write.table(Species_info,"Species_info.txt",sep=";")

# set the dump directory
setwd(paste(work.folder,"/Data_dump",sep=""))

# reopen this file
Species_info=read.table("Species_info.txt",sep=";",header = T)

#############################################################################

# collect only functional trait data
fish_traits <- Species_info[,colnames(Species_info) %in% c(
"Genus_species","Class","Familly","Genus","Schooling","Activity","Mobility","Reef_associated",
"Position","Diet","Trophic.level_Fishbase","Trophic.level_Fishbase","BodyShape_Fishbase","MaxLengthTL_Fishbase",
"Trophic.level_Fishbase","BodyShape_Fishbase","a","b"),]
colnames(fish_traits)[1] <- "Species"



                          
##########################
##### Habitat ############
##########################
# collect the habitat data
habitat=species.site.matrix$site.data[,c(11:18)]
row.names(habitat)=species.site.matrix$site.data$Sample.name
habitat[,1] <- as.factor(as.character(habitat[,1]))
habitat[,4] <- as.factor(as.character(habitat[,4]))
habitat[,5] <- as.factor(as.character(habitat[,5]))

habitat <- na.omit(habitat)
# give the row name


res.famd <- FAMD(habitat, graph = FALSE)
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_famd_ind(res.famd, 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)




habit.score <- res.famd$ind$coord
colnames(habit.score) <- c("PC1","PC2","PC3","PC4","PC5")



