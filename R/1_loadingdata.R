#########################################################################################
################## this is an exemple of script to load UVC data and make it ############
############################# ready to use for analyses #################################
#########################################################################################

# load the functions to collect the data




#  library 



pkgs <- c('rfishbase','leaflet','pbmcapply','parallel','tidyverse','plyr',
          'effects','lme4','lmerTest','broom','broom.mixed','cowplot','gridExtra','ade4','ggplot2','vegan','DBI','dplyr','RMySQL')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


#########################################################################################
################################ work on fish numbers ###################################
#########################################################################################

# first set a working directory. whatch out a new folder "Data_dump" will be created 
# to save locally the table from the MySQL database
work.folder="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data"
#If working from home
#work.folder="~/Documents/Bubot/Analyses/data"

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



# load MySQL drivers
drv=dbDriver("MySQL")

# get connection to the database
con = dbConnect(drv, user="UVC.reader", dbname="Underwater Visual Census",password="Mayfish976",host="162.38.198.139")

# just check tables availables
dbListTables(con)

# load the table
Species_info=dbReadTable(con, "Species_info")

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
fish_traits <- Species_info[,c(1,2,3,10:15)]
rownames(fish_traits) <- fish_traits[,1]
fish_traits <- fish_traits[,-1]

for (i in 1:5) {
  fish_traits[,i]<- factor(fish_traits[,i],order=T)
}
fish_traits[,6]<- factor(fish_traits[,6])


################################################################################
#' Extract Trait in Rfishbase
#' 
#' Authors: ---------
#' 
#' @function  TODO 
#' 
#' @param sp_distrib a data frame with lat, lot and presence absence of species
#' Data from OBIS extracted by Albouy, Camille, et al. "The marine fish food web is globally connected." Nature ecology & evolution 3.8 (2019): 1153-1161.
#' @param cores number of core to use to decrease computing time
#'              
#'@param 
#' 
#' @return a list of dataframe with species in element  and a data fra
#' 
#################################################################################

nb_cores <- 2
trait_fishbase <- do.call(rbind,pbmclapply(1:nrow(Species_info), function(i){   #
  
  #sp <- "Abudefduf sparoides"
  #sp<-as.character(gsub("_", " ", sp))
  sp<-as.character(gsub("_", " ", Species_info$ID[i]))
  #Loading growth data from Fishbase
  estimate_growth_data <- estimate(sp) %>%
    #Selecting the column that are of interest
    select(c("Species",'MaxLengthTL','Troph','DepthMax',"a","b",
             'K','TempPrefMin', 'TempPrefMean', 'TempPrefMax')) 


  species_info <- species(sp) %>%
  #Selecting the column that are of interest
   select(c("Species","BodyShapeI"))

  #INFO VRAIMENT PAS TERRIBLE DES ZERO PARTOUT, -1 QUAND Y A UNE INFO??
  #traits_level <- ecology(sp) %>%
    #Selecting the column that are of interest
   # select(c("Species","FeedingType","FoodTroph","Schooling","Solitary","Shoaling",
    #         "Benthic","Sessile","Mobile","Demersal","Endofauna","Pelagic","Megabenthos",
    #         "Level")) 
  
  #average variables and select the most frequent item
  #traits_level <-  data.frame(
  #  t(data.frame(sapply(c("Species","FeedingType"), function(i)
  #  names(sort(table(traits_level[,i]),decreasing = TRUE)[1])))),
  #t(data.frame(apply(traits_level[,-c(1,2)],2,min,na.rm=T))))
  
  diet_data <- fooditems(sp)%>%
    #Selecting the column that are of interest
      select(c("FoodI","FoodII","FoodIII"))
  
  if(nrow(diet_data)==1) { diet_data <- data.frame(Species = sp,diet_data) 
  
  }else{ 
    
    #choose the most frequent item
  diet_data <- data.frame(Species=sp,t(data.frame(sapply(c("FoodI","FoodII","FoodIII"), function(i)
    names(sort(table(diet_data[,i]),decreasing = TRUE)[1])))))
  }
  
  swim_data  <-  data.frame(swimming(sp))%>%
    #Selecting the column that are of interest
    select(c("Species","AdultType"))%>%
    dplyr::rename(swimtype="AdultType")
  
  #taxonomy <- taxize::tax_name(query = sp, get = c("genus","family"), db = "ncbi")  %>%
  #Selecting the column that are of interest
  # select(c("query","genus","family"))
  #colnames(taxonomy)[1] <- "Species"
  
  data <- plyr::join_all(list(species_info,diet_data,swim_data, estimate_growth_data), by = 'Species', type = 'full')
  
  return(data)
  
},mc.cores = nb_cores))

trait_fishbase$Species <- as.character(gsub(" ", "_", trait_fishbase$Species))

fish_traits <- merge(fish_traits,trait_fishbase,by.x="row.names",by.y="Species",all.x=T)
colnames(fish_traits)[1] <- "Species"
###########################################################################################################
######################################## get the biomass ##################################################
###########################################################################################################

# first set a working directory. whatch out a new folder "Data_dump" will be created 
# to save locally the table from the MySQL database
work.folder="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data"


# run the first function to select the data that are wanted
selected.event=Event.list(DB.connection="no",work.folder=work.folder,measurement.method=c("vidsync"),
                          island=c("Europa","Juan_de_Nova","Mayotte"))


# calculate the Max N table
Max.N=MaxN.calculation(DB.connection="no",work.folder=work.folder,selected.event=selected.event,max.dist=7)

# compute the species matrix. this function is particularly slow, I made it fast, so take your time...
species.site.matrix=species.matrix(DB.connection="no",work.folder=work.folder,Max.N=Max.N,biomass.calc="yes") 

# get a list of species
sp.list=colnames(species.site.matrix$species.matrix)

# extract entire sp list
#write.table(sp.list,file="size.species.list.txt",row.names = F,col.names = F)

# create a vector with the name of sites for analyses
# open the site file to get a site name
sites=as.data.frame(read.csv("site.code.csv"))

# make a vector with site name
site.name=sites$Site[match(species.site.matrix$site.data$Sample.code,sites$Sample_code)]

# make a vector with island name
island.name=sites$island[match(species.site.matrix$site.data$Sample.code,sites$Sample_code)]

# compute total biomass
tot.biomass=unlist(apply(species.site.matrix$species.matrix,1,sum))


# just check the total biomass per site and plot it against depth to check
tot.biomass=apply(species.site.matrix$species.matrix,1,sum)
plot(species.site.matrix$site.data$depth,tot.biomass,pch=16,cex=0.5,ylim=c(0,150000))

####### make some boxplot ###########
# make some generic depth class
depth.class=cut(species.site.matrix$site.data$depth, c(0,20,40,60,120), labels = FALSE)
# make a boxplot
boxplot(tot.biomass[island.name=="Mayotte"]~depth.class[island.name=="Mayotte"]*site.name[island.name=="Mayotte"],
        col=c("blue","green","orange","red"))

#### exemple to select a given familly
# check just labridae
select.fam=species.site.matrix$species.data$familly.select=="SERRANIDAE"

select.fam.data=species.site.matrix$species.matrix[,select.fam]
# compute total biomass
tot.biomass.fam=select.fam.data
tot.biomass.fam=unlist(apply(select.fam.data,1,sum))

# make some generic depth class
depth.class=cut(species.site.matrix$site.data$depth, c(0,20,40,60,120), labels = FALSE)
# make a boxplot
boxplot(tot.biomass.fam[island.name=="Juan_de_nova"]~depth.class[island.name=="Juan_de_nova"]*site.name[island.name=="Juan_de_nova"],
        col=c("blue","green","orange","red"))




##########################
##### Habitat ############
##########################
#install.packages("PCAmixdata")
library(PCAmixdata)

# collect the habitat data
habitat=species.site.matrix$site.data[,c(10:17)]
# give the row name
row.names(habitat)=species.site.matrix$site.data$Sample.name

# do a PCA on categorical data
habit.mca=PCAmix(X.quali=apply(habitat,2,as.character),rename.level = T)

habit.score=habit.mca$ind$coord

# get the row name so it get ploted on the MCA graph
r.names=species.site.matrix$site.data$H.community # change the last $ by H.substrat.type, Community,... to get an idea of habitat

# get the variance of axes
names=list(r.names,c("PC1","PC2","PC3","PC4","PC5"))
dimnames(habit.score)=names

vars=apply(habit.score,2,var)
perc.axes=(vars/sum(vars))*100
barplot(perc.axes)

plot(habit.mca$ind$coord[,1],habit.mca$ind$coord[,2],cex=0)#,xlim=c(-3,-2),ylim=c(0,1))
text(habit.mca$ind$coord[,1],habit.mca$ind$coord[,2],names[[1]])

# just to check that axis 1 seems to match depth related habitat
plot(habit.mca$ind$coord[,1],species.site.matrix$site.data$depth)

