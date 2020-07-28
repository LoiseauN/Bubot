#########################################################################################
################## this is an exemple of script to load UVC data and make it ############
############################# ready to use for analyses #################################
#########################################################################################

# load the functions to collect the data
source("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data/0_Function.R")



# open library Vegan
library(vegan)


#########################################################################################
################################ work on fish numbers ###################################
#########################################################################################

# first set a working directory. whatch out a new folder "Data_dump" will be created 
# to save locally the table from the MySQL database
work.folder="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data"
#If working from home
work.folder="~/Documents/Bubot/Analyses/data"

# run the first function to select the data that are wanted
selected.event=Event.list(DB.connection="yes",work.folder=work.folder,measurement.method=c("wave","webfish"),
                          island=c("Europa","Juan_de_Nova","Mayotte"))

# calculate the Max N table
Max.N=MaxN.calculation(DB.connection="yes",work.folder=work.folder,selected.event=selected.event,max.dist=7)

# compute the species matrix. this function is particularly slow, I made it fast, so take your time...
species.site.matrix=species.matrix(DB.connection="yes",work.folder=work.folder,Max.N=Max.N,biomass.calc="no") 

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






data(varespec)
data(varechem)
## Basic Analysis
vare.cap <- capscale(varespec ~ N + P + K + Condition(Al), varechem,
                     dist="bray")

vare.cap <- capscale(varespec ~ N + P + K + Condition(Al), varechem,
                     dist="bray")
###########################################################################################
#################### start to do some mix model analyses ##################################
###########################################################################################

library("lme4")
library("plyr")
############################### GLM mixte #######################################

# combine the data in a dataframe to make it easier
number.dat=data.frame(number.ind,
                      depth=as.numeric(species.site.matrix$site.data$depth),
                      habitat.substrat=habit.mca$ind$coord[,2],
                      habitat.bicenose=habit.mca$ind$coord[,1],
                      Island=island.name,
                      site=site.name,
                      replicate=species.site.matrix$site.data$Sample.code,
                      subreplicate=species.site.matrix$site.data$Sample.name)

#################################################################
############### cleaning dataset ################################
#################################################################

####### check replication with count function
# summary number of replicates
repl=count(number.dat$replicate)
# get a list of sample with replication
to.select=repl[!repl$freq==1,1]

# remove sample without replication
new.number.dat=number.dat[!is.na(match(number.dat$replicate,to.select)),]

# remove juan de nova data
new.number.dat=new.number.dat[!new.number.dat$Island=="Juan_de_nova",]
new.number.dat$Island


############### do LMM or GLMM ########################
library(nlme)

hist(new.number.dat$number.ind)
hist(log10(new.number.dat$number.ind))

#create a model with all effects and interaction
individuals.full=lme(log10(number.ind)~depth*Island*habitat.substrat*habitat.bicenose, random= ~1|site/replicate,data=new.number.dat)

#create a model without interactions
individuals.full.no.inter=lme(log10(number.ind)~depth+Island+habitat.substrat+habitat.bicenose, random= ~1|site/replicate,data=new.number.dat)

# plot residuals of full model
plot(individuals.full.no.inter)

# plot qqplot
qqnorm(residuals(individuals.full.no.inter))
qqline(residuals(individuals.full.no.inter))

# plot hist of residuals
hist(residuals(individuals.full.no.inter))

# collect AIC
sort(c(full.inter=summary(individuals.full)$AICtab[1],
       full=summary(individuals.full.no.inter)$AICtab[1]))

# test for significance for interaction
anova(individuals.full,individuals.full.no.inter)

## do some plotting to check assumptions
# plot residuals of full model
plot(fitted(individuals.full),residuals(individuals.full))





############### DBRDA ########################
hab_tot <- species.site.matrix$site.data
hab_tot <- na.omit(hab_tot)
hab_tot <- hab_tot[,-1]
hab_tot <- aggregate(. ~ Sample.code,FUN=mean, data= hab_tot)



mat_esp <- as.data.frame.matrix(xtabs(Max.N ~ Sampling_code + Species, data = Max.N))
mat_esp <- mat_esp[rownames(mat_esp) %in% hab_tot$Sample.code]

mat_esp <- mat_esp[,apply(mat_esp,2,sum)>5]

vare.cap <- capscale(mat_esp ~ depth, hab_tot,
                     dist="jaccard")


data(varespec)
data(varechem)
## Basic Analysis
vare.cap <- capscale(varespec ~ N + P + K + Condition(Al), varechem,
                     dist="bray")






###########################################################################################################
######################################## get the biomass ##################################################
###########################################################################################################

# first set a working directory. whatch out a new folder "Data_dump" will be created 
# to save locally the table from the MySQL database
work.folder="D:/Dropbox/work/research_projects/MAPOR/R_analyses"


# run the first function to select the data that are wanted
selected.event=Event.list(DB.connection="yes",work.folder=work.folder,measurement.method=c("vidsync"),
                          island=c("Europa","Juan_de_Nova","Mayotte"))


# calculate the Max N table
Max.N=MaxN.calculation(DB.connection="yes",work.folder=work.folder,selected.event=selected.event,max.dist=7)

# compute the species matrix. this function is particularly slow, I made it fast, so take your time...
species.site.matrix=species.matrix(DB.connection="yes",work.folder=work.folder,Max.N=Max.N,biomass.calc="yes") 

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


