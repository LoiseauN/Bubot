
###########################################################################################################
######################################## get the biomass ##################################################
###########################################################################################################

# first set a working directory. whatch out a new folder "Data_dump" will be created 
# to save locally the table from the MySQL database
#work.folder="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/data"


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



###########################################################################################################
######################################## Work at the site scale ##################################################
###########################################################################################################

species_site_scale_bm <- species.site.matrix$species.matrix
species_site_scale_bm <- species_site_scale_bm[,which(colnames(species_site_scale_bm)!="unknown_fish")]

hab_pc_site_scale_bm <- habit.score
rownames(hab_pc_site_scale_bm) <- rownames(species_site_scale_bm)

hab_pc_site_scale_bm<- merge(hab_pc_site_scale_bm,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code","depth")]),by.x="row.names",by.y="Sample.name") 
hab_pc_site_scale_bm<-hab_pc_site_scale_bm[,-1]
hab_pc_site_scale_bm<- aggregate(. ~ Sample.code, data = hab_pc_site_scale_bm, mean)

hab_pc_site_scale_bm$classDepth <- NA

for (i in 1: nrow(hab_pc_site_scale_bm)){
  if(hab_pc_site_scale_bm$depth[i]<20){ hab_pc_site_scale_bm$classDepth[i] <- "[0-20["}
  if(hab_pc_site_scale_bm$depth[i]>=20 & hab_pc_site_scale_bm$depth[i]<40){ hab_pc_site_scale_bm$classDepth[i] <- "[20-40["}
  if(hab_pc_site_scale_bm$depth[i]>=40 & hab_pc_site_scale_bm$depth[i]<60){ hab_pc_site_scale_bm$classDepth[i] <- "[40-60["}
  if(hab_pc_site_scale_bm$depth[i]>=60 & hab_pc_site_scale_bm$depth[i]<80){ hab_pc_site_scale_bm$classDepth[i] <- "[60-80["}
  if(hab_pc_site_scale_bm$depth[i]>=80){ hab_pc_site_scale_bm$classDepth[i] <- ">80"}
  
}

hab_pc_site_scale_bm<- merge(hab_pc_site_scale_bm,sites,by.x="Sample.code",by.y="Sample_code")
rownames(hab_pc_site_scale_bm) <- hab_pc_site_scale_bm[,1]
hab_pc_site_scale_bm<- hab_pc_site_scale_bm[,-1]


for(i in 7:9){hab_pc_site_scale_bm[,i] <-as.factor(hab_pc_site_scale_bm[,i]) } 

species_site_scale_bm <- species.site.matrix$species.matrix
species_site_scale_bm <- merge(species_site_scale_bm,data.frame(species.site.matrix$site.data[,c("Sample.name","Sample.code")]),by.x="row.names",by.y="Sample.name") 
species_site_scale_bm <- species_site_scale_bm[,-1]
species_site_scale_bm <- aggregate(. ~ Sample.code, data = species_site_scale_bm, sum)
rownames(species_site_scale_bm) <- species_site_scale_bm[,1]
species_site_scale_bm <- species_site_scale_bm[,-1]
species_site_scale_bm <- species_site_scale_bm[,which(colnames(species_site_scale_bm)!="unknown_fish")]
species_site_scale_bm <- species_site_scale_bm[apply(species_site_scale_bm,1,sum)>0,]
hab_pc_site_scale_bm <- hab_pc_site_scale_bm[rownames(hab_pc_site_scale_bm) %in% rownames(species_site_scale_bm) ,]


`%notin%` <- Negate(`%in%`)

abumat <-  dat_complet[,c("variable","value","Sample.code")]
abumat <-   as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= dat_complet))

cov=dat_complet[,c("variable","Mobility","Activity","Schooling","Position",'Size',
                   "Diet")] #maxLength ,"clean_diet"
cov= unique(cov)
rownames(cov) <- cov[,1]
cov <- cov[,-1]
cov.pcoa <- na.omit(cov)

trait.dist <- cluster::daisy(cov.pcoa,metric ="gower")

abumat <- abumat[,apply(abumat,2,sum)>0]
abumat <- abumat[apply(abumat,1,sum)>0,]
trait.dist_mat <- as.matrix(trait.dist)
abumat <- as.matrix(abumat[,colnames(abumat) %in% rownames(trait.dist_mat)])
trait.dist_mat <- trait.dist_mat[,colnames(trait.dist_mat) %in% colnames(abumat)]
trait.dist_mat <- trait.dist_mat[rownames(trait.dist_mat) %in% colnames(abumat),]


abumat0_1 <- abumat
abumat0_1[abumat0_1>0] <- 1

abumat_relatif <- abumat
for (i in 1:nrow(abumat_relatif)){
  abumat_relatif[i,] <- abumat_relatif[i,]/sum(abumat_relatif[i,])
}


#Plot distance decay en fonction des profondeurs
coord_depth <- species.site.matrix$site.data[,c(2,5:7)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]
#--

coord_depth <- coord_depth[rownames(coord_depth) %in% rownames(abumat),]

coord <- coord_depth[,-2]
colnames(coord) <- c("name","lat","lon")
geodist <-  as.matrix(round(GeoDistanceInMetresMatrix(coord) / 1000,3))
depthdist <- as.matrix(dist(coord_depth[,2],"euclidean"))
colnames(depthdist)<- rownames(coord_depth)
rownames(depthdist)<- rownames(coord_depth)


###############################################################################
###############################################################################
#taxo richness : data= 0/1, q=0, tau=min (c'est la richesse spé)
#taxo entropy: data= relative biomass, q=1, tau=min (c'est exp(Shannon) )
#fonctio richness: data= 0/1, q=0, tau=mean
#fonctio entropy: data= 0/1, q=1, tau=mean



#alpha
alpha_hill_taxo_richess  <- alpha.fd.hill (asb_sp_w = abumat0_1,
                                           sp_dist  = trait.dist_mat,
                                           q        = 0,
                                           tau      = "min")$asb_FD_Hill

alpha_hill_taxo_entropy  <- alpha.fd.hill (asb_sp_w = abumat_relatif,
                                           sp_dist  = trait.dist_mat,
                                           q        = 1,
                                           tau      = "min")$asb_FD_Hill

alpha_hill_fonct_richess <- alpha.fd.hill (asb_sp_w = abumat0_1,
                                           sp_dist  = trait.dist_mat,
                                           q        = 0,
                                           tau      = "mean")$asb_FD_Hill

alpha_hill_fonct_entropy <- alpha.fd.hill (asb_sp_w = abumat_relatif,
                                           sp_dist  = trait.dist_mat,
                                           q        = 1,
                                           tau      = "mean")$asb_FD_Hill

alpha_hill_all <- data.frame(hill_taxo_richess  = alpha_hill_taxo_richess,
                             hill_taxo_entropy  = alpha_hill_taxo_entropy,
                             hill_fonct_richess = alpha_hill_fonct_richess,
                             hill_fonct_entropy = alpha_hill_fonct_entropy)

#alpha_hill_all$classDepth <- as.factor(str_split_fixed(rownames(alpha_hill_all), "_", 2)[,2])

alpha_hill_all <- merge(alpha_div,alpha_hill_all,by="row.names",all.x=T)


for (i in 1:4){
  print(i)
  if (i == 1) data_plot <- alpha_hill_all
  if (i == 2) data_plot <- subset(alpha_hill_all,alpha_hill_all$island=="Mayotte")
  if (i == 3) data_plot <- subset(alpha_hill_all,alpha_hill_all$island=="Juan_de_nova")
  if (i == 4) data_plot <- subset(alpha_hill_all,alpha_hill_all$island=="Europa")
  
  
  FD_q0 <- ggplot(data_plot, aes(x=depth, y=FD_q0)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill taxo richness")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  
  FD_q1<- ggplot(data_plot, aes(x=depth, y=FD_q1)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill taxo entropy")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  FD_q0.1<- ggplot(data_plot, aes(x=depth, y=FD_q0.1)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill funct richness")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  FD_q1.1<- ggplot(data_plot, aes(x=depth, y=FD_q1.1)) + 
    geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
    #geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
    #  position=position_dodge(0.05),color ="cadetblue3")+
    theme_bw()+ylab("Hill funct entropy")+
    geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")
  
  
  if (i == 1) title <- textGrob("All Islands",
                                gp=gpar(fontsize=20,fontface=2))
  if (i == 2) title <- textGrob("Mayotte",
                                gp=gpar(fontsize=20,fontface=2))
  if (i == 3) title <- textGrob("Juan de Nova",
                                gp=gpar(fontsize=20,fontface=2))
  if (i == 4) title <- textGrob("Europa",
                                gp=gpar(fontsize=20,fontface=2))
  
  grid.arrange(FD_q0,FD_q0.1,FD_q1,FD_q1.1,ncol=2,top = title)
  
}



#beta
beta_hill_taxo_richess  <- beta.fd.hill (asb_sp_w = abumat0_1,
                                         sp_dist  = trait.dist_mat,
                                         q        = 0,
                                         tau      = "min",
                                         beta_type = "Jaccard")

beta_hill_taxo_entropy  <- beta.fd.hill (asb_sp_w = abumat_relatif,
                                         sp_dist  = trait.dist_mat,
                                         q        = 1,
                                         tau      = "min",
                                         beta_type = "Jaccard")

beta_hill_fonct_richess <- beta.fd.hill (asb_sp_w = abumat0_1,
                                         sp_dist  = trait.dist_mat,
                                         q        = 0,
                                         tau      = "mean",
                                         beta_type = "Jaccard")

beta_hill_fonct_entropy <- beta.fd.hill (asb_sp_w = abumat_relatif,
                                         sp_dist  = trait.dist_mat,
                                         q        = 1,
                                         tau      = "mean",
                                         beta_type = "Jaccard")


beta_hill_taxo_richess_t <- reshape::melt(as.matrix(beta_hill_taxo_richess$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_taxo_richess$beta_fd_q$q0)))$value,]
beta_hill_taxo_entropy_t <- reshape::melt(as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_taxo_entropy$beta_fd_q$q1)))$value,]
beta_hill_fonct_richess_t <- reshape::melt(as.matrix(beta_hill_fonct_richess$beta_fd_q$q0))[melt(upper.tri(as.matrix(beta_hill_fonct_richess$beta_fd_q$q0)))$value,]
beta_hill_fonct_entropy_t <- reshape::melt(as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1))[melt(upper.tri(as.matrix(beta_hill_fonct_entropy$beta_fd_q$q1)))$value,]


#Plot distance  en fonction des profondeurs
coord_depth <- species.site.matrix$site.data[,c(2,5:7)]
coord_depth<- aggregate(. ~ Sample.code, data = coord_depth, mean)
rownames(coord_depth) <- coord_depth[,1]

coord_depth <- coord_depth[rownames(coord_depth) %in% rownames(abumat),]

coord <- coord_depth[,-2]
colnames(coord) <- c("name","lat","lon")
geodist <-  as.matrix(round(GeoDistanceInMetresMatrix(coord) / 1000,3))
depthdist <- as.matrix(dist(coord_depth[,2],"euclidean"))
colnames(depthdist)<- rownames(coord_depth)
rownames(depthdist)<- rownames(coord_depth)

depthdist_t <- reshape::melt(as.matrix(depthdist))[melt(upper.tri(as.matrix(depthdist)))$value,]
geodist_t  <- reshape::melt(as.matrix(geodist))[melt(upper.tri(as.matrix(geodist)))$value,]

beta_hill_all <- data.frame(X1                      = beta_hill_taxo_richess_t[,1],
                            X2                      = beta_hill_taxo_richess_t[,2],
                            beta_hill_taxo_richess  = beta_hill_taxo_richess_t[,3],
                            beta_hill_taxo_entropy  = beta_hill_taxo_entropy_t[,3],
                            beta_hill_fonct_richess = beta_hill_fonct_richess_t[,3],
                            beta_hill_fonct_entropy = beta_hill_fonct_entropy_t[,3],
                            depthdist               = depthdist_t[,3],
                            geodist                 = geodist_t[,3])

BetaFD_q0 <- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_taxo_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo richness")

BetaFD_q1<- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_taxo_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill taxo entropy")

BetaFD_q0.1<- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_fonct_richess))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc richness")

BetaFD_q01.1<- ggplot(beta_hill_all,aes(x=depthdist,y=beta_hill_fonct_entropy))+
  geom_point(color="cyan4")+
  theme_bw()+
  ylab("beta")+
  theme(legend.position = "none")+
  ggtitle("Beta hill fonc entropy")

grid.arrange(BetaFD_q0,BetaFD_q0.1,BetaFD_q1,BetaFD_q01.1)









