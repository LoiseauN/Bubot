####################


alpha_div_all



mod <- lm(sp_richn ~ PC1_hab + PC2_hab + depth,data = alpha_div_all)
mod <- lm(sp_richn ~ PC1_hab + PC2_hab + depth,data = alpha_div_all)





beta_hill










hab_selec<-cbind(rownames(Hab),Hab[,c(1,2,5)])
hab_selec[,2]<-as.numeric(hab_selec[,2])
hab_selec[,3]<-as.numeric(hab_selec[,3])
colnames(hab_selec)<-c("site","Lat", "Long", "Prof")

GDM_results<-matrix(NA,3,6)
rownames(GDM_results)<-c("tot","turn","nest")
colnames(GDM_results)<-c("site","Lat", "Long", "Houle","Prof","Habi","Ile")


hab_selec<- cbind(rownames(hab_pc_site_scale),hab_pc_site_scale[,c(1:4,6)])
colnames(hab_selec)<-c("site","PC1","PC2","PC3","PC4","depth")
hab_selec<-merge(hab_selec,unique(species.site.matrix$site.data[,c("Sample.code","latitude", "longitude")]),by.x="site",by.y="Sample.code",all.x=T)
colnames(hab_selec)<-c("site","PC1","PC2","PC3","PC4","depth","Lat", "Long")
############TOTAL#####################################
all_beta$beta.jtu <- as.data.frame(as.matrix(all_beta$beta.jtu))
                                   
                                   dat <- na.omit(as.data.frame(as.matrix(all_beta$beta.jtu)))
                                   dat <-dat[,colnames(dat) %in% rownames(dat)]
                                   
                                   test <- cbind(rownames(dat), dat)
                                   colnames(test)[1] <- "site"
                                   hab_selec <- hab_selec[hab_selec$site %in% test$site,]
                                   
                                   exFormat3 <- formatsitepair(test,3, 
                                                               XColumn="Long", YColumn="Lat",
                                                               siteColumn="site",predData=hab_selec)
                                   Mod <- gdm(exFormat3, geo=T)
                                   
                                   plot(Mod, plot.layout = c(3, 3))
                                   
                                   ############TURN#####################################
                                   bob<-anosim (as.dist(all_beta$beta.jac) ~  PC1*PC2*PC3*PC4*depth,data = hab_selec)
                                   bob2<-adonis2(as.dist(BETAX$beta.jac) ~ Houle*Prof*Habi*Ile,data = hab_selec)
                                   BETAX
                                   plot(bob)
                                   
                                   
                                   load(system.file("./data/gdm.RData", package="gdm"))
                                   sppData <- gdmExpData[, c(1,2,13,14)]
                                   envTab <- gdmExpData[, c(2:ncol(gdmExpData))]
                                   
                                   
                                   site <- unique(sppData$site)
                                   gdmDissim <- cbind(site, gdmDissim)
                                   exFormat3 <- formatsitepair(gdmDissim, 3, XColumn="Long", YColumn="Lat", predData=envTab,
                                                               siteColumn="site")
                                   
                                   
                                   ############TOTAL#####################################
                                   exFormat3 <- formatsitepair(as.matrix(all_beta$beta.jac),3, 
                                                               predData=hab_selec, XColumn="Long", YColumn="Lat",
                                                               siteColumn="site")
                                   Mod <- gdm(exFormat3, geo=T)
###########################################################################################
#################### start to do some mix model analyses ##################################
###########################################################################################

library("lme4")
library("plyr")
############################### GLM mixte ########################################

#At the video scale


# combine the data in a dataframe to make it easier
number.dat=data.frame(number.ind,
                      depth=as.numeric(species.site.matrix$site.data$depth),
                      habitat.substrat=habit.mca$ind$coord[,2],
                      habitat.bicenose=habit.mca$ind$coord[,1],
                      Island=island.name,
                      site=site.name,
                      replicate=species.site.matrix$site.data$Sample.code,
                      subreplicate=species.site.matrix$site.data$Sample.name)


number.dat <- merge(hab_pc_video_scale,number.dat)
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



new.number.dat$Island    <- as.factor(new.number.dat$Island  )
ggplot(data      = new.number.dat,
       aes(x     = depth,
           y     = number.ind,
           col   = island,
           group = island))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  #theme(legend.position = "none")+
  geom_smooth(method = "loess",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)





#At the site scale 
alpha_div <- merge(alpha_div, hab_pc_site_scale, by="row.names")
load("gravity.RData")
for (i in 2:ncol(gravity)){
  gravity[,i]<- as.numeric(gravity[,i])
  
}
alpha_div<- merge(alpha_div, gravity, by.x="Row.names",by.y="site",all.x=T)


ggplot(data      = alpha_div,
       aes(x     = depth,
           y     = fdiv,
           col   = island,
           group = island))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)

ggplot(data      = alpha_div,
       aes(x     = depth,
           y     = Abu,
           col   = island,
           group = island))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+ ylim(0,1500)+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)



ggplot(data      = alpha_div,
       aes(x     = grav_nearest_pop,
           y     = Abu,
           col   = classDepth,
           group = classDepth))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+ ylim(0,1500)+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)



ggplot(data      = alpha_div,
       aes(x     = gravtot100km,
           y     = sp_richn,
           col   = classDepth,
           group = classDepth))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  #theme(legend.position = "none")+
  geom_smooth(method = "lm",
              se     = FALSE,
              size   = .5, 
              alpha  = .8)


