
tab <- merge(species_video_scale,hab_pc_video_scale[,c(2,8:10)],by.x="row.names",by.y="Row.names")
rownames(tab) <- tab[,1]
tab <- tab[,-1]

tab0_20   <- tab[tab$classDepth %in% "[0-20[",]
tab20_40  <- tab[tab$classDepth %in% "[20-40[",]
tab40_60  <- tab[tab$classDepth %in% "[40-60[",]
tab60_80  <- tab[tab$classDepth %in% "[60-80[",]
tab_sup80 <- tab[tab$classDepth %in% ">80",]

abu0_20   <- tab0_20[,-c(319:321)]
abu20_40  <- tab20_40[,-c(319:321)]
abu40_60  <- tab40_60[,-c(319:321)]
abu60_80  <- tab60_80[,-c(319:321)]
abu_sup80 <- tab_sup80[,-c(319:321)]

abu0_20   <- data.frame(abu=apply(abu0_20[,apply(abu0_20,2,sum)>0,],2,sum))
trait_abu0_20  <- data.frame(merge(abu0_20,fish_traits,by.x="row.names",by.y="Species",all.x=T))

abu20_40  <-data.frame(abu=apply(abu20_40[,apply(abu20_40,2,sum)>0,],2,sum))
trait_abu20_40  <- data.frame(merge(abu20_40,fish_traits,by.x="row.names",by.y="Species",all.x=T))

abu40_60  <- data.frame(abu=apply(abu40_60[,apply(abu40_60,2,sum)>0,],2,sum))
trait_abu40_60  <- data.frame(merge(abu40_60,fish_traits,by.x="row.names",by.y="Species",all.x=T))

abu60_80  <- data.frame(abu=apply(abu60_80[,apply(abu60_80,2,sum)>0,],2,sum))
trait_abu60_80  <- data.frame(merge(abu60_80,fish_traits,by.x="row.names",by.y="Species",all.x=T))

abu_sup80 <- data.frame(abu=apply(abu_sup80[,apply(abu_sup80,2,sum)>0,],2,sum))
trait_abu_sup80  <- data.frame(merge(abu_sup80,fish_traits,by.x="row.names",by.y="Species",all.x=T))

res<-rbind(trait_abu0_20,trait_abu20_40,trait_abu40_60,
           trait_abu60_80,trait_abu_sup80)

res<- cbind(res,c(rep("[0-20[",nrow(trait_abu0_20)),
                  rep("[20-40[",nrow(trait_abu20_40)),
                  rep("[40-60[",nrow(trait_abu40_60)),
                  rep("[60-80[",nrow(trait_abu60_80)),
                  rep(">80",nrow(trait_abu_sup80)))) 


colnames(res)[c(1,25)]   <- c("Species","Depth")

#SIZE
ggplot(res, aes(x=log10(MaxLengthTL), y=log10(abu),color=Depth))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_smooth(method ="lm")+theme_bw()+
  facet_wrap(~ Depth,ncol = 3)  +ylim(0,4)+
  labs(x="log10(MaxLengthTL)",y="log10(abu)")

#TROPHIQUE
ggplot(res, aes(x=Troph, y=log10(abu),color=Depth))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_smooth(method ="lm")+theme_bw()+ ylim(0,4)+
  facet_wrap(~ Depth,ncol = 3)  +
  labs(x="Troph",y="log10(abu)")

res_diet <- na.omit(data.frame(Diet = res$Diet, abu = res$abu,  Depth = res$Depth))

res_diet$clean_diet <- NA
for (i in 1:nrow(res_diet)){
  if       (res_diet$Diet[i]=="HD")        { res_diet$clean_diet[i] <- "Herbivore-Detritivore" }
  else if  (res_diet$Diet[i]=="PK")  { res_diet$clean_diet[i] <- "Planktivore" }
  else if  (res_diet$Diet[i]=="FC")  { res_diet$clean_diet[i] <- "Piscivore" }
  else if  (res_diet$Diet[i]=="IM")  { res_diet$clean_diet[i] <- "Invertivore" }
  else if  (res_diet$Diet[i]=="IS")  { res_diet$clean_diet[i] <- "Invertivore" }
  else if  (res_diet$Diet[i]=="OM")   { res_diet$clean_diet[i] <- "Omnivore" }
  else if  (res_diet$Diet[i]=="HM")  { res_diet$clean_diet[i] <- "Herbivore-Detritivore" }
  else   {res_diet$clean_diet[i] <- NA }
 
}

res_diet <- xtabs(abu ~ clean_diet + Depth, res_diet)

res_diet_relative <- res_diet

for (i in 1:ncol(res_diet_relative)){
  
  for (j in 1:nrow(res_diet_relative)){
    
    res_diet_relative[j,i]<- (res_diet[j,i]/sum(res_diet[,i]))*100
  }
  
}

res_diet_relative <- data.frame(res_diet_relative)
#Pour conserver l'ordre des boxplot
res_diet_relative$clean_diet<- factor(res_diet_relative$clean_diet,levels = c('Herbivore-Detritivore','Omnivore','Planktivore','Invertivore','Piscivore'),ordered = TRUE)


p <- ggplot(data = res_diet_relative, aes(x = clean_diet, y = Freq,
                                       fill=clean_diet)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  theme_bw()
p + facet_wrap(~ Depth, ncol = 2)

#####################
tab2 <- merge(species_video_scale,hab_pc_video_scale[,c(2,8:10)],by.x="row.names",by.y="Row.names")
rownames(tab2) <- tab2[,1]



library(reshape2)
dat_complet<-melt(tab2, id.vars = c(1,320:322))
dat_complet <- dat_complet[dat_complet$value >0,]
dat_complet  <- data.frame(merge(dat_complet,fish_traits,by.x="variable",by.y="Species",all.x=T))

dat_complet$clean_diet <- NA
for (i in 1:nrow(dat_complet)){
  
  print(i)
  if(is.na(dat_complet$Diet[i])) { dat_complet$clean_diet[i] <- NA }
  else if(dat_complet$Diet[i]=="HD")  { dat_complet$clean_diet[i] <- "Herbivore-Detritivore" }
  else if(dat_complet$Diet[i]=="PK")  { dat_complet$clean_diet[i] <- "Planktivore" }
  else if(dat_complet$Diet[i]=="FC")  { dat_complet$clean_diet[i] <- "Piscivore" }
  else if(dat_complet$Diet[i]=="IM")  { dat_complet$clean_diet[i] <- "Invertivore" }
  else if(dat_complet$Diet[i]=="IS")  { dat_complet$clean_diet[i] <- "Invertivore" }
  else if(dat_complet$Diet[i]=="OM")   { dat_complet$clean_diet[i] <- "Omnivore" }
  else { dat_complet$clean_diet[i] <- "Herbivore-Detritivore" }

}

ggplot(dat_complet, aes(x=depth, y=log10(value),color=clean_diet))+
  geom_point(size=2, show.legend = TRUE)+
  scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
  geom_smooth()+theme_bw()+ ylim(0,4)+
  facet_wrap(~ island,nrow = 3)  +
  labs(x="Troph",y="log10(abu)")
