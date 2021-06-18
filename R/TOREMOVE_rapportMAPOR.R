# AFTER SCRIPT FROM 1 to 4
# GRaph for important species



bohar <- subset(dat_complet,dat_complet$species== "Lutjanus_bohar")
gris <- subset(dat_complet,dat_complet$species== "Carcharhinus_amblyrhynchos")
marteau <- subset(dat_complet,dat_complet$species== "Sphyrna_lewini")
elasmo <- subset(dat_complet, dat_complet$class!="Actinopterygii") #17 obs, 30 inds
merou <- subset(dat_complet, dat_complet$family=="Serranidae") #17 obs, 30 inds
merou <- merou[!grepl("Pseudanthias", merou$species),]





bohar_biomass <- data_summary(bohar, varname="Groupweigth", 
                    groupnames=c("island", "classDepth"))
bohar_abundance <- data_summary(bohar, varname="abundance", 
                       groupnames=c("island", "classDepth"))

gris_biomass <- data_summary(gris, varname="Groupweigth", 
                              groupnames=c("island", "classDepth"))
gris_abundance <- data_summary(gris, varname="abundance", 
                                groupnames=c("island", "classDepth"))

marteau_biomass <- data_summary(marteau, varname="Groupweigth", 
                              groupnames=c("island", "classDepth"))
marteau_abundance <- data_summary(marteau, varname="abundance", 
                                groupnames=c("island", "classDepth"))

elasmo_biomass <- data_summary(elasmo, varname="Groupweigth", 
                              groupnames=c("island", "classDepth"))
elasmo_abundance <- data_summary(elasmo, varname="abundance", 
                                groupnames=c("island", "classDepth"))

merou_biomass <- data_summary(merou, varname="Groupweigth", 
                              groupnames=c("island", "classDepth"))
merou_abundance <- data_summary(merou, varname="abundance", 
                                groupnames=c("island", "classDepth"))

a <- ggplot(bohar_biomass, aes(x=classDepth, y=Groupweigth, group = island, color=island)) +  
  geom_line(aes(linetype=island)) + 
  geom_errorbar(aes(ymin=Groupweigth-sd, ymax=Groupweigth+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Lutjanus Bohar",x="Depth", y = "Biomass")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                  "#d1495b",
                                  "#edae49"))

b <- ggplot(bohar_abundance, aes(x=classDepth, y=abundance, group = island, color=island)) +  
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(aes(linetype=island)) + 
  geom_point(aes(shape=island))+
  labs(title="Lutjanus Bohar",x="Depth", y = "abundance")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

c<- ggplot(gris_biomass, aes(x=classDepth, y=Groupweigth, group = island, color=island)) +  
  geom_line(aes(linetype=island)) + 
  geom_errorbar(aes(ymin=Groupweigth-sd, ymax=Groupweigth+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Grey shark",x="Depth", y = "Biomass")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

d <- ggplot(gris_abundance, aes(x=classDepth, y=abundance, group = island, color=island)) +  
  geom_line(aes(linetype=island)) +
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Grey shark",x="Depth", y = "abundance")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))


e <- ggplot(marteau_biomass, aes(x=classDepth, y=Groupweigth, group = island, color=island)) +  
  geom_line(aes(linetype=island)) +
  geom_errorbar(aes(ymin=Groupweigth-sd, ymax=Groupweigth+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Hammerhead",x="Depth", y = "Biomass")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

f <- ggplot(marteau_abundance, aes(x=classDepth, y=abundance, group = island, color=island)) +  
  geom_line(aes(linetype=island)) + 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Hammerhead",x="Depth", y = "abundance")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

g<- ggplot(elasmo_biomass, aes(x=classDepth, y=Groupweigth, group = island, color=island)) +  
  geom_line(aes(linetype=island)) + 
  geom_errorbar(aes(ymin=Groupweigth-sd, ymax=Groupweigth+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Elasmobranchii",x="Depth", y = "Biomass")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

h <- ggplot(elasmo_abundance, aes(x=classDepth, y=abundance, group = island, color=island)) +  
  geom_line(aes(linetype=island)) + 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Elasmobranchii",x="Depth", y = "abundance")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

i<- ggplot(merou_biomass, aes(x=classDepth, y=Groupweigth, group = island, color=island)) +  
  geom_line(aes(linetype=island)) +
  geom_errorbar(aes(ymin=Groupweigth-sd, ymax=Groupweigth+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Grouper",x="Depth", y = "Biomass")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

j <- ggplot(merou_abundance, aes(x=classDepth, y=abundance, group = island, color=island)) +  
  geom_line(aes(linetype=island)) + 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_point(aes(shape=island))+
  labs(title="Grouper",x="Depth", y = "abundance")+
  theme_bw()+ scale_color_manual(values=c("#00798c",
                                          "#d1495b",
                                          "#edae49"))

grid.arrange(a,b,c,d,e,f,g,h,i,j,ncol=2)




abu_ind_values <- data.frame(abundance = apply(abundance_mat,1,sum))

alpha_div <- merge(alpha_div,abu_ind_values,by = "row.names",
                   all= T )

rownames(alpha_div) <- alpha_div[,1]
alpha_div <- alpha_div[,-1]



richness <- data_summary(alpha_div, varname="sp_richn", 
                              groupnames=c("island", "classDepth"))
fric <- data_summary(alpha_div, varname="fric", 
                                groupnames=c("island", "classDepth"))
biom <- data_summary(alpha_div, varname="biomass", 
                        groupnames=c("island", "classDepth"))
abundance <- data_summary(alpha_div, varname="abundance", 
                     groupnames=c("island", "classDepth"))
fori <- data_summary(alpha_div, varname="fori", 
                     groupnames=c("island", "classDepth"))

a <- ggplot(abundance, aes(x=classDepth, y=abundance, group = island, color=island))+ 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Abundance", x="Depth", y = "Abundance")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

b <- ggplot(biom, aes(x=classDepth, y=log10(biomass), group = island, color=island))+ 
  geom_errorbar(aes(ymin=log10(biomass-sd), ymax=log10(biomass+sd)), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Biomass", x="Depth", y = "log(Biomass)")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()



c <- ggplot(richness, aes(x=classDepth, y=sp_richn, group = island, color=island))+ 
  geom_errorbar(aes(ymin=sp_richn-sd, ymax=sp_richn+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Species richness", x="Depth", y = "Species richness")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()
 
 
d <- ggplot(fric, aes(x=classDepth, y=fric, group = island, color=island))+ 
   geom_errorbar(aes(ymin=fric-sd, ymax=fric+sd), width=.1, 
                 position=position_dodge(0.06)) +
   geom_line(size=1) + 
   geom_point(aes(shape=island))+
   labs(title= "Functional richness", x="Depth", y = "fric")+
   scale_color_manual(values=c("#00798c",
                               "#d1495b",
                               "#edae49"))+
   theme_bw()

grid.arrange(a,b,c,d,ncol=2)







################

Lethrinidae <- subset(dat_complet,dat_complet$family== "Lethrinidae")
Lethrinidae_biomass <- data_summary(Lethrinidae, varname="Groupweigth", 
                                    groupnames=c("island", "classDepth"))
Lethrinidae_abundance <- data_summary(Lethrinidae, varname="abundance", 
                                      groupnames=c("island", "classDepth"))

a <- ggplot(Lethrinidae_abundance, aes(x=classDepth, y=abundance, group = island, color=island))+ 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Lethrinidae", x="Depth", y = "Abundance")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

b <- ggplot(Lethrinidae_biomass, aes(x=classDepth, y=log10(Groupweigth), group = island, color=island))+ 
  geom_errorbar(aes(ymin=log10(Groupweigth-sd), ymax=log10(Groupweigth+sd)), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Lethrinidae", x="Depth", y = "log(Biomass)")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()


Lutjanidae <- subset(dat_complet,dat_complet$family== "Lutjanidae")
Lutjanidae_biomass <- data_summary(Lutjanidae, varname="Groupweigth", 
                                   groupnames=c("island", "classDepth"))
Lutjanidae_abundance <- data_summary(Lutjanidae, varname="abundance", 
                                     groupnames=c("island", "classDepth"))

c <- ggplot(Lutjanidae_abundance, aes(x=classDepth, y=abundance, group = island, color=island))+ 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Lutjanidae", x="Depth", y = "Abundance")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

d <- ggplot(Lutjanidae_biomass, aes(x=classDepth, y=log10(Groupweigth), group = island, color=island))+ 
  geom_errorbar(aes(ymin=log10(Groupweigth-sd), ymax=log10(Groupweigth+sd)), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Lutjanidae", x="Depth", y = "log(Biomass)")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()


Carangidae <- subset(dat_complet,dat_complet$family== "Carangidae")
Carangidae_biomass <- data_summary(Carangidae, varname="Groupweigth", 
                                   groupnames=c("island", "classDepth"))
Carangidae_abundance <- data_summary(Carangidae, varname="abundance", 
                                     groupnames=c("island", "classDepth"))


e <- ggplot(Carangidae_abundance, aes(x=classDepth, y=abundance, group = island, color=island))+ 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Carangidae", x="Depth", y = "Abundance")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

f <- ggplot(Carangidae_biomass, aes(x=classDepth, y=log10(Groupweigth), group = island, color=island))+ 
  geom_errorbar(aes(ymin=log10(Groupweigth-sd), ymax=log10(Groupweigth+sd)), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Carangidae", x="Depth", y = "log(Biomass)")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()


Serranidae <- subset(dat_complet,dat_complet$family== "Serranidae")
Serranidae_biomass <- data_summary(Serranidae, varname="Groupweigth", 
                                   groupnames=c("island", "classDepth"))
Serranidae_abundance <- data_summary(Serranidae, varname="abundance", 
                                     groupnames=c("island", "classDepth"))

g <- ggplot(Serranidae_abundance, aes(x=classDepth, y=abundance, group = island, color=island))+ 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Serranidae", x="Depth", y = "Abundance")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

h <- ggplot(Serranidae_biomass, aes(x=classDepth, y=log10(Groupweigth), group = island, color=island))+ 
  geom_errorbar(aes(ymin=log10(Groupweigth-sd), ymax=log10(Groupweigth+sd)), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Serranidae", x="Depth", y = "log(Biomass)")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()


Scombridae <- subset(dat_complet,dat_complet$family== "Scombridae")
Scombridae_biomass <- data_summary(Scombridae, varname="Groupweigth", 
                                   groupnames=c("island", "classDepth"))
Scombridae_abundance <- data_summary(Scombridae, varname="abundance", 
                                     groupnames=c("island", "classDepth"))


i <- ggplot(Scombridae_abundance, aes(x=classDepth, y=abundance, group = island, color=island))+ 
  geom_errorbar(aes(ymin=abundance-sd, ymax=abundance+sd), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Scombridae", x="Depth", y = "Abundance")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

j <- ggplot(Scombridae_biomass, aes(x=classDepth, y=log10(Groupweigth), group = island, color=island))+ 
  geom_errorbar(aes(ymin=log10(Groupweigth-sd), ymax=log10(Groupweigth+sd)), width=.1, 
                position=position_dodge(0.06)) +
  geom_line(size=1) + 
  geom_point(aes(shape=island))+
  labs(title= "Scombridae", x="Depth", y = "log(Biomass)")+
  scale_color_manual(values=c("#00798c",
                              "#d1495b",
                              "#edae49"))+
  theme_bw()

grid.arrange(a,b,c,d,e,f,g,h,i,j,ncol=2)
