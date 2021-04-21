load(file.path("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData"))

#Working on Mayotte only
dat_complet_mayotte<- dat_complet[dat_complet$island=="Mayotte",]

var <- c("reef_associated","mobility","activity","schooling","position","diet","clean_diet",
         "trophic.level","bodyShape_Fishbase","maxLengthTL_Fishbase")

for(j in 1:length(var)){
  
  main.plot <- ggplot(dat_complet_mayotte, aes(x=depth, y=log10(Groupweigth),color=dat_complet_mayotte[,var[j]]))+
    geom_point(size=2, show.legend = TRUE)+
    scale_color_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
    #scale_color_hp(discrete = TRUE, option = "LunaLovegood", name = "Depth",direction = -1) +
    geom_smooth(method = "loess")+
    theme_bw()+ 
    labs(colour = var[j]) + 
    theme(legend.position = "right")+
    ylim(0,4)+
    labs(x="Depth",y="biomassdance (log)")+
    geom_vline(xintercept=20,  linetype="dotted",size=1)+
    geom_vline(xintercept=40,  linetype="dotted",size=1)+
    geom_vline(xintercept=60,  linetype="dotted",size=1)+
    geom_vline(xintercept=80,  linetype="dotted",size=1)
  
  #+
  #geom_label(label="0-20m", x=8, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="20-40m",x=30, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="40-60m", x=52, y=4, size=3, hjust=0,color="black")+
  #geom_label(label="60-80m", x=74, y=4, size=3, hjust=0,color="black")+
  #geom_label(label=">80m", x=10, y=4, size=3, hjust=0,color="black")
  
  biomass_classDepth <- dat_complet_mayotte[,c("Groupweigth","classDepth",var[j])]
  colnames(biomass_classDepth)[3] <- "trait"
  biomass_classDepth <- aggregate(. ~ classDepth + trait, data = biomass_classDepth, sum)
  
  ## pyramid charts are two barcharts with axes flipped
  biomass_classDepth<- with(biomass_classDepth, biomass_classDepth[order(trait,classDepth),])
  #("Herbivore-Detritivore","Omnivore","Planktivore","Invertivore","Piscivore"))
  
  for (i in 1:length(unique(biomass_classDepth$classDepth))){
    print(i)
    sub <- subset(biomass_classDepth , biomass_classDepth$classDepth==unique(biomass_classDepth$classDepth)[i])
    sub$perc <- round((sub$Groupweigth/sum(sub$Groupweigth))*100,3)
    sub$alpha <- sub$perc/2
    
    #Add modality that are absent with 0
    if(length(sub$trait %in% unique(biomass_classDepth$trait)) < length(unique(biomass_classDepth$trait))){
      notin <- biomass_classDepth$trait
      notin <- data.frame(unique(notin[notin %notin% sub$trait]))
      notin <- data.frame(classDepth=rep(unique(biomass_classDepth$classDepth)[i],nrow(notin)),
                          trait=notin[,1],
                          Groupweigth=rep(0,nrow(notin)),
                          perc=rep(0,nrow(notin)),
                          alpha=rep(0,nrow(notin)))
      sub<-rbind(sub,notin)
    }
    
    
    sub2 <- rbind(sub,sub) 
    sub2$alpha[c((nrow(sub)+1):nrow(sub2))] <- -sub2$alpha[c((nrow(sub)+1):nrow(sub2))] 
    sub2$direction <-c(rep("pos",nrow(sub)),rep("neg",nrow(sub)))
    
    
    assign(paste0("inset.plot",i), ggplot(sub2, aes(x = trait, y = alpha, fill = trait)) + 
             geom_bar(data = subset(sub2, direction == "pos"), stat = "identity", width=1) + 
             geom_bar(data = subset(sub2, direction == "neg"), stat = "identity", width=1) + 
             scale_fill_manual(values=c("chartreuse3","gold","blue","red","brown4","gray46","black"))+
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
               legend.background = element_rect(fill = "transparent")))
  }
  #main.plot<- 
  if (j==1 || j==5 || j==6){ 
    assign(paste0("main.plot",j), ggdraw() +
             draw_plot(main.plot) +
             draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot2, x = 0.20, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot3, x = 0.35, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot4, x = 0.52, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot5, x = 0.72, y = 0.81, width = 0.12, height = 0.12))
  }
  
  if (j==2){ 
    assign(paste0("main.plot",j), ggdraw() +
             draw_plot(main.plot) +
             draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot2, x = 0.19, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot3, x = 0.35, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot4, x = 0.51, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot5, x = 0.7, y = 0.81, width = 0.12, height = 0.12)
    )}
  
  if (j==3){ 
    assign(paste0("main.plot",j), ggdraw() +
             draw_plot(main.plot) +
             draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot2, x = 0.195, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot3, x = 0.35, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot4, x = 0.51, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot5, x = 0.71, y = 0.81, width = 0.12, height = 0.12)
    )}
  
  if (j==4 || j==7 || j==8 || j==9 || j==10){ 
    assign(paste0("main.plot",j), ggdraw() +
             draw_plot(main.plot) +
             draw_plot(inset.plot1, x = 0.04, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot2, x = 0.17, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot3, x = 0.32, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot4, x = 0.46, y = 0.81, width = 0.12, height = 0.12)+
             draw_plot(inset.plot5, x = 0.65, y = 0.81, width = 0.12, height = 0.12)
    )}
  
}



ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Activity.png", 
       plot = main.plot1, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Schooling.png", 
       plot = main.plot2, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Position.png", 
       plot = main.plot3, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/clean_diet.png", 
       plot = main.plot4, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Size.png", 
       plot = main.plot5, 
       width = 297, 
       height = 210, 
       units = "mm")

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/fig/Diet.png", 
       plot = main.plot6, 
       width = 297, 
       height = 210, 
       units = "mm")


#Functional space
# PROBLEM SUR DES PC A 0,0, 0
# cluster_core = 1 === singleton
plotPCOA <- dat_complet_mayotte[,c("FE","value","classDepth","PC1","PC2")]
plotPCOA <- aggregate(. ~ classDepth + FE, data = plotPCOA, sum)

plotPCOA_1 <- merge(aggregate(value ~ FE + classDepth, plotPCOA, sum), 
                    aggregate(PC1 ~ FE , plotPCOA, mean),by="FE")

plotPCOA <- merge(plotPCOA_1, 
                  aggregate(PC2 ~ FE , plotPCOA, mean),by="FE")

FunctSpace<- ggplot(plotPCOA, aes(x=PC1, y=PC2)) + 
  geom_point(aes(size=log10(value+1),colour= classDepth,alpha=0.4))+ #
  scale_shape_manual(values=c(4, 16))+
  geom_encircle(aes(colour= classDepth),s_shape = 1, expand = 0,size=3,
                alpha = 0.7, show.legend = FALSE)+
  theme_bw()+labs(x = "PCOA 1")+labs(y = "PCOA 2") +
  facet_wrap(~ classDepth,ncol = 6)  +
  scale_colour_hp_d(option = "LunaLovegood",direction = 1)+
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none",
        #axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks = element_blank()
  )

plotPCOA_2 <- dat_complet_mayotte[,c("PC1","PC2")]
colnames(plotPCOA_2)<-c("axis1","axis2")
FunctSpace + geom_point(data = plotPCOA_2, aes(x = axis1, y = axis2),size=1,color="grey",alpha=0.2) +
  ggpubr::stat_chull(data = plotPCOA_2,aes(x = axis1, y = axis2), 
                     alpha = 0.1, geom = "polygon")
