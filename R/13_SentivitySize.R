
library(gridExtra)
sizeBUBOT_clean$Deep_shallow <- NA
for (i in 1:nrow(sizeBUBOT_clean)){
  if(sizeBUBOT_clean$depth[i]<30)  sizeBUBOT_clean$Deep_shallow[i] <- "shallow"
  if(sizeBUBOT_clean$depth[i]>30)  sizeBUBOT_clean$Deep_shallow[i] <- "deep"
}
  

ggplot(data=sizeBUBOT_clean, aes(x=depth,y=size))+
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(sizeBUBOT_clean$depth))+
  theme_bw()+ylab("Size")+xlab("")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")+
  ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
                                             after_stat(f.value.label),
                                             after_stat(p.value.label),
                                             sep = "*\", \"*")),
                          formula = y ~ x,
                          geom = "text", label.x = 75, label.y = 0, hjust = 1)+
  ggforce::facet_wrap_paginate(~ species,nrow = 5,ncol=5,  page = 5)

ggplot(data=sizeBUBOT_clean, aes(x=size,fill=Deep_shallow))+
  geom_density(alpha=0.4)+
  theme_bw()+ylab("Size")+xlab("")+
  theme(plot.title = element_text(hjust = 0.5))+
   facet_wrap(~ species,nrow = 13,ncol=10,scales = "free_y")




pl <- lapply(1:length(unique(sizeBUBOT_clean$species)), function(x) { 
  
  df <- subset(sizeBUBOT_clean,sizeBUBOT_clean$species==unique(sizeBUBOT_clean$species)[x])
  ggplot(data=df, aes(x=size,fill=Deep_shallow))+
    geom_density(alpha=0.4)+xlim(0,max(sizeBUBOT_clean$size))+
    theme_bw()+labs(title =unique(sizeBUBOT_clean$species)[x])+xlab("")+
    theme(plot.title = element_text(hjust = 0.5,size=5),
          legend.position = "none")
  
} )

plots <- marrangeGrob(pl, nrow = 5, ncol = 5)

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/fig/Sensi_size.pdf", 
       plot = plots, 
       width = 8, 
       height = 8, 
       units = "in",
       dpi=300)

library(plyr)
revalue(sizeBUBOT_clean$species, c("Acanthurus_nigrofuscus.or.Ctenochaetus_striatus.or.Ctenochaetus_binotatus" = "Ctenochaetus_striatus")) -> sizeBUBOT_clean$species


#Keep only measurement at the level of the species
sizeBUBOT_clean<- subset(sizeBUBOT_clean,!is.na(sizeBUBOT_clean$species))

pl <- lapply(1:length(unique(sizeBUBOT_clean$species)), function(x) { 
  
  df <- subset(sizeBUBOT_clean,sizeBUBOT_clean$species==unique(sizeBUBOT_clean$species)[x])
  
  #kolmo test 
  sub30 <- df[df$depth < 30,]
  over30<- df[df$depth > 30,]
  if (nrow(sub30) == 0 || nrow(over30) == 0){kolmo_res = NA}
  else {kolmo_res =   ks.test(sub30$size,over30$size)$p.value}

  kolmo_res <- data.frame(species = unique(sizeBUBOT_clean$species)[x],pvalue=kolmo_res,diff_size = mean(sub30$size)-mean(over30$size))
 return(kolmo_res)
  
  #ggplot(data=df, aes(x=depth,y=size))+
  #  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(sizeBUBOT_clean$depth))+
  #  ylim(0,max(sizeBUBOT_clean$size))+
  #  theme_bw()+labs(title =unique(sizeBUBOT_clean$species)[x])+xlab("")+
  #  theme(plot.title = element_text(hjust = 0.5,size=5))+
  #  geom_smooth(method = lm,formula = y ~ x,colour="orange",fill="orange")# +
    # ggpmisc::  stat_poly_eq(aes(label =  paste(after_stat(rr.label),
    #                                            after_stat(f.value.label),
    #                                          after_stat(p.value.label),
    #                                          sep = "*\", \"*")),
    #                       formula = y ~ x,
    #                       geom = "text", label.x = 75, label.y = 0, hjust = 1)
  
} )
plots <- marrangeGrob(pl, nrow = 5, ncol = 5)

ggsave(filename="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/fig/Sensi_size.pdf", 
       plot = plots, 
       width = 8, 
       height = 8, 
       units = "in",
       dpi=300)





ml <- marrangeGrob(pl, nrow=2, ncol=2)


do.call(grid.arrange, c(pl, ncol=4))






##### total biomass and density, recycling, storage
pl <- na.omit(do.call(rbind,pl))
pl$type <- rep("species",nrow(pl))
pl$signe <- NA
for (i in 1:nrow(pl)) { 
  if(pl$diff_size[i] > 0) {pl$signe[i] <- "positif"
  }else {pl$signe[i] <- "negatif" }
  }


 ggplot(pl, aes(x = pvalue, y = type, fill = factor(stat(quantile))) ) +
   ggridges::stat_density_ridges( geom = "density_ridges_gradient",
                        calc_ecdf = TRUE, quantiles = c( 0.28),from = 0, to = 1,
                        scale = 20) +
  geom_vline(xintercept = 0.05)+ylab("Density")+
   #geom_vline(xintercept=c(0.05), linetype="dotted")+
            scale_fill_manual(
              name = "p-value",
              values = c("#B35900FF","#D9AC82FF"),
              labels = c("significant", "non-significant"))+
    geom_point(aes(x = pvalue, y = jitter(c(rep(c(2.2,4.6),nrow(pl)/2),4.6),2),color = signe),
                size = 2,
                inherit.aes = FALSE)+
    scale_color_manual(
     name = "Difference size",
     values = c("#41A6D9FF","#006699FF"),
     labels = c("Higher in shallow reef", "Higher in deep reef"))+
    theme_bw()+
   theme(
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank())


 
 ### COMPARISON SIZE BUBOT AND SIZE RLS

 sizeBUBOT_mean <- sizeBUBOT %>% select(Species, Size) 
 sizeBUBOT_mean <- aggregate(. ~ Species, data = sizeBUBOT_mean, mean)
 
 
 sizeRLS_mean <- sizeRLS %>% select(species, size) 
 sizeRLS_mean <- aggregate(. ~ species, data = sizeRLS_mean, mean)
 
 comparison <- merge(sizeBUBOT_mean, sizeRLS_mean, by.x= "Species", by.y = "species")
 
 