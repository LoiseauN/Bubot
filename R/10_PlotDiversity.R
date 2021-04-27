
library(ggplot2)
library(reshape2)


df <- melt(alpha_div_all, id.vars="depth")

library(ggplot2)

data_plot <- df[df$variable %like% "hill", ]
ggplot(data_plot, aes(x=depth, y=value, fill=variable,color=variable)) + geom_line(stat='identity')


library(data.table)

mtcars[rownames(mtcars) %like% "Merc", ]



# Everything on the same plot
ggplot(d, aes(Xax,value, col=variable)) + 
  geom_point() + 
  stat_smooth() 

# Separate plots
ggplot(d, aes(Xax,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)




 ggplot(alpha_div_all, aes(x=depth, y=hill_phylo_richess)) + 
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
