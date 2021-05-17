
##Supplementary




a <- ggplot(alpha_div_all, aes(x=classDepth, y=sp_richn)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-richness taxo")+ geom_jitter(width = 0.1, size = 0.5)

b <- ggplot(alpha_div_all, aes(x=classDepth, y=alpha_hill_taxo_entropy)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-entropy taxo")+ geom_jitter(width = 0.1, size = 0.5)

c <- ggplot(alpha_div_all, aes(x=classDepth, y=alpha_hill_fonct_richess)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-richness fonct")+ geom_jitter(width = 0.1, size = 0.5)

d <- ggplot(alpha_div_all, aes(x=classDepth, y=alpha_hill_fonct_entropy)) + 
  geom_boxplot(fill ="cadetblue3",pch=21)+
  theme_bw()+ylab("Alpha-entropy fonct")+ geom_jitter(width = 0.1, size = 0.5)

title <- textGrob("Alpha Hill boxplot",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2,top = title)



###PLot Beta

a <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_taxo_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

title <- textGrob("Beta Hill",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2,top = title)

