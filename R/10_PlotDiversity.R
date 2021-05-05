pkgs <- c('reshape2','mFD','viridis','data.table','ggplot2')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# Plot alpha
ind <- c("sp_richn","fdis","fmpd","fnnd","feve","fric","fdiv","fori","fspe",
         "biomass","hill_taxo_entropy","hill_fonct_richess",
         "hill_fonct_entropy","hill_phylo_richess","hill_phylo_entropy")

df <- reshape2::melt(alpha_div_all, id.vars="depth")
df$value <- as.numeric(df$value)
df <- df[df$variable %in% ind,]

ggplot(df,aes(x = depth, y = value, color = variable )) +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()+
  stat_smooth()+
    theme(legend.position = "none")


a <- ggplot(alpha_div_all, aes(x=depth, y=sp_richn)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
   theme_bw()+ylab("Alpha-richness taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
  theme_bw()+ylab("Alpha-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
  theme_bw()+ylab("Alpha-richness fonct")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
  theme_bw()+ylab("Alpha-entropy fonct")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

e <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_phylo_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
  theme_bw()+ylab("Alpha-richness phylo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

f <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_phylo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div$depth))+
  theme_bw()+ylab("Alpha-entropy phylo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

title <- textGrob("Alpha Hill",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,e,f,ncol=2,top = title)

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

e <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_phylo_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

f <- ggplot(beta_hill, aes(x=diff_depth, y=beta_hill_phylo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(beta_hill$diff_depth))+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")


title <- textGrob("Beta Hill",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,e,f,ncol=2,top = title)


###PLot Distance decay
ResHill<- Decay_Hill_10toInfdepth
ResHill <- as.data.frame(ResHill)
ResHill <- merge(ResHill,From10toInfdepth, by="row.names",all.x=T)

a <- ggplot(ResHill, aes(x=depth, y=taxo_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-richness taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(ResHill, aes(x=depth, y=taxo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=taxo_entro_m-taxo_entro_sd, ymax=taxo_entro_m+taxo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-entropy taxo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(ResHill, aes(x=depth, y=fct_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_rich_m-fct_rich_sd, ymax=fct_rich_m+fct_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-richness fonct")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(ResHill, aes(x=depth, y=fct_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_entro_m-fct_entro_sd, ymax=fct_entro_m+fct_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-entropy fonct")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

e <- ggplot(ResHill, aes(x=depth, y=phylo_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=phylo_rich_m-phylo_rich_sd, ymax=phylo_rich_m+phylo_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-richness phylo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

f <- ggplot(ResHill, aes(x=depth, y=phylo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=phylo_entro_m-phylo_entro_sd, ymax=phylo_entro_m+phylo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-entropy phylo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")


title <- textGrob("Depth Decay",
                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,e,f,ncol=2,top = title)


alpha_div_all

ggplot(ResHill, aes(x=depth, y=phylo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=phylo_entro_m-phylo_entro_sd, ymax=phylo_entro_m+phylo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-entropy phylo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")