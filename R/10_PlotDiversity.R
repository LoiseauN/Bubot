pkgs <- c('reshape2','mFD','viridis','data.table','ggplot2','grid')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


#Some values


sum(abundance_mat[,colnames(abundance_mat) %in% dat_complet$genus])
sum(abundance_mat[,colnames(abundance_mat) %in% dat_complet$family])

dim(abundance_mat[,colnames(abundance_mat) %in% dat_complet$genus])
dim(abundance_mat[,colnames(abundance_mat) %in% dat_complet$family])

# Number per detph class
summa<- matrix(NA,4,4)
rownames(summa) <- unique(alpha_div_all$classDepth)
colnames(summa) <- c("mean","sd","max","min")
for (i in 1:length(unique(alpha_div_all$classDepth))){
  summa[i,1] <- mean(subset(alpha_div_all,alpha_div_all$classDepth == unique(alpha_div_all$classDepth)[i])$sp_richn)
  summa[i,2] <- sd(subset(alpha_div_all,alpha_div_all$classDepth == unique(alpha_div_all$classDepth)[i])$sp_richn)
  summa[i,3] <- max(subset(alpha_div_all,alpha_div_all$classDepth == unique(alpha_div_all$classDepth)[i])$sp_richn)
  summa[i,4] <- min(subset(alpha_div_all,alpha_div_all$classDepth == unique(alpha_div_all$classDepth)[i])$sp_richn)

  }



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


biom_plot <- ggplot(alpha_div_all, aes(x=depth, y=log10(biomass))) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_all$depth))+
  theme_bw()+ylab("Biomass")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")


a <- ggplot(alpha_div_all, aes(x=depth, y=sp_richn)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_all$depth))+
  theme_bw()+ylab("Taxonomic")+xlab("")+ggtitle("Richness")
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_taxo_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_all$depth))+
  theme_bw()+ylab(" ")+xlab(" ")+ggtitle("Structure")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_fonct_richess)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_all$depth))+
  theme_bw()+ylab("Functional")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(alpha_div_all, aes(x=depth, y=alpha_hill_fonct_entropy)) + 
  geom_point(fill ="cadetblue3",pch=21)+xlim(0,max(alpha_div_all$depth))+
  theme_bw()+ylab("")+xlab("Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

#title <- textGrob("Alpha Hill",
#                  gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2)#,top = title)




###PLot Distance decay
ResHill<- Decay_Hill_10toInfdepth
ResHill <- as.data.frame(ResHill)
ResHill <- merge(ResHill,From10toInfdepth, by="row.names",all.x=T)

a <- ggplot(ResHill, aes(x=depth, y=taxo_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=taxo_rich_m-taxo_rich_sd, ymax=taxo_rich_m+taxo_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Taxonomic")+xlab("")+ggtitle("Dissimiliarity composition")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

b <- ggplot(ResHill, aes(x=depth, y=taxo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=taxo_entro_m-taxo_entro_sd, ymax=taxo_entro_m+taxo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab(" ")+xlab(" ")+ggtitle("Dissimiliarity structure")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

c <- ggplot(ResHill, aes(x=depth, y=fct_rich_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_rich_m-fct_rich_sd, ymax=fct_rich_m+fct_rich_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Functional")+xlab("Difference Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

d <- ggplot(ResHill, aes(x=depth, y=fct_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=fct_entro_m-fct_entro_sd, ymax=fct_entro_m+fct_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("")+xlab("Difference Depth (m)")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")

#title <- textGrob("Depth Decay",
 #                 gp=gpar(fontsize=20,fontface=2))
grid.arrange(a,b,c,d,ncol=2)#,top = title)


ggplot(ResHill, aes(x=depth, y=phylo_entro_m)) + 
  geom_point(fill ="cadetblue3",pch=21)+ylim(0,1)+xlim(0,max(alpha_div$depth))+
  geom_errorbar(aes(ymin=phylo_entro_m-phylo_entro_sd, ymax=phylo_entro_m+phylo_entro_sd), width=.2,
                position=position_dodge(0.05),color ="cadetblue3")+
  theme_bw()+ylab("Beta-entropy phylo")+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 2),colour="orange",fill="orange")






###Plot Functional Space
biomass_mat_classDepth = reshape2::melt(dat_complet , id.vars = c( "classDepth" , "species" ) , measure.vars = "Groupweigth")
biomass_mat_classDepth = reshape2::dcast( biomass_mat_classDepth , classDepth~species,sum,na.rm=T)
rownames(biomass_mat_classDepth) <- biomass_mat_classDepth[,1]
biomass_mat_classDepth <- biomass_mat_classDepth[,-1]

#Remove site with not enough species 
preabs_mat_classDepth <- biomass_mat_classDepth
preabs_mat_classDepth[preabs_mat_classDepth>0] <- 1
preabs_mat_classDepth <- preabs_mat_classDepth[apply(preabs_mat_classDepth,1,sum)>4,]
preabs_mat_classDepth <- preabs_mat_classDepth[,apply(preabs_mat_classDepth,2,sum)>0]
biomass_mat_classDepth <- biomass_mat_classDepth[rownames(biomass_mat_classDepth) %in% rownames(preabs_mat_classDepth),]
biomass_mat_classDepth <- biomass_mat_classDepth[,colnames(biomass_mat_classDepth) %in% colnames(preabs_mat_classDepth)]

#Traits matrix
trait_mat <- dat_complet[,c("species","mobility","activity","schooling","position","clean_diet",
                            "maxLengthTL_Fishbase")]
trait_mat <-unique(trait_mat)

rownames(trait_mat)  <- trait_mat[,1]

trait_mat  <- trait_mat[,-1]

trait_mat <- trait_mat[rownames(trait_mat) %in% colnames(biomass_mat_classDepth),]

trait_cat <- data.frame(trait_name = colnames(trait_mat),
                        trait_type = c("O","N","O","O","O","Q")
)

traits_summary <- mFD::sp.tr.summary(tr_cat = trait_cat,    # Traits informations
                                     sp_tr  = trait_mat, stop_if_NA = TRUE)
#Traits distance     
sp_dist_traits <-  mFD::funct.dist(sp_tr         = trait_mat,
                                   tr_cat        = trait_cat,
                                   metric        = "gower",
                                   scale_euclid  = "scale_center",
                                   ordinal_var   = "classic",
                                   weight_type   = "equal",
                                   stop_if_NA    = TRUE)

#Traits space quality    #4 dimension
fspaces_quality <- mFD::quality.fspaces(sp_dist             = sp_dist_traits,
                                        maxdim_pcoa         = 10,
                                        deviation_weighting = "absolute",
                                        fdist_scaling       = FALSE,
                                        fdendro             = "average")

#Correlation Traits PCOA
sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"
tr_faxes <- mFD::traits.faxes.cor(sp_tr = trait_mat, 
                                  sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")], plot = TRUE)

tr_faxes$tr_faxes_plot


#Compute Functional diversity
rownames(biomass_mat_classDepth) <- c("0-20m","20-40m","40-60m",
                                      ">60m")
alpha_fd_indices <- mFD::alpha.fd.multidim(sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
                                           asb_sp_w      = as.matrix(biomass_mat_classDepth),
                                           ind_vect      = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", "fspe"),
                                           scaling       = TRUE,
                                           check_input   = TRUE,
                                           details_returned = TRUE)


# Seb illustrating FD with new mFD package
# link (not to share at this point, software paper submitted to Ecography)
# https://cmlmagneville.github.io/mFD/
# 48 warning messages not a problem (points not drawn)


# libraries ---
library(tidyverse)
library(mFD)


# color code for depth ----

#hab_depth<-c("[0-20[","[20-40[","[40-60[",
    #         ">60")

hab_depth<-c("0-20m","20-40m","40-60m",
             ">60m")

vcolors <- col2rgb(c("cyan2","deepskyblue",
                     "deepskyblue3","dodgerblue4"))


vcolors <- apply(vcolors, 2, function(x) {
  rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
})



names(vcolors)<-hab_depth

# coordinates of all species
pool_coord<-alpha_fd_indices$details$sp_faxes_coord

# vertices of all fe in 4D ----
pool_vert_nm<-alpha_fd_indices$details$pool_vert_nm

# range of axes
range_faxes_coord <- range(pool_coord[,1:4])
range_axes <- range_faxes_coord +
  c(-1, 1) * (range_faxes_coord[2] - range_faxes_coord[1]) * 0.1

# indices values
hab_depth_fd<-alpha_fd_indices$functional_diversity_indices


## plotting along pairs of axes ####

pairs_axes<-list(c(1,2))#, c(3,4))

for (z in 1:length(pairs_axes))
{
  
  # names of axes   
  xy<-pairs_axes[[z]]
  
  # list to store ggplot
  ggplot_list<-list()
  
  
  for (v in hab_depth ) {
    # v="[0-20["
    
    # color for habitat*pH levels
    col_v<-as.character(vcolors[v])
    
    # species present in v
    sp_v<-names(which(alpha_fd_indices$details$asb_sp_occ[v,]==1))
    
    # background with axes range set + title
    ggplot_v<-background.plot(range_faxes=range_axes,
                              faxes_nm=paste0("PC", xy), 
                              color_bg="white")
    ggplot_v<-ggplot_v + labs(subtitle=v)
    
    # convex hull of species pool
    ggplot_v<-pool.plot(ggplot_bg=ggplot_v,
                        sp_coord2D=pool_coord[,xy],
                        vertices_nD=pool_vert_nm,
                        plot_pool=FALSE,
                        color_ch=NA, fill_ch="gray90", alpha_ch=1
    )
    
    # plot convex hull of assemblage but not species
    ggplot_v<-fric.plot( ggplot_bg=ggplot_v, 
                         asb_sp_coord2D=list(vv=pool_coord[sp_v,xy]),
                         asb_vertices_nD=list(vv=alpha_fd_indices$details$asb_vert_nm[[v]]),
                         plot_sp = FALSE,
                         color_ch=c(vv=col_v),
                         fill_ch=c(vv=col_v),
                         alpha_ch=c(vv=0.1)
    )
    
    
    # plot species weights using plot.fide without showing mean value
    ggplot_v<- fide.plot(ggplot_bg=ggplot_v,
                         asb_sp_coord2D=list(vv=pool_coord[sp_v,xy]),
                         asb_sp_relatw=list(vv=alpha_fd_indices$details$asb_sp_relatw[v,sp_v]),
                         asb_fide_coord2D=list(vv=hab_depth_fd[v,paste0("fide_PC", xy)]),
                         plot_sp = TRUE,
                         shape_sp = c(vv=21),
                         color_sp =c(vv=col_v),
                         fill_sp = c(vv=paste0(col_v,"70") ),
                         shape_fide = c(vv=23),
                         size_fide = c(vv=1),
                         color_fide = c(vv=col_v),
                         fill_fide = c(vv=col_v),
                         color_segment = c(vv=col_v),
                         width_segment = c(vv=0.5),
                         linetype_segment = c(vv=1)
    )
    
    
    # ggplot_v storing in list
    ggplot_list[[v]]<-ggplot_v
    
  }# end of v
  
  
  # patchwork of plots : 2 habitats (rows) * 2 columns (pH)
  FD_xy<- (ggplot_list[[1]] + ggplot_list[[2]] ) / 
    (ggplot_list[[3]]+  ggplot_list[[4]] )
  
  
  # saving as png ----
  sz<-3
  #ggsave(FD_xy, filename = paste0("FD_pc",xy[1],"vs",xy[2],".png"), 
  #      device = "png", width=3*sz, height=4*sz)
  
  
}# end of axes_id
FD_xy




