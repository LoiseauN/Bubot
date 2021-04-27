#

pkgs <- c('reshape2','mFD','viridis')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))
#Compute all indices
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")

#load("~/Documents/Bubot/Bubot_Analyse/Bubot/data/Data_dump/dat_complet.RData")




#At the site scale

  #Biomass matrix
    dat_complet_mayotte <- dat_complet[dat_complet$island=="Mayotte",]
    
    #Some very few family (37 individuals ) do not have any info
    dat_complet_mayotte <-  dat_complet_mayotte[dat_complet_mayotte$family %notin% c("Belonidae",
                                                                                     "Muraenidae",
                                                                                     "Nemipteridae",
                                                                                     "Pseudochromidae",
                                                                                     "Siganidae"),]
    
    biomass_mat = melt( dat_complet_mayotte , id.vars = c( "site" , "species" ) , measure.vars = "Groupweigth" )
    biomass_mat = dcast( biomass_mat , site~species,sum,na.rm=T)
    rownames(biomass_mat) <- biomass_mat[,1]
    biomass_mat <- biomass_mat[,-1]
    
    #Remove site with not enough species 
    preabs_mat <- biomass_mat
    preabs_mat[preabs_mat>0] <- 1
    preabs_mat <- preabs_mat[apply(preabs_mat,1,sum)>4,]
    preabs_mat <- preabs_mat[,apply(preabs_mat,2,sum)>0]
    biomass_mat <- biomass_mat[rownames(biomass_mat) %in% rownames(preabs_mat),]
    biomass_mat <- biomass_mat[,colnames(biomass_mat) %in% colnames(preabs_mat)]
    
#Traits matrix
    trait_mat <- dat_complet_mayotte[,c("species","mobility","activity","schooling","position","clean_diet",
                             "trophic.level","maxLengthTL_Fishbase")]
    trait_mat <-unique(trait_mat)
    
    rownames(trait_mat)  <- trait_mat[,1]
    
    trait_mat  <- trait_mat[,-1]
    
    trait_cat <- data.frame(trait_name = colnames(trait_mat),
                            trait_type = c("O","N","O","O","O","Q","Q")
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
      
      mFD::quality.fspaces.plot(fspaces_quality            = fspaces_quality,
                                quality_metric             = "mad",
                                fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", "pcoa_4d", "pcoa_5d", "pcoa_6d"),
                                name_file                  = NULL,
                                range_dist                 = NULL,
                                range_dev                  = NULL,
                                range_qdev                 = NULL,
                                gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
                                gradient_deviation_quality = c(low = "yellow", high = "red"),
                                x_lab                      = "Trait-based distance")
      
      #Correlation Traits PCOA
      sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"
      tr_faxes <- mFD::traits.faxes.cor(sp_tr = trait_mat, 
                                               sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")], plot = TRUE)
      
      tr_faxes$tr_faxes_plot
      
      #Plot functional space
      mFD::funct.space.plot(
        sp_faxes_coord    = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
        faxes             = NULL,
        name_file         = NULL,
        faxes_nm          = NULL,
        range_faxes       = c(NA, NA),
        color_bg          = "grey95",
        color_pool          = "darkturquoise",
        fill_pool           = "white",
        shape_pool          = 21,
        size_pool           = 1,
        plot_ch           = TRUE,
        color_ch          = "darkblue",
        fill_ch           = "white",
        alpha_ch          = 1,
        plot_vertices     = TRUE,
        color_vert        = "darkturquoise",
        fill_vert         = "darkturquoise",
        shape_vert        = 22,
        size_vert         = 1,
        plot_sp_nm         = NULL,
        nm_size            = 3,
        nm_color           = "black",
        nm_fontface        = "plain",
        check_input        = TRUE)
      
      
      #Compute Functional diversity
      alpha_fd_indices <- mFD::alpha.fd.multidim(sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
                                                        asb_sp_w      = as.matrix(biomass_mat),
                                                        ind_vect      = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", "fspe"),
                                                        scaling       = TRUE,
                                                        check_input   = TRUE,
                                                        details_returned = TRUE)
      
      
      fd_ind_values <- alpha_fd_indices$"functional_diversity_indices"
      fd_ind_values
      
      
      tax_ind_values <- data.frame(biomass = apply(biomass_mat,1,sum))
      
alpha_div_all <- merge(fd_ind_values,tax_ind_values,by = "row.names",
                   all= T )

rownames(alpha_div_all) <- alpha_div_all[,1]
alpha_div_all <- alpha_div_all[,-1]


alpha_div_all <- merge(alpha_div_all,hab_pc_site_scale,by = "row.names",
                       all.X= T )

rownames(alpha_div_all) <- alpha_div_all[,1]
alpha_div <- alpha_div_all[,-1]

save(alpha_div,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/alpha_div.RData")

ggplot(data = alpha_div, 
       aes(x = depth, y = PC1, color = depth)) +
  geom_point(size=4)+scale_color_viridis(direction = -1)+theme_bw()+
  theme(axis.text.x = element_text(
    size=12),
    axis.text.y = element_text(
      size=12),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))


ggplot(data = alpha_div, 
       aes(x = depth, y = sp_richn, color = depth)) +
  geom_point(size=4)+scale_color_viridis(direction = -1)+theme_bw()+
  theme(axis.text.x = element_text(
    size=12),
    axis.text.y = element_text(
      size=12),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))


ggplot(data = alpha_div, 
       aes(x = depth, y = log10(biomass), color = depth)) +
  geom_point(size=4)+scale_color_viridis(direction = -1)+theme_bw()+
  theme(axis.text.x = element_text(
    size=12),
    axis.text.y = element_text(
      size=12),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

