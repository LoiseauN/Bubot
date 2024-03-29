#

pkgs <- c('reshape2','mFD','viridis')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))
#Compute all indices
load(here::here("data/Data_dump/dat_complet.RData"))




#At the site scale

  #Biomass matrix
    biomass_mat = reshape2::melt(dat_complet , id.vars = c( "site" , "species" ) , measure.vars = "Groupweigth")
    biomass_mat = reshape2::dcast( biomass_mat , site~species,sum,na.rm=T)
    rownames(biomass_mat) <- biomass_mat[,1]
    biomass_mat <- biomass_mat[,-1]
    
    #Remove site with not enough species 
    preabs_mat <- biomass_mat
    preabs_mat[preabs_mat>0] <- 1
    preabs_mat <- preabs_mat[apply(preabs_mat,1,sum)>4,]
    preabs_mat <- preabs_mat[,apply(preabs_mat,2,sum)>0]
    biomass_mat <- biomass_mat[rownames(biomass_mat) %in% rownames(preabs_mat),]
    biomass_mat <- biomass_mat[,colnames(biomass_mat) %in% colnames(preabs_mat)]
    
    #Number at the genus and species and family lever
    abundance_mat = reshape2::melt(dat_complet , id.vars = c( "site" , "species" ) , measure.vars = "abundance")
    abundance_mat = reshape2::dcast( abundance_mat , site~species,sum,na.rm=T)
    rownames(abundance_mat) <- abundance_mat[,1]
    abundance_mat <- abundance_mat[,-1]
    abundance_mat <- abundance_mat[rownames(abundance_mat) %in% rownames(biomass_mat),]
    abundance_mat <- abundance_mat[,colnames(abundance_mat) %in% colnames(biomass_mat)]

#Traits matrix
    trait_mat <- dat_complet[,c("species","mobility","activity","schooling","position","clean_diet",
                             "maxLengthTL_Fishbase")]
    trait_mat <-unique(trait_mat)
    
    rownames(trait_mat)  <- trait_mat[,1]
    
    trait_mat  <- trait_mat[,-1]
    
    trait_mat <- trait_mat[rownames(trait_mat) %in% colnames(biomass_mat),]
    
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
      
      #Compute percentage explained
      pcoa_trdist <- ape::pcoa(sp_dist_traits)
      eigen <- pcoa_trdist$values$Eigenvalues
      #Keep positive eigenvalue
      eigen <- eigen[eigen>0]
  
      #Compute relative eigenvalue
      rel_eigen<- eigen/pcoa_trdist$trace
   
      pcoa_trdist$trace
      df <- data.frame(Eigenvalues = paste0('E',seq(1:8)),
                       Value = pcoa_trdist$values$Relative_eig[1:8])

      hist_eigen <- ggplot(data=df, aes(x=Eigenvalues, y=Value)) + geom_bar(stat="identity",fill="steelblue")+
        geom_text(aes(label=round(Value,digits=2)), vjust=1.6, color="white", size=3.5)+
        theme_minimal()
      
      
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
      
alpha_div <- merge(fd_ind_values,tax_ind_values,by = "row.names",
                   all= T )

rownames(alpha_div) <- alpha_div[,1]
alpha_div <- alpha_div[,-1]


alpha_div <- merge(alpha_div,hab_pc_site_scale,by = "row.names",
                       all.x= T)

rownames(alpha_div) <- alpha_div[,1]
alpha_div <- alpha_div[,-1]

save(alpha_div,file=here::here("results/alpha_div.RData"))

