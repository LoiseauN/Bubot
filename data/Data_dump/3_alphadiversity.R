#Compute all indices

#At the video scale 

alpha.fd.multidim <- function(sp_coord, asb_sp_w,
                              ind_vect = c("fdis", "fmpd", "fnnd", "feve",
                                           "fric", "fdiv", "fori", "fspe"),
                              scaling = TRUE, check.input = TRUE,
                              store_details = TRUE)
  

#At the site scale
for (i in 1:length(unique(dat_complet$Sample.code))){
  #i=20
  subdat <- dat_complet[dat_complet$Sample.code==unique(dat_complet$Sample.code)[i],]
  subcoord <- unique(subdat[,c("variable","PC1","PC2","PC3","PC4")])
  rownames(subcoord)<- subcoord[,1]
  subcoord<- subcoord[,-1]
  
  
  abu  <- sum(subdat$value)
  S    <- length(unique(subdat$variable))

  abumat <-  subdat[,c("variable","value")]
  as.data.frame.matrix(xtabs(value~variable,data= abumat))
  Funct_div <- alpha.fd.multidim(sp_coord = subcoord, asb_sp_w=abumat)
  
  Funct_div <- multidimFD(coord=subcoord, weight= abumat, check_species_pool=TRUE, verb=TRUE,
                       folder_plot=NULL, nm_asb_plot=NULL, Faxes_plot=NULL, Faxes_nm_plot=NULL, 
                       plot_pool=FALSE) 
    
    
  
}

