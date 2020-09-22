#Compute all indices
load("~/Documents/Bubot/Bubot_Analyse/Bubot_Analyse/data/Data_dump/dat_complet.RData")
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
    
    abumat <-  subdat[,c("variable","value","Sample.code")]
    abumat <-   as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= abumat))
    abumat  <- abumat[,abumat>0]
    
    abumat <- abumat[,colnames(abumat) %in% rownames(subcoord)]
    subcoord <- subcoord[rownames(subcoord) %in% colnames(abumat),]
    
    abumat <- abumat[,order(rownames(subcoord))]
    
    Funct_div <- multidimFD(coord=as.matrix(subcoord), weight= as.matrix(abumat), check_species_pool=TRUE, verb=TRUE,
                            folder_plot=NULL, nm_asb_plot=NULL, Faxes_plot=NULL, Faxes_nm_plot=NULL, 
                            plot_pool=FALSE) 
    
    
    
  }
