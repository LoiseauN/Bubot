#Compute all indices
load("~/Documents/Bubot/Bubot_Analyse/Bubot_Analyse/data/Data_dump/dat_complet.RData")
#At the video scale 

alpha.fd.multidim <- function(sp_coord, asb_sp_w,
                              ind_vect = c("fdis", "fmpd", "fnnd", "feve",
                                           "fric", "fdiv", "fori", "fspe"),
                              scaling = TRUE, check.input = TRUE,
                              store_details = TRUE)
  
  
  #At the site scale
  dat_complet <- merge(dat_complet,  species.site.matrix$site.data[,c("Sample.name","Sample.code")],by.x="VideoID",by.y="Sample.name",all.x=T)

alpha_div <- matrix(NA,nrow=length(unique(dat_complet$Sample.code)),ncol=8)

  for (i in 1:length(unique(dat_complet$Sample.code))){
    print(i)
    #i=20
    subdat <- dat_complet[dat_complet$Sample.code==unique(dat_complet$Sample.code)[i],]
    if(nrow(subdat)<2){next}
    
    subcoord <- unique(subdat[,c("variable","PC1","PC2","PC3","PC4")])
    rownames(subcoord)<- subcoord[,1]
    subcoord<- subcoord[,-1]
    subcoord<- na.omit(subcoord)
    
    abu  <- sum(subdat$value)
    S    <- length(unique(subdat$variable))
    
    abumat <-  subdat[,c("variable","value","Sample.code")]
    abumat <-   as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= abumat))
    abumat  <- abumat[,abumat>0]
    
    abumat <- abumat[,colnames(abumat) %in% rownames(subcoord)]
    subcoord <- subcoord[rownames(subcoord) %in% colnames(abumat),]
    
    subcoord <- subcoord[sort(row.names(subcoord)),]
    abumat <- abumat[sort(names(abumat))]

    result<- tryCatch(Funct_div <- multidimFD(coord=as.matrix(subcoord), weight= as.matrix(abumat), check_species_pool=TRUE, verb=TRUE,
                            folder_plot=NULL, nm_asb_plot=NULL, Faxes_plot=NULL, Faxes_nm_plot=NULL, 
                            plot_pool=FALSE),
                          error=function(err){result="NA"}
    )
    
    if((is(result)[1]=="character")=="TRUE") {
    
    alpha_div[i,1] <- abu
    alpha_div[i,2] <- S
    alpha_div[i,3] <- NA
    alpha_div[i,4] <- NA
    alpha_div[i,5] <- NA
    alpha_div[i,6] <- NA
    alpha_div[i,7] <- NA
    alpha_div[i,8] <- NA
    }
    else{
      alpha_div[i,1] <- abu
      alpha_div[i,2] <- S
      alpha_div[i,3] <- Funct_div[19]
      alpha_div[i,4] <- Funct_div[20]
      alpha_div[i,5] <- Funct_div[21]
      alpha_div[i,6] <- Funct_div[22]
      alpha_div[i,7] <- Funct_div[23]
      alpha_div[i,8] <- Funct_div[24]
    }
  
    
    }
