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


abumat <-  subdat[,c("variable","value","Sample.code")]
abumat <-   as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= dat_complet))
coord  <- unique(dat_complet[,c("variable","PC1","PC2","PC3","PC4")])
rownames(coord) <- coord[,1]
coord<- coord[,-1]

#POOURQUOI ENCORE DES COORD AVEC NA, A TESTER
coord <- na.omit(coord)


abumat <- abumat[ncol(abumat) - sapply(1:nrow(abumat),function(x) sum(abumat[x,]%in%0))>4,]
abumat <- abumat[,apply(abumat,2,sum)>0]
abumat <- abumat[apply(abumat,1,sum)>0,]

abumat <- abumat[,colnames(abumat) %in% rownames(coord)]
coord <- coord[rownames(coord) %in% colnames(abumat),]

write.csv(abumat, file = "abumat.csv")
write.csv(coord, file = "coord.csv")

load_all("./")
alpha_div <- alpha.fd.multidim(sp_faxes_coord = coord, asb_sp_w =abumat[-c(130,144),],
                           scaling = TRUE, check.input = TRUE,
                           store_details = FALSE)

#Ligne 130 et 144 Comprendre erreur
#   Error in alpha.fd.multidim(sp_faxes_coord = coord, asb_sp_w = abumat,  : 
#                             Error: the sum of relative weights is not equal to one forMAEURUV011
                           
alpha_div$Abu    <- apply(abumat[-c(130,144),],1,sum)



