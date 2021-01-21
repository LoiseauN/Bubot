#Compute all indices
load("~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/data/Data_dump/dat_complet.RData")



# Script from mFD packages
#path <- "~/Documents/mFDpackages/lastversion/mFD_vers2/R"
# source all files containing the string 'Rex'
#source.all( path, ".R" )

devtools::load_all("~/Documents/mFDpackages/Git/mFD_shared/R")

devtools::load_all("~/Documents/Postdoc MARBEC/PACKAGE R - FDIV/Git/mFD_shared/R")

#At the site scale
dat_complet <- merge(dat_complet,  species.site.matrix$site.data[,c("Sample.name","Sample.code")],by.x="VideoID",by.y="Sample.name",all.x=T)

abumat <-  dat_complet[,c("variable","value","Sample.code")]
abumat <-   as.data.frame.matrix(xtabs(value ~ Sample.code + variable ,data= dat_complet))
coord  <- unique(dat_complet[,c("variable","PC1","PC2","PC3","PC4")])
rownames(coord) <- coord[,1]
coord<- coord[,-1]

#POOURQUOI ENCORE DES COORD AVEC NA, A TESTER
coord <- na.omit(coord)
abumat <- abumat[ncol(abumat) - sapply(1:nrow(abumat),function(x) sum(abumat[x,]%in%0))>4,]
abumat <- abumat[,apply(abumat,2,sum)>0]
abumat <- abumat[apply(abumat,1,sum)>0,]

abumat <- as.matrix(abumat[,colnames(abumat) %in% rownames(coord)])
coord <- as.matrix(coord[rownames(coord) %in% colnames(abumat),])


alpha_div <- alpha.fd.multidim(sp_faxes_coord = coord, asb_sp_w = abumat,
                           scaling = TRUE, check.input = TRUE,
                           store_details = FALSE)

alpha_div$abu <- apply(abumat,1,sum)
alpha_div <- merge(alpha_div,hab_pc_site_scale,by = "row.names",
                   all.x=T,all.y=F)
rownames(alpha_div) <- alpha_div[,1]
alpha_div <- alpha_div[,-1]

save(alpha_div,file="~/Documents/Postdoc MARBEC/BUBOT/Bubot Analyse/Bubot/results/alpha_div.RData")

ggplot(data = alpha_div, 
       aes(x = depth, y = fdis, color = depth)) +
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
       aes(x = depth, y = log10(abu), color = depth)) +
  geom_point(size=4)+scale_color_viridis(direction = -1)+theme_bw()+
  theme(axis.text.x = element_text(
    size=12),
    axis.text.y = element_text(
      size=12),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))



