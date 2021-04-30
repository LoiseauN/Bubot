#####################################################################################################
#####################################################################################################
################ here are three functions to recover and prepare data for analyses ##################
#####################################################################################################
#####################################################################################################





#####################################################################################################
######################################## function 1 #################################################
#####################################################################################################



#######################################################################################################
######### This is a function to create a vector of Event code to select in RUV_data table #############
#######################################################################################################
#                                                                                                     #
#                                     FUNCTION NAME : "Event.list"                                    #
#                                                                                                     #
#                                                                                                     #
# This function input are the technique (tool) used to analyse video which are webfish and wave       #
#    ex : measurement.method=c("webfish","wave") the default is webfish and wave, for now this        #
#         this input should stay as it is or changed as c("vidsync") but do not mix the two           #
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
# A second input is the island selection in this case Mayotte, Europa and Juan de Nova                #
#    ex : island=c("Mayotte","Europa","Juan_de_Nova") any of the island can be removed or added       #
#         the default is all the island are loaded                                                    #
#                                                                                                     #
#                                                                                                     #
# Finally it is requested to give a working directory in which a Data_dump folder will automatically  #
# be created to store localy database table to be able to work off line                               #
#    ex: work.folder="D:/Dropbox/work/research_projects/MAPOR/R_analyses" be carefull, the / might    #
#        need to be changed under linux I'm not sure                                                  #
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
# There is also an option to connect to the database or work only locally (faster). But the first     #
# running of the code need to be done while connected to internet                                     #
#     ex: input : DB.connection="yes" will allow an internet connection, anything else than "yes"     #
#         will force the local working only. The default is a connection ("yes")                      #
#                                                                                                     #
#                                                                                                     #
# The output of that function is just a vector of event numbers that will allow to select obervations #
# in the original database (RUV_data table)                                                           #
#                                                                                                     #
#                                                                                                     #
#######################################################################################################
#######################################################################################################





# create a function to return event code when given measurement method and island
Event.list=function(DB.connection="yes",
                    work.folder,measurement.method=c("webfish","wave"),
                    island=c("Mayotte","Europa","Juan_de_Nova")) {
  
  if(DB.connection=="yes") {
    # verify that required packages exist, and install them if not
    if (!require(DBI)) install.packages('DBI')
    if (!require(RMySQL)) install.packages('RMySQL')
    # open required packages
    library(DBI)
    library(RMySQL)
    
    # load MySQL drivers
    drv=dbDriver("MySQL")
    
    # get connection to the database
    con = dbConnect(drv, user="UVC.reader", dbname="Underwater Visual Census",password="Mayfish976",host="162.38.198.139")
    
    # load the tables
    RUV_data=dbReadTable(con, "RUV_data")
    Sampling_type=dbReadTable(con, "Sampling_type")
    
    # set working directory
    setwd(work.folder)
    # create a data_dump folder
    dir.create("Data_dump",showWarnings = F)
    
    # save the data locally in case of offline work
    setwd(paste(work.folder,"/Data_dump",sep=""))
    # write all the tables
    write.table(Sampling_type,"Sampling_type.txt",sep=";")
    write.table(RUV_data,"RUV_data.txt",sep=";")
    
  } # close the if condition
  
  # set the dump directory
  setwd(paste(work.folder,"/Data_dump",sep=""))
  
  
  # reopen these files
  Sampling_type=read.table("Sampling_type.txt",sep=";",header = T)
  RUV_data=read.table("RUV_data.txt",sep=";",header = T)
  
  # get back to the original working directory
  setwd(work.folder)
  
  #############################################################################
  
  ######### find sampling code that correspond to the island ############
  isl.cor=cbind(c("Mayotte","Europa","Juan_de_Nova"),
                code=c("YT","FR_europa","FR_juan_de_nova"))
  
  # find the country code to select
  isl.select=isl.cor[match(island,isl.cor[,1]),2]
  
  # find the corresponding sampling codes
  Sampling_codes=as.character(Sampling_type$X.sampling_code[!is.na(match(Sampling_type$country,isl.select))])
  
  # get the measurement event corresponding to these sampling code and measurement method
  
  selected.event=RUV_data$Event[!is.na(match(as.character(RUV_data$Sampling_code),Sampling_codes))&
                                  !is.na(match(as.character(RUV_data$Treatment_type),measurement.method))]
  
  selected.event
}


################################################################################################################






#####################################################################################################
######################################## function 2 #################################################
#####################################################################################################



#######################################################################################################
################ This is a function to collect max N per species per video ############################
#######################################################################################################
#                                                                                                     #
#                                     FUNCTION NAME : "MaxN.calculation"                              #
#                                                                                                     #
#                                                                                                     #
# This function inputs are basically the output of the function "Event.list" which is a vector        #
# of selected events in the RUV_data table from the UVC database                                      #
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
# It is also requested to give a working directory in which a Data_dump folder will automatically     #
# be created to store localy database tables to be able to work off line                              #
#    ex: work.folder="D:/Dropbox/work/research_projects/MAPOR/R_analyses" be carefull, the / might    #
#        need to be changed under linux I'm not sure                                                  #
#                                                                                                     #
# there is a maximal observation distance that we want to keep that need to be set for stereo work.   #
# Default is 7 m, default value can be left alone for mono video work, nothing will change.           #
#    ex: max.dist=7 where distance is in m                                                            #
#                                                                                                     #
#                                                                                                     #
# There is also an option to connect to the database or work only locally (faster). But the first     #
# running of the code need to be done while connected to internet                                     #
#     ex: input : DB.connection="yes" will allow an internet connection, anything else than "yes"     #
#         will force the local working only. The default is a connection ("yes")                      #
#                                                                                                     #
#                                                                                                     #
# The output of that function is a table that will give values for each analysed video :              #
#    * the RUV_treatment_code which is a code to link video "drop" to shorter video analysed          #
#    * the "Sampling_code" for each video which is basically the code of a video "drop"               #
#    * the "field_code" for lecture comodity                                                          #
#    * the "replicate" which is basically the replicate number on a given "drop"                      #
#    * the "video_analysed" this is the name of the video analysed for convenience                    #
#    * the "Date" of the video sampling                                                               #
#    * the "time_obs" which is the time at which the observation was made that day                    #
#    * the "Level_ID" which say what level of identification was made (species, genus or familly)     #
#    * the "Species" which is the name of the object identified (fish)                                #
#    * the "Max.N" which is the maximum number of individual observed on one image for a given        #
#       species in a given video                                                                      #
#    * the "Biomass" which is the total weight estimated for a given species in g in a given video    #
#                                                                                                     #
#                                                                                                     #
#######################################################################################################
#######################################################################################################


#############################################################################
######## create a function to collect max N per species per video ###########
#############################################################################


# create the function
MaxN.calculation=function(DB.connection="yes",work.folder,selected.event,max.dist=7) {
  
  # verify that required packages exist, and install them if not
  if (!require(chron)) install.packages('chron')
  # open required packages
  library(chron)
  
  if (DB.connection=="yes") {
    # verify that required packages exist, and install them if not
    if (!require(DBI)) install.packages('DBI')
    if (!require(RMySQL)) install.packages('RMySQL')
    # open required packages
    library(DBI)
    library(RMySQL)
    
    
    # load MySQL drivers
    drv=dbDriver("MySQL")
    
    # get connection to the database
    con = dbConnect(drv, user="UVC.reader", dbname="Underwater Visual Census",password="Mayfish976",host="162.38.198.139")
    
    # collect the different table available
    Table.name=dbListTables(con)
    
    # load the tables
    RUV_sampling=dbReadTable(con, "RUV_sampling")
    RUV_data=dbReadTable(con, "RUV_data")
    RUV_treatment_metadata=dbReadTable(con, "RUV_treatment_metadata")
    RUV_video_info=dbReadTable(con, "RUV_video_info")
    Fish_school=dbReadTable(con, "Fish_school")
    Especes=dbReadTable(con, "Especes")
    
    
    # set working directory
    setwd(work.folder)
    # create a data_dump folder
    dir.create("Data_dump",showWarnings = F)
    
    # save the data locally in case of offline work
    setwd(paste(work.folder,"/Data_dump",sep=""))
    # write all the tables
    write.table(RUV_sampling,"RUV_sampling.txt",sep=";")
    write.table(RUV_data,"RUV_data.txt",sep=";")
    write.table(RUV_treatment_metadata,"RUV_treatment_metadata.txt",sep=";")
    write.table(RUV_video_info,"RUV_video_info.txt",sep=";")
    write.table(Fish_school,"Fish_school.txt",sep=";")
    write.table(Especes,"Especes.txt",sep=";")
  } # close the if condition
  
  # set the dump directory
  setwd(paste(work.folder,"/Data_dump",sep=""))
  
  # reopen these files
  RUV_sampling=read.table("RUV_sampling.txt",sep=";",header = T)
  RUV_data=read.table("RUV_data.txt",sep=";",header = T)
  RUV_treatment_metadata=read.table("RUV_treatment_metadata.txt",sep=";",header = T)
  RUV_video_info=read.table("RUV_video_info.txt",sep=";",header = T)
  Fish_school=read.table("Fish_school.txt",sep=";",header = T)
  Especes=read.table("Especes.txt",sep=";",header = T)
  
  # get back to the original working directory
  setwd(work.folder)
  
  ##################################################################################
  ############ create a table of max N ############################################
  #################################################################################
  
  # select the data that correspond to the selected event
  trimmed.RUV_data=RUV_data[match(selected.event,RUV_data$Event),]
  
  # si travail stereo select also fish observations that are closer to a certain distance
  if (unique(trimmed.RUV_data$Treatment_type)=="vidsync") {
    trimmed.RUV_data=trimmed.RUV_data[trimmed.RUV_data$Distance<=max.dist,]
  }
  
  #################################################################################
  ########################## get a and b coefficient for biomass ##################
  #################################################################################
  
  # get the species list and a and b coeficient associated
  sp.list=unique(trimmed.RUV_data$Species)
  a.sp.list= Especes$coeff_a[match(sp.list,Especes$id)]
  b.sp.list=Especes$coeff_b[match(sp.list,Especes$id)]
  
  # find coefficient at the Genus level
  genus.list=unique(trimmed.RUV_data$Species[trimmed.RUV_data$Level_ID=="Genus"])
  # get the number of genera
  n.gen=length(genus.list)
  # create two vector to save a and b coef
  a.genus=vector(length=n.gen)
  b.genus=vector(length=n.gen)
  # make a loop to calculate each coef as an average of all genus
  for (i in 1:n.gen) {
    a.genus[i]=mean(Especes$coeff_a[Especes$genre==genus.list[i]],na.rm=T)
    b.genus[i]=mean(Especes$coeff_b[Especes$genre==genus.list[i]],na.rm=T)
  }
  
  # find coefficient at the Familly level
  familly.list=unique(trimmed.RUV_data$Species[trimmed.RUV_data$Level_ID=="Familly"])
  # get the number of genera
  n.fam=length(familly.list)
  # create two vector to save a and b coef
  a.fam=vector(length=n.fam)
  b.fam=vector(length=n.fam)
  # make a loop to calculate each coef as an average of all genus
  for (i in 1:n.fam) {
    a.fam[i]=mean(Especes$coeff_a[tolower(Especes$famille)==tolower(familly.list)[i]],na.rm=T)
    b.fam[i]=mean(Especes$coeff_b[tolower(Especes$famille)==tolower(familly.list)[i]],na.rm=T)
  }
  
  # fill in the genus coef in the species list
  a.sp.list[!is.na(match(sp.list,genus.list))]=a.genus[match(sp.list,genus.list)][!is.na(match(sp.list,genus.list))]
  b.sp.list[!is.na(match(sp.list,genus.list))]=b.genus[match(sp.list,genus.list)][!is.na(match(sp.list,genus.list))]
  
  # fill in the familly coef in the species list
  a.sp.list[!is.na(match(sp.list,familly.list))]=a.fam[match(sp.list,familly.list)][!is.na(match(sp.list,familly.list))]
  b.sp.list[!is.na(match(sp.list,familly.list))]=b.fam[match(sp.list,familly.list)][!is.na(match(sp.list,familly.list))]
  
  # collect these coefficients in a dataframe
  a.b.coef=data.frame(sp.list,a.sp.list,b.sp.list)
  
  # make a list of unique sampling code
  list.Sampling_codes=as.character(unique(trimmed.RUV_data$Sampling_code))
  
  # find the number of sampling code
  n.samp.cod=length(list.Sampling_codes)
  
  # make a loop to collect the info for each sampling code
  for (i in 1:n.samp.cod) {
    # select data from the i sample code
    data.select=trimmed.RUV_data[trimmed.RUV_data$Sampling_code==list.Sampling_codes[i],]
    # get the replicate info
    video.analysed=as.character(unique(data.select$Analyzed_video_name))
    # get the date
    date=as.character(unique(data.select$Date))
    # if stereo data are used, there are no replication per video therefore replicate should be 1
    # get number of replicate
    if (unique(data.select$Treatment_type)=="vidsync") { n.repl=1} else {n.repl=length(video.analysed)}
    # create replicate name
    replicate=paste("R",1:n.repl,sep="")
    # make a loop to make species list in each replicate
    for (j in 1:n.repl) {
      # idem here if it is stereo work, no replication so all data is used
      if (unique(data.select$Treatment_type)=="vidsync") {
        replicate.select=data.select
      } else {
        # select the data per replicate
        replicate.select=data.select[data.select$Analyzed_video_name==video.analysed[j],]
      }
      
      # get the RUV_treatment_code
      RUV_code=unique(replicate.select$RUV_treatment_code)
      # get the Field_code
      Field_code=unique(replicate.select$Field_code)
      # get the fish school info
      School=Fish_school[!is.na(match(Fish_school$RUV_treatment_code,RUV_code)),]
      #School=Fish_school[Fish_school$RUV_treatment_code==RUV_code,]
      
      # make a list of species
      sp=as.character(unique(replicate.select$Species))
      # collect the identification level
      level.ID=replicate.select$Level_ID[match(sp,replicate.select$Species)]
      # make a vector to capture the number of specimen in each species
      n.species=vector(length=length(sp)) 
      # make a vector to get fish biomass in g
      biomass.species=vector(length=length(sp)) 
      
      # make a loop to count specimen of each species and get their total weight
      for (k in 1:length(sp)) {
        # get fish number
        n.species[k]=numb=length(replicate.select$Species[replicate.select$Species==sp[k]])
        # for stereo work calculate weight
        if (unique(data.select$Treatment_type)=="vidsync") {
          # get fish weight
          sizes=replicate.select$Size[replicate.select$Species==sp[k]]
          # get a and b
          a=a.b.coef$a.sp.list[a.b.coef$sp.list==sp[k]]
          b=a.b.coef$b.sp.list[a.b.coef$sp.list==sp[k]]
          # calculate total weight
          biomass.species[k]=weight=sum(a*sizes^b)
        } else {
          biomass.species[k]=0 
        }
        # scale values for the scholl recording
        if (length(na.omit(School$Species))>=1) if (any(School$Species==sp[k])) {
          n.species[k]=School$Total_number[School$Species==sp[k]]
          if (unique(data.select$Treatment_type)=="vidsync") {
            biomass.species[k]=(weight/numb)*n.species[k]
          } else {biomass.species[k]=0}
        }
      }
      
      
      
      
      if (j==1 & i==1) {
        Max.N=data.frame(RUV_treatment_code=rep(RUV_code[1],length(sp)),            # only the first RUV_code is given out of the entire set of videos
                         Sampling_code=rep(list.Sampling_codes[i],length(sp)),
                         field_code=as.character(rep(Field_code,length(sp))),
                         replicate=rep(replicate[j],length(sp)),
                         video_analysed=rep(video.analysed[j],length(sp)),
                         Date=rep(date,length(sp)),
                         time_obs=rep(mean(chron(times=replicate.select$Absolute_time)),length(sp)),
                         Level_ID=level.ID,
                         Species=sp,
                         Max.N=n.species,
                         Biomass=biomass.species)
        
        length(rep(video.analysed[j],length(sp)))
        
      } else {
        Max.N2=data.frame(RUV_treatment_code=rep(RUV_code[1],length(sp)),
                          Sampling_code=rep(list.Sampling_codes[i],length(sp)),
                          field_code=rep(Field_code,length(sp)),
                          replicate=rep(replicate[j],length(sp)),
                          video_analysed=rep(video.analysed[j],length(sp)),
                          Date=rep(date,length(sp)),
                          time_obs=rep(mean(chron(times=replicate.select$Absolute_time)),length(sp)),
                          Level_ID=level.ID,
                          Species=sp,
                          Max.N=n.species,
                          Biomass=biomass.species)
        Max.N=rbind(Max.N,Max.N2)
      }
    }
  }
  # display max N
  Max.N
} # close the function

#############################################################################################################







#####################################################################################################
######################################## function 3 #################################################
#####################################################################################################



#######################################################################################################
################ This is a function that prepare the specimen count or weight for analyses ############
#######################################################################################################
#                                                                                                     #
#                                     FUNCTION NAME : "species.matrix"                                #
#                                                                                                     #
#                                                                                                     #
# This function inputs are basically the Max.N table output from the function "MaxN.calculation"      #
#                                                                                                     #
#                                                                                                     #
# there is an other input requesting if calculation must be done with biomass or no. Default is set   #
#  to no                                                                                              #
#   ex: biomass.calc="no" if desired need to be set as "yes"                                          #
#                                                                                                     #
#                                                                                                     #
# It is also requested to give a working directory in which a Data_dump folder will automatically     #
# be created to store localy database tables to be able to work off line                              #
#    ex: work.folder="D:/Dropbox/work/research_projects/MAPOR/R_analyses" be carefull, the / might    #
#        need to be changed under linux I'm not sure                                                  #
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
# There is also an option to connect to the database or work only locally (faster). But the first     #
# running of the code need to be done while connected to internet                                     #
#     ex: input : DB.connection="yes" will allow an internet connection, anything else than "yes"     #
#         will force the local working only. The default is a connection ("yes")                      #
#                                                                                                     #
#                                                                                                     #
# The output of that function is a list including :                                                   #
#    * a matrix ($species.matrix) that give fish count for each species (columns) for each video      #
#       from the Max.N matrix                                                                         #
#                                                                                                     #
#    * a table ($site.data) with data matching each videos analysed. These data are:                  #
#               - "Sample.name" which is the sample code with sample replicate                        #
#               - "Sample.code" code of the sample                                                    #
#               - "Sample.replicate" number of replicate which match to a small video analysed        #
#               - "Field.code" this is a simple sample code used on the field                         #
#               - "depth" this is the depth at which the video was recorded                           #
#               - "latitude" this is the latitude in wgs84 of the recording                           #
#               - "longitude" this is the longitude of the recording                                  #
#               - "Date" this is the date of the recording                                            #
#               - "time" this is the hour at which the analysed video was recorded                    #
#                                                                                                     #
#    * a table ($species.data) with identification level that match the species in the column of the  #
#        matrix and it contain :                                                                      #
#           - "original.species.list" which is the same species list as in the matrix column          #
#           - "level.id" the level at which each column was idendified (unknown when not id was done) #
#           - "species.select" with species name and unknown for all the other that could not be      #
#               identified at the species level (even genus...)                                       #
#           - "genus.select" with genus name for all species at list identified to the genus level    #
#               and unknown for the others                                                            #
#           - "familly.select" with famillies name for all species at list identified to the familly  #
#                                                                                                     #
#                                                                                                     #
#######################################################################################################
#######################################################################################################

###########################################################################################################
#### create a function to transform a max.N table into a sample-species matrix and give some metadata #####
###########################################################################################################


# start the function
species.matrix=function(DB.connection="yes",work.folder,Max.N,biomass.calc="no") {
  
  # collect data from database
  
  if (DB.connection=="yes") {
    # verify that required packages exist, and install them if not
    if (!require(DBI)) install.packages('DBI')
    if (!require(RMySQL)) install.packages('RMySQL')
    # open required packages
    library(DBI)
    library(RMySQL)
    
    # load MySQL drivers
    drv=dbDriver("MySQL")
    
    # get connection to the database
    con = dbConnect(drv, user="UVC.reader", dbname="Underwater Visual Census",password="Mayfish976",host="162.38.198.139")
    
    # collect the different table available
    Table.name=dbListTables(con)
    
    # load the tables
    Sampling_type=dbReadTable(con, "Sampling_type")
    RUV_sampling=dbReadTable(con, "RUV_sampling")
    RUV_data=dbReadTable(con, "RUV_data")
    RUV_treatment_metadata=dbReadTable(con, "RUV_treatment_metadata")
    RUV_video_info=dbReadTable(con, "RUV_video_info")
    Especes=dbReadTable(con, "Especes")
    Habitat=dbReadTable(con, "Habitat")
    
    # set working directory
    setwd(work.folder)
    # create a data_dump folder
    dir.create("Data_dump",showWarnings = F)
    
    # save the data locally in case of offline work
    setwd(paste(work.folder,"/Data_dump",sep=""))
    # write all the tables
    write.table(Sampling_type,"Sampling_type.txt",sep=";")
    write.table(RUV_sampling,"RUV_sampling.txt",sep=";")
    write.table(RUV_data,"RUV_data.txt",sep=";")
    write.table(RUV_treatment_metadata,"RUV_treatment_metadata.txt",sep=";")
    write.table(RUV_video_info,"RUV_video_info.txt",sep=";")
    write.table(Especes,"Especes.txt",sep=";")
    write.table(Habitat,"Habitat.txt",sep=";")
  } # close the if condition
  
  # set the dump directory
  setwd(paste(work.folder,"/Data_dump",sep=""))
  
  # reopen these files
  Sampling_type=read.table("Sampling_type.txt",sep=";",header = T)
  RUV_sampling=read.table("RUV_sampling.txt",sep=";",header = T)
  RUV_data=read.table("RUV_data.txt",sep=";",header = T)
  RUV_treatment_metadata=read.table("RUV_treatment_metadata.txt",sep=";",header = T)
  RUV_video_info=read.table("RUV_video_info.txt",sep=";",header = T)
  Especes=read.table("Especes.txt",sep=";",header = T)
  Habitat=read.table("Habitat.txt",sep=";",header = T)
  
  # get back to the original working directory
  setwd(work.folder)
  
  ##################################################################################
  
  
  
  # combine Sampling_code info and replicate
  combine.name=paste(Max.N$Sampling_code,"-",Max.N$replicate,sep="")
  
  # get the row names of the matrix
  sample.name=unique(combine.name)
  
  # get the column names of the matrix
  species.list=unique(Max.N$Species)
  
  # create the matrix
  n.sp=length(species.list)
  n.site=length(sample.name)
  site.species.matrix=matrix(nrow = n.site, ncol = n.sp, dimnames = list(sample.name,species.list))
  
  # this is very slow, this need to be improved
  # make a choice for numbers or biomass
  if (biomass.calc=="yes") {
    
    # fill that matrix with biomass
    for (i in 1:n.sp) {
      for (j in 1:n.site) {
        biom=Max.N$Biomass[combine.name==sample.name[j] & Max.N$Species==species.list[i]]
        if (length(biom)==0) {
          site.species.matrix[j,i]=0
        } else {
          site.species.matrix[j,i]=biom
        }
      }
    }
    
  } else {
    
    # fill that matrix with numbers
    for (i in 1:n.sp) {
      for (j in 1:n.site) {
        numb=Max.N$Max.N[combine.name==sample.name[j] & Max.N$Species==species.list[i]]
        if (length(numb)==0) {
          site.species.matrix[j,i]=0
        } else {
          site.species.matrix[j,i]=numb
        }
      }
    }
  }
  
  ##############################################################################################
  # get the metadata that match each site: depth, gps position, time of beginning of recording #
  ##############################################################################################
  
  # get the sample name and the replicate name that correspond to each sample
  sample.code=unlist(strsplit(sample.name,"-"))[rep(c(1,2),n.site)==1]
  sample.replicate=unlist(strsplit(sample.name,"-"))[rep(c(1,2),n.site)==2]
  
  # get the corresponding depth
  # get the glog depth
  depth.sample=RUV_sampling$depth[match(sample.code,RUV_sampling$X.sampling_code)]
  # get the sounder depth
  depth.sample.bis=Sampling_type$depth[match(sample.code,Sampling_type$X.sampling_code)]
  # replace the no glog data by sounder data
  depth.sample[is.na(depth.sample)]=depth.sample.bis[is.na(depth.sample)]
  
  # get the gps position
  latitude=Sampling_type$latitude[match(sample.code,Sampling_type$X.sampling_code)]
  longitude=Sampling_type$longitude[match(sample.code,Sampling_type$X.sampling_code)]
  
  # get field code
  Field.code=Sampling_type$field_code[match(sample.code,Sampling_type$X.sampling_code)]
  
  # create a match vector to get time in Max.N
  n.Max.N=length=length(Max.N$Sampling_code)
  
  location=vector(length=n.site)
  place=c(1:n.Max.N)
  for (i in 1:n.site) {
    location[i]=place[Max.N$Sampling_code==sample.code[i] & Max.N$replicate==sample.replicate[i]][1]
  }
  times=Max.N$time_obs[location]
  date=Max.N$Date[location]
  
  
  # get the habitat data
  H.substrat.type=Habitat$Substrat_type[match(sample.code,Habitat$X.Sampling_code)]
  H.topography=Habitat$Topography[match(sample.code,Habitat$X.Sampling_code)]
  H.complexity=Habitat$Complexity[match(sample.code,Habitat$X.Sampling_code)]
  H.community=Habitat$Community[match(sample.code,Habitat$X.Sampling_code)]
  H.coral.biota=Habitat$Coral_biota[match(sample.code,Habitat$X.Sampling_code)]
  H.sponge=Habitat$Sponge[match(sample.code,Habitat$X.Sampling_code)]
  H.non.scleratinian.coral=Habitat$Non_scleratinian_coral[match(sample.code,Habitat$X.Sampling_code)]
  H.scleratinian.coral=Habitat$Scleratinian_coral[match(sample.code,Habitat$X.Sampling_code)]
  
  
  
  # collect all of that in a dataframe
  site.data=data.frame(Sample.name=sample.name,
                       Sample.code=sample.code,
                       Sample.replicate=sample.replicate,
                       Field.code=Field.code,
                       depth=depth.sample,
                       latitude=latitude,
                       longitude=longitude,
                       Date=date,
                       time=times,
                       H.substrat.type,
                       H.topography,
                       H.complexity,
                       H.community,
                       H.coral.biota,
                       H.sponge,
                       H.non.scleratinian.coral,
                       H.scleratinian.coral)
  
  
  #####################################################################################################
  ########### create a vector with familly name and genus name corresponding to the samples ###########
  #####################################################################################################
  
  # create a vector to save level of identification
  level.id=vector(length=n.sp)
  # find the level of identification
  level.id[!is.na(match(species.list,Especes$id))]="Species"
  level.id[!is.na(match(species.list,Especes$genre))]="Genus"
  level.id[!is.na(match(tolower(species.list),tolower(Especes$famille)))]="Familly"
  level.id[species.list=="unknown_fish"]="Unknown"
  
  # make a vector with only species (the other are marked unknown)
  species.name=as.character(species.list)
  species.name[!is.na(match(species.list,Especes$genre))]="Unknown"
  species.name[!is.na(match(tolower(species.list),tolower(Especes$famille)))]="Unknown"
  species.name[species.list=="unknown_fish"]="Unknown"
  
  # make a vector with only genus (the other are marked unknown)
  genus.name=as.character(species.list)
  genus.name[level.id=="Species"]=as.character(Especes$genre[match(species.list[level.id=="Species"],Especes$id)])
  genus.name[level.id=="Familly"]="Unknown"
  
  # make a vector with only Famillies (the other are marked unknown)
  familly.name=as.character(species.list)
  familly.name[level.id=="Species"]=as.character(Especes$famille[match(species.list[level.id=="Species"],Especes$id)])
  familly.name[level.id=="Genus"]=as.character(Especes$famille[match(species.list[level.id=="Genus"],Especes$genre)])
  familly.name[level.id=="Familly"]=toupper(species.list[level.id=="Familly"])
  
  # collect all this information
  species.classification=data.frame(original.species.list=species.list,
                                    level.id=level.id,
                                    species.select=species.name,
                                    genus.select=genus.name,
                                    familly.select=familly.name)
  
  
  # collect all this information into a list
  Data.analysis=list(species.matrix=site.species.matrix,
                     site.data=site.data,
                     species.data=species.classification)
}
####################################################################################################





#####################################################################################################
########### Improve decay model to avoid some problem with glm###########
#####################################################################################################

decay.model <- function (y, x, model.type = "exponential", y.type = "similarities", 
                         perm = 100) 
{
  model.type <- match.arg(model.type, c("exponential", "power"))
  switch(model.type, exponential = {
    y.type <- match.arg(y.type, c("similarities", "dissimilarities"))
    switch(y.type, similarities = {
      y <- as.vector(y)
      x <- as.vector(x)
      log.glm <- glm(y ~ x, family = gaussian(link = "log"),start = coef(lm(y ~ x)))
      null.dev <- vector(mode = "numeric", length = perm)
      for (i in 1:perm) {
        null.dev[i] <- glm(y ~ sample(x), family = gaussian(link = "log"),start = coef(lm(y ~ sample(x))))$deviance
      }
      p.value <- mean(null.dev < log.glm$deviance)
      parameters <- list(data = data.frame(x, y), model = log.glm, 
                         model.type = model.type, y.type = y.type, pseudo.r.squared = 1 - 
                           log.glm$deviance/log.glm$null.deviance, a.intercept = exp(log.glm$coefficients[1]), 
                         b.slope = log.glm$coefficients[2], p.value = ifelse(p.value == 
                                                                               0, 1/perm, p.value))
      class(parameters) <- "decay"
      return(parameters)
    }, dissimilarities = {
      y <- as.vector(1 - y)
      x <- as.vector(x)
      log.glm <- glm(y ~ x, family = gaussian(link = "log"),start = coef(lm(y ~ x)))
      null.dev <- vector(mode = "numeric", length = perm)
      for (i in 1:perm) {
        null.dev[i] <- glm(y ~ sample(x), family = gaussian(link = "log"),start = coef(lm(y ~ sample(x))))$deviance
      }
      p.value <- mean(null.dev < log.glm$deviance)
      parameters <- list(data = data.frame(x, 1 - y), model = log.glm, 
                         model.type = model.type, y.type = y.type, pseudo.r.squared = 1 - 
                           log.glm$deviance/log.glm$null.deviance, a.intercept = 1 - 
                           exp(log.glm$coefficients[1]), b.slope = -log.glm$coefficients[2], 
                         p.value = ifelse(p.value == 0, 1/perm, p.value))
      class(parameters) <- "decay"
      return(parameters)
    })
  }, power = {
    y.type <- match.arg(y.type, c("similarities", "dissimilarities"))
    switch(y.type, similarities = {
      y <- as.vector(y)
      x <- as.vector(log(x))
      log.glm <- glm(y ~ x, family = gaussian(link = "log"))
      null.dev <- vector(mode = "numeric", length = perm)
      for (i in 1:perm) {
        null.dev[i] <- glm(y ~ sample(x), family = gaussian(link = "log"))$deviance
      }
      p.value <- mean(null.dev < log.glm$deviance)
      parameters <- list(data = data.frame(exp(x), y), 
                         model = log.glm, model.type = model.type, y.type = y.type, 
                         pseudo.r.squared = 1 - log.glm$deviance/log.glm$null.deviance, 
                         a.intercept = exp(log.glm$coefficients[1]), b.slope = log.glm$coefficients[2], 
                         p.value = ifelse(p.value == 0, 1/perm, p.value))
      class(parameters) <- "decay"
      return(parameters)
    }, dissimilarities = {
      y <- as.vector(1 - y)
      x <- as.vector(log(x))
      log.glm <- glm(y ~ x, family = gaussian(link = "log"),start = coef(lm(y ~ x)))
      null.dev <- vector(mode = "numeric", length = perm)
      for (i in 1:perm) {
        null.dev[i] <- glm(y ~ sample(x), family = gaussian(link = "log"),start = coef(lm(y ~ sample(x))))$deviance
      }
      p.value <- mean(null.dev < log.glm$deviance)
      parameters <- list(data = data.frame(exp(x), 1 - 
                                             y), model = log.glm, model.type = model.type, 
                         y.type = y.type, pseudo.r.squared = 1 - log.glm$deviance/log.glm$null.deviance, 
                         a.intercept = 1 - exp(log.glm$coefficients[1]), 
                         b.slope = -log.glm$coefficients[2], p.value = ifelse(p.value == 
                                                                                0, 1/perm, p.value))
      class(parameters) <- "decay"
      return(parameters)
    })
  })
}


#####################################################################################################
###########  Convert distance matrix in dataframe###########
#####################################################################################################
#' @title Convert distance matrix to data frame
#' @description This function takes a distance matrix (of class 'dist') and transforms it to a data.frame, where each row represents a single pairwise comparison.
#' @param dist Distance matrix (object of class 'dist')
#' @param tri Logical, if TRUE - only lower triangular part of dist will be returned
#' @return Data frame
#' @export
#' @seealso \code{\link[spaa]{dist2list}} from spaa package.
#' @author The code is based on \code{\link[spaa]{dist2list}} by Jinlong Zhang.
#'
#' @examples
#' # Generate dummy data
#' datt <- dist(rnorm(10))
#' attr(datt, "Labels") <- paste("obj", 1:10, sep="")
#'
#' # Convert dist to data frame
#' head( dist2list(datt)  )
#' dim( dist2list(datt, tri = T)  ) # only lower-triangular part of dist
#' dim( dist2list(datt, tri = F)  ) # full distance matrix
#'
dist2list <- function (dist, tri=TRUE) {
  if (!class(dist) == "dist") { stop("Error: The input data must be a dist object.\n") }
  
  dat <- as.data.frame(as.matrix(dist))
  if (is.null(names(dat))) {
    rownames(dat) <- paste(1:nrow(dat))
  }
  value <- stack(dat)$values
  rnames <- rownames(dat)
  namecol <- expand.grid(rnames, rnames)
  colnames(namecol) <- c("col", "row")
  res <- data.frame(namecol, value)
  
  if(tri == TRUE){    # return only lower triangular part of dist
    res <- res[-which(upper.tri(as.matrix(dist), diag = T)), ]
  }
  
  return(res)
}







#####################################################################################################
###########  FUNCTION TO COMPUTE BETA###########
#####################################################################################################

testbeta <- function (x, traits, multi = TRUE, warning.time = TRUE, return.details = FALSE, 
                      fbc.step = FALSE, parallel = FALSE, opt.parallel = beta.para.control(), 
                      qhull.opt = NULL) 
{
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) 
    stop("The data in 'x' is not numeric.", call. = TRUE)
  xvals <- unique(as.vector(x))
  if (any(!is.element(xvals, c(0, 1)))) 
    stop("The 'x' table contains values other than 0 and 1: data should be presence/absence.", 
         call. = TRUE)
  if (!is.matrix(traits)) {
    traits <- as.matrix(traits)
  }
  if (is.null(row.names(x))) 
    stop("'x' should have row names with site names", call. = TRUE)
  if (is.null(colnames(x))) 
    stop("'x' should have column names with species names", 
         call. = TRUE)
  if (is.null(row.names(traits))) 
    stop("'traits' should have row names with species names", 
         call. = TRUE)
  if (is.null(colnames(traits))) 
    stop("'traits' should have columns names with traits names", 
         call. = TRUE)
  if (any(colnames(x) != row.names(traits))) 
    stop("Species names in 'x' and 'traits' must be identical (including order)", 
         call. = TRUE)
  if (!is.numeric(traits)) 
    stop("The data in 'traits' is not numeric.", call. = TRUE)
  if (any(is.na(traits))) 
    stop("NA are not allowed in 'traits'", call. = TRUE)
  if (ncol(x) != nrow(traits)) 
    stop("Number of species in 'x' and 'traits' must be identical", 
         call. = TRUE)
  if (!is.null(qhull.opt) && !is.character(qhull.opt))
    stop("qhull.opt is not character")
  if (!"FA" %in% qhull.opt)
    qhull.opt <- c("FA", qhull.opt)
  qhull.opt <- paste(qhull.opt, collapse = " ")
  if (parallel) {
    control.para <- beta.para.control()
    if (!missing(opt.parallel)) {
      control.para[names(opt.parallel)] <- opt.parallel
    }
    nc <- control.para$nc
    if (!is.numeric(nc)) 
      stop("nc must be numeric (integer)", call. = TRUE)
    nc <- as.integer(nc)
    type <- control.para$type
    if (!type %in% c("SOCK", "PSOCK", "FORK")) 
      stop("type only supoort (P)SOCK or FORK", call. = TRUE)
    if (type == "FORK" && Sys.info()["sysname"] == "Windows") 
      stop("Only SOCK clusters are enabled on Windows", 
           call. = TRUE)
    LB <- control.para$LB
    if (!is.logical(LB)) 
      stop("LB must be logical", call. = TRUE)
    size <- control.para$size
    if (!is.null(size) && !is.numeric(size)) 
      stop("size must be numeric (integer)", call. = TRUE)
  }
  f1 <- function(z, N, D) {
    comb_z <- combn(N, z, FUN = paste, collapse = "_")
    comb_inter_z2 <- comb_inter[[z - 2]]
    coord_vert_inter_e <- coord_vert_inter[[z - 2]]
    vol_inter_z <- rep(0, length(comb_z))
    coord_vert_inter_z <- list()
    n1 <- sub("_\\d+$", "", comb_z)
    n2 <- sub("^\\d+_", "", comb_z)
    n1 <- fmatch(n1, comb_inter_z2, nomatch = NA)
    n2 <- fmatch(n2, comb_inter_z2, nomatch = NA)
    for (k in 1:length(comb_z)) {
      seti <- coord_vert_inter_e[[n1[k]]]
      setj <- coord_vert_inter_e[[n2[k]]]
      coord_vert_inter_z[[k]] <- rep(NA, D)
      if (!is.na(sum(seti) + sum(setj))) {
        interij <- inter(seti, setj)
        vol_inter_z[k] <- interij$vol_inter
        coord_vert_inter_z[[k]] <- interij$coord_vert_inter
      }
    }
    return(list(comb_z = comb_z, coord_vert_inter_z = coord_vert_inter_z, 
                vol_inter_z = vol_inter_z))
  }
  D <- ncol(traits)
  Si <- rowSums(x)
  if (any(Si <= D)) 
    stop(paste("'community ", row.names(x)[which(Si <= D)], 
               " must contain at least ", D + 1, " species", sep = ""))
  N <- nrow(x)
  if (N < 2) 
    stop("Computing dissimilairty requires at least 2 communities", 
         call. = TRUE)
  if (multi) {
    if (fbc.step) {
      fbc.step <- FALSE
      warnings("As multi = TRUE, fbc.step was set to FALSE")
    }
    if (warning.time & N > 10) 
      stop(paste("Computing mulitple functional dissimilarity on more than 10 communities may take a long time. \n    \t\t\t\t\t\t\t\t\tSet 'multi' or 'warning.time' to FALSE"))
    if (warning.time & D > 4) 
      stop(paste("Computing mulitple functional dissimilarity in a", 
                 D, "-dimensions functional space may take a long time. \n    \t\t\t\t\t\t\t\t\tSet 'multi' or 'warning.time' to FALSE"))
  }
  nb.step <- 2
  if (multi) 
    nb.step <- N
  if (fbc.step) {
    step.fbc <- as.data.frame(matrix("", nb.step, 1, dimnames = list(c("           FRi", 
                                                                       paste("intersection", 2:nb.step, sep = "_")), c("iteration"))))
    step.fbc[, 1] <- as.character(step.fbc[, 1])
    step.fbc[1, 1] <- paste("0/", N, sep = "")
    for (k in 2:nb.step) step.fbc[k, 1] <- paste("0/", choose(N, 
                                                              k), sep = "")
  }
  FRi <- numeric(N)
  names(FRi) <- row.names(x)
  coord_vert_i <- vector(mode = "list", length = N)
  for (i in 1:N) {
    tr_i <- traits[which(x[i, ] == 1), ]
    ch_i <- convhulln(tr_i, options = "FA")
    FRi[i] <- ch_i$vol
    coord_vert_i[[i]] <- tr_i[unique(as.integer(ch_i$hull)), 
    ]
  }
  names(coord_vert_i) <- row.names(x)
  sumFRi <- sum(FRi)
  inter <- function(set1, set2, qhull.opt = "FA") {
    set1rep <- d2q(cbind(0, cbind(1, set1)))
    set2rep <- d2q(cbind(0, cbind(1, set2)))
    polytope1 <- redundant(set1rep, representation = "V")$output
    polytope2 <- redundant(set2rep, representation = "V")$output
    H_chset1 <- scdd(polytope1, representation = "V")$output
    H_chset2 <- scdd(polytope2, representation = "V")$output
    H_inter <- rbind(H_chset1, H_chset2)
    V_inter <- scdd(H_inter, representation = "H")$output
    vert_1n2 <- q2d(V_inter[, -c(1, 2)])
    coord_vert_inter <- rep(NA, ncol(set1))
    vol_inter <- 0
    if (is.matrix(vert_1n2)) 
      if (nrow(vert_1n2) > ncol(vert_1n2)) {
        coord_vert_inter <- vert_1n2
        vol_inter <- try(convhulln(vert_1n2, qhull.opt)$vol)
        if (inherits(vol_inter, "try-error")){
          save(set1, set2, file = sprintf("D:/test/betapart/Alexandria/%s.Rdata",
                                          round(runif(1)*1e6)))    
          vol_inter <- NA
        }
      }
    return(list(vol_inter = vol_inter, coord_vert_inter = coord_vert_inter))
  }
  comb2 <- combn(N, 2)
  vol_inter2_mat <- matrix(0, N, N, dimnames = list(row.names(x), 
                                                    row.names(x)))
  vol_inter2 <- numeric(ncol(comb2))
  coord_vert_inter2 <- vector(mode = "list", length = ncol(comb2))
  if (parallel) {
    combi <- function(x, y) {
      vol <- c(x$vol, y$vol)
      coord <- c(x$coord, y$coord)
      k <- c(x$k, y$k)
      return(list(vol = vol, coord = coord, k = k))
    }
    iter <- if (is.null(size)) 
      isplitIndices(ncol(comb2), chunks = nc)
    else isplitIndices(ncol(comb2), chunkSize = size)
    cl <- parallel::makeCluster(nc, type = type)
    doParallel::registerDoParallel(cl)
    if (type %in% c("SOCK", "PSOCK")) 
      parallel::clusterExport(cl, c("x", "traits", "comb2", 
                                    "inter", "qhull.opt"), envir = environment())
    interp <- foreach(i = iter, .packages = c("rcdd", "geometry"), 
                      .combine = combi, .inorder = LB) %dopar% {
                        seqs <- i
                        vol <- numeric(length(seqs))
                        coord <- vector(mode = "list", length = length(seqs))
                        u <- 1
                        for (k in seqs) {
                          i <- comb2[1, k]
                          j <- comb2[2, k]
                          seti <- traits[which(x[i, ] == 1), ]
                          setj <- traits[which(x[j, ] == 1), ]
                          interij <- inter(seti, setj, qhull.opt)
                          vol[u] <- interij$vol_inter
                          coord[[u]] <- interij$coord_vert_inter
                          u <- u + 1
                        }
                        res <- list(vol = vol, coord = coord, k = seqs)
                        res
                      }
    parallel::stopCluster(cl)
    ordo <- order(interp$k)
    vol_inter2 <- interp$vol
    vol_inter2 <- vol_inter2[ordo]
    coord_vert_inter2 <- interp$coord
    coord_vert_inter2 <- coord_vert_inter2[ordo]
    vol_inter2_mat[t(comb2[2:1, ])] <- vol_inter2
  }
  else {
    for (k in 1:ncol(comb2)) {
      i <- comb2[1, k]
      j <- comb2[2, k]
      seti <- traits[which(x[i, ] == 1), ]
      setj <- traits[which(x[j, ] == 1), ]
      interij <- inter(seti, setj, qhull.opt)
      vol_inter2_mat[j, i] <- interij$vol_inter
      vol_inter2[k] <- interij$vol_inter
      coord_vert_inter2[[k]] <- interij$coord_vert_inter
      if (fbc.step) {
        step.fbc["intersection_2", 1] <- paste(k, "/", 
                                               ncol(comb2), sep = "")
        write.table(step.fbc, file = "step.fbc.txt", 
                    row.names = T, col.names = F, sep = "\\t")
      }
    }
  }
  shared <- not.shared <- matrix(0, N, N, dimnames = list(row.names(x), 
                                                          row.names(x)))
  for (i in 1:(N - 1)) for (j in (i + 1):N) {
    shared[j, i] <- vol_inter2_mat[j, i]
    not.shared[i, j] <- FRi[i] - vol_inter2_mat[j, i]
    not.shared[j, i] <- FRi[j] - vol_inter2_mat[j, i]
  }
  sum.not.shared <- not.shared + t(not.shared)
  max.not.shared <- pmax(not.shared, t(not.shared))
  min.not.shared <- pmin(not.shared, t(not.shared))
  comb_inter <- list()
  comb_inter[[1]] <- combn(N, 2, paste, collapse = "_")
  coord_vert_inter <- list()
  coord_vert_inter[[1]] <- coord_vert_inter2
  vol_inter <- list()
  vol_inter[[1]] <- vol_inter2
  FRt <- NA
  a <- NA
  if (N > 2 & multi) {
    for (z in 3:N) {
      res <- f1(z, N, D)
      comb_inter[[z - 1]] <- res$comb_z
      coord_vert_inter[[z - 1]] <- res$coord_vert_inter_z
      vol_inter[[z - 1]] <- res$vol_inter_z
      if (fbc.step) {
        step.fbc[paste("intersection", z, sep = "_"), 
                 1] <- paste(ncol(res$comb_z), "/", ncol(res$comb_z), 
                             sep = "")
        write.table(step.fbc, file = "step.fbc.txt", 
                    row.names = T, col.names = F, sep = "\\t")
      }
    }
    sumvol_sign <- rep(NA, N - 1)
    for (k in 2:N) {
      sumvol_sign[k - 1] <- (-1)^(k - 1) * sum(vol_inter[[k - 
                                                            1]])
    }
    FRt <- sumFRi + sum(sumvol_sign)
    a <- sumFRi - FRt
  }
  details <- NA
  if (return.details) {
    names(coord_vert_i) <- names(FRi)
    CH <- list(FRi = FRi, coord_vertices = coord_vert_i)
    intersections <- list(combinations = comb_inter, volumes = vol_inter, 
                          coord_vertices = coord_vert_inter)
    details <- list(CH = CH, intersections = intersections)
  }
  functional.computations <- list(sumFRi = sumFRi, FRt = FRt, 
                                  a = a, shared = shared, not.shared = not.shared, sum.not.shared = sum.not.shared, 
                                  max.not.shared = max.not.shared, min.not.shared = min.not.shared, 
                                  details = details)
  class(functional.computations) <- "functional.betapart"
  return(functional.computations)
}




#####################################################################################################
###########  FUNCTION TO COMPUTE DISTANCE BETWEEN COORDINATES###########
#####################################################################################################
library(Imap)
ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}



#AddLevel function
##############
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}
##############

`%notin%` <- Negate(`%in%`)









####################################################################################
# FUNCTION to compute Chao's alpha and beta diversity, expressed as Hill Numbers,
# for a set of communities
# Modified from Chiarello Marèlène
# This function requires the installation of packages entropart (Marcon & Hérault, 2015 ; Marcon et al.,
# PloS One, 2014)
#
# Arguments :
# - matrix : a matrix with raw or relative abundances of species (columns) in
#            communities (rows)
# -tree_phylog (optional) : a rooted ultrametric phylogenetic tree in phylog format
#                           see ?newick2phylog and nota bene below for explanation on phylog format
#                           The tip labels and the column names of the matrix must be identical ;
#                           but not necessarely in the same order.
#
# - q : value(s) for the q parameter, among 0,1,2. Default is q=c(0,1,2)
#       See Chao et al. 2014 for explanations about the q parameter.
#
# - run_example : if TRUE, a simple example provided at the end of this script is runned.
#
#
# Details :
# - The taxonomic and phylogenetic diversities are calculated following Chao et al (2014). Please see the
#   entropart documentation for more details.
# - If tree_phylog is provided, the phylogenetic diversity is calculated.
# - Chao's phylogenetic diversity can be calculated on non-ultrametric trees, but the meaning is not clear.
#   If a non-ultrametric is provided, phylogenetic diversity will be calculated an returned with a warning.
# - If raw abundances are provided, unequal numbers of individuals between communities are allowed. In this case,
#   the total abundance in each community will be taken into account when calculating pairwise beta diversity.
#
#
# OUTPUTS : a list with for each biodiversity facet (i.e. taxo and phylo):
# - $alpha_taxo: matrix of taxonomic alpha diversity values for each value of q (column) for all communities
#   (rows);
# - $alpha_phylo: matrix of phylogenetic alpha diversity values for each value of q (column) for all communities
#   (rows);
# - $beta_taxo: up to three matrices containing pairwise taxonomic beta diversity for each value of q;
# - $beta_phylo: up to three matrices containing pairwise phylogenetic beta diversity for each value of q;
#
#
# NB : The conversion of a tree from the phylo format into the phylog format is easy :
# write.tree(tree, file="tree.txt") # 1°/ you need to make a .txt file in a newick format
# tree<-paste(readLines('tree.txt')) # 2°/ you need to read it as a character string
# tree_phylog<-newick2phylog(tree) # 3°/ newick2phylog{ade4} converts the character string into a phylog object
#
####################################################################################
####################################################################################

chao_alpha_beta <- function(matrix, tree_phylog=NULL, q=c(0,1,2), beta=TRUE, run_example=F)
  
{
  
  # check function arguments
  if(missing(matrix)) stop("A matrix with relative abundance data must be provided")
  if(is.null(tree_phylog)==F & class(tree_phylog) != "phylog") stop("Please convert the tree into a 'phylog' format")
  if(any(is.na(matrix))) stop("The abundances matrix contains NA(s). Please check")
  
  # load libraries
  require(ape)
  require(ade4)
  require(entropart)
  
  ########################## Alpha and beta phylo
  if(is.null(tree_phylog)==F){
    
    if(is.ultrametric(as.phylo(tree_phylog))==F) warning("Your tree is not ultrametric")
    
    alpha_matrix<-matrix(NA,nrow=length(rownames(matrix)), ncol=length(q), dimnames=list(rownames(matrix),paste0("q",q)))
    
    for(i in 1:length(rownames(matrix)))
    {
      print(paste0("i = ", i))
      
      rel_abundances_i<-matrix[i,]/sum(matrix[i,])
      rel_abundances_i <- as.vector(t(rel_abundances_i))
      names(rel_abundances_i) <-  colnames(matrix)
      
      for(j in 1:length(q))
      {
        alpha_matrix[i,j]<-ChaoPD(rel_abundances_i, q=q[j],tree_phylog,  CheckArguments=T)
      } # end of j
    } # end of i
    
    if(beta==T){ # if.beta
      
      combin<-combn(rownames(matrix),2)
      
      res_beta<-lapply(1:length(q), function(x) matrix(0,nrow=length(rownames(matrix)), ncol=length(rownames(matrix)), dimnames=list(rownames(matrix),rownames(matrix))))
      names(res_beta)<-paste0("q",q)
      
      for(k in 1:(length(combin)/2))  
      {
        print(paste0("k = ", k))
        com_1<-combin[1,k]
        com_2<-combin[2,k]
        
        for(l in 1:length(q))
        {
          alpha_com_1<-alpha_matrix[com_1,paste0("q",q[l])]
          alpha_com_2<-alpha_matrix[com_2,paste0("q",q[l])]
          
          weight_com_1<-sum(matrix[com_1,])
          weight_com_2<-sum(matrix[com_2,])
          
          weighted_abundances_com_1<-(matrix[com_1,]*weight_com_1)/(weight_com_1+weight_com_2)
          weighted_abundances_com_2<-(matrix[com_2,]*weight_com_2)/(weight_com_1+weight_com_2)
          
          rel_abundances_2_com<-rbind(weighted_abundances_com_1, weighted_abundances_com_2)
          rel_abundances_2_com<-apply(rel_abundances_2_com, 2, sum)
          rel_abundances_2_com<-rel_abundances_2_com/sum(rel_abundances_2_com)
          
          gamma<-ChaoPD(rel_abundances_2_com, q=q[l], tree_phylog, CheckArguments=F)
          
          if(q[l]==0) #2
          {
            mean_alpha<-alpha_com_1*(weight_com_1/(weight_com_1+weight_com_2))+alpha_com_2*(weight_com_2/(weight_com_1+weight_com_2))
            
            res_beta$q0[com_1, com_2]<-(gamma/mean_alpha)-1
            res_beta$q0[com_2, com_1]<-res_beta$q0[com_1, com_2]
          } # end of if() #2
          
          if(q[l]==1) #3
          {
            alpha_com_1<-log(alpha_com_1)
            alpha_com_2<-log(alpha_com_2)
            
            mean_alpha<-alpha_com_1*(weight_com_1/(weight_com_1+weight_com_2))+alpha_com_2*(weight_com_2/(weight_com_1+weight_com_2))
            
            res_beta$q1[com_1, com_2]<-(gamma/exp(mean_alpha))-1
            res_beta$q1[com_2, com_1]<-res_beta$q1[com_1, com_2]
          } # end of if() #3
          
          if(q[l]==2) #4
          {
            alpha_com_1<-1-1/alpha_com_1
            alpha_com_2<-1-1/alpha_com_2
            
            mean_alpha<-alpha_com_1*(weight_com_1/(weight_com_1+weight_com_2))+alpha_com_2*(weight_com_2/(weight_com_1+weight_com_2))
            
            res_beta$q2[com_1, com_2]<-(gamma/(1/(1-mean_alpha)))-1
            res_beta$q2[com_2, com_1]<-res_beta$q2[com_1, com_2]
          } # end of if() #4      
          
        } # end of l
      } # end of k
    } # end of if.beta
  } # end of if.null(tree_phylog)
  
  ################################# Alpha and beta taxo
  alpha_matrix_taxo<-matrix(NA,nrow=length(rownames(matrix)), ncol=length(q), dimnames=list(rownames(matrix),paste0("q",q)))
  
  for(i in 1:length(rownames(matrix)))
  {
    
    rel_abundances_i<-matrix[i,]/sum(matrix[i,])
    rel_abundances_i <- as.ProbaVector(rel_abundances_i)
    
    for(j in 1:length(q))
    {
      if(q[j]==0) #if1
      {
        alpha_matrix_taxo[i,j]<-Tsallis(rel_abundances_i, q = 0, CheckArguments=T)
      } # end of if1
      
      if(q[j]==1) # if2
      {
        alpha_matrix_taxo[i,j]<-exp(Tsallis(rel_abundances_i, q = 1, CheckArguments=T))
      } # end of if2
      
      if(q[j]==2) # if3
      {
        alpha_matrix_taxo[i,j]<-1/(1-Tsallis(rel_abundances_i, q = 2, CheckArguments=F))
      } # if3
    } # end of j
  } # end of i
  
  combin<-combn(rownames(matrix),2)
  
  if(beta==T){ # if.beta
    res_beta_taxo<-lapply(1:length(q), function(x) matrix(0,nrow=length(rownames(matrix)), ncol=length(rownames(matrix)), dimnames=list(rownames(matrix),rownames(matrix))))
    names(res_beta_taxo)<-paste0("q",q)
    
    for(k in 1:(length(combin)/2))  
    {
      com_1<-combin[1,k]
      com_2<-combin[2,k]
      
      for(l in 1:length(q))
      {
        alpha_com_1<-alpha_matrix_taxo[com_1,paste0("q",q[l])]
        alpha_com_2<-alpha_matrix_taxo[com_2,paste0("q",q[l])]
        
        weight_com_1<-sum(matrix[com_1,])
        weight_com_2<-sum(matrix[com_2,])
        
        weighted_abundances_com_1<-(matrix[com_1,]*weight_com_1)/(weight_com_1+weight_com_2)
        weighted_abundances_com_2<-(matrix[com_2,]*weight_com_2)/(weight_com_1+weight_com_2)
        
        rel_abundances_2_com<-rbind(weighted_abundances_com_1, weighted_abundances_com_2)
        rel_abundances_2_com<-apply(rel_abundances_2_com, 2, sum)
        rel_abundances_2_com<-rel_abundances_2_com/sum(rel_abundances_2_com)
        
        if(q[l]==0) #2
        {
          mean_alpha<-alpha_com_1*(weight_com_1/(weight_com_1+weight_com_2))+alpha_com_2*(weight_com_2/(weight_com_1+weight_com_2))
          
          gamma<-Tsallis(rel_abundances_2_com, q = 0, CheckArguments=T)
          res_beta_taxo$q0[com_1, com_2]<-(gamma/mean_alpha)-1
          res_beta_taxo$q0[com_2, com_1]<-res_beta_taxo$q0[com_1, com_2]
        } # end of if() #2
        
        if(q[l]==1) #3
        {
          alpha_com_1<-log(alpha_com_1)
          alpha_com_2<-log(alpha_com_2)
          
          mean_alpha<-alpha_com_1*(weight_com_1/(weight_com_1+weight_com_2))+alpha_com_2*(weight_com_2/(weight_com_1+weight_com_2))
          
          gamma<-exp(Tsallis(rel_abundances_2_com, q = 1, CheckArguments=F))
          res_beta_taxo$q1[com_1, com_2]<-(gamma/exp(mean_alpha))-1
          res_beta_taxo$q1[com_2, com_1]<-res_beta_taxo$q1[com_1, com_2]
        } # end of if() #3
        
        if(q[l]==2) #4
        {
          alpha_com_1<-1-1/alpha_com_1
          alpha_com_2<-1-1/alpha_com_2
          
          mean_alpha<-alpha_com_1*(weight_com_1/(weight_com_1+weight_com_2))+alpha_com_2*(weight_com_2/(weight_com_1+weight_com_2))
          
          gamma<-1/(1-Tsallis(rel_abundances_2_com, q = 2, CheckArguments=F))
          res_beta_taxo$q2[com_1, com_2]<-(gamma/(1/(1-mean_alpha)))-1
          res_beta_taxo$q2[com_2, com_1]<-res_beta_taxo$q2[com_1, com_2]
        } # end of if() #4      
        
      } # end of l
    } # end of k
  } # end of if.beta
  
  if(is.null(tree_phylog)==F) {
    res_chao_alpha_beta<-list(alpha_matrix_taxo,res_beta_taxo, alpha_matrix, res_beta)
    names(res_chao_alpha_beta)<-c("alpha_taxo","beta_taxo", "alpha_phylo", "beta_phylo")
    return(res_chao_alpha_beta)
  } # end of is.null(tree_phylog)==F
  
  if(is.null(tree_phylog)) {
    res_chao_alpha_beta<-list(alpha_matrix_taxo,res_beta_taxo)
    names(res_chao_alpha_beta)<-c("alpha_taxo","beta_taxo")
    return(res_chao_alpha_beta)
  } # end of if.taxo
  #####################################
  
} # end of chao_alpha_beta

####################################################################################
# Example

run_example=FALSE

if( run_example==TRUE) {
  
  
  # INPUTS
  matrix<-matrix(NA, nrow=7, ncol=5)
  matrix[1,]<-c(1,2,3,4,5)
  matrix[2,]<-c(5,4,3,2,0)
  matrix[3,]<-c(3,2,1,4,5)
  matrix[4,]<-c(2,0,5,4,1)
  matrix[5,]<-c(4,3,2,0,5)
  matrix[6,]<-c(8,3,2,0,5)
  matrix[7,]<-c(4,3,2,0,10)
  
  
  rownames(matrix)<-c("Community_A", "Community_B", "Community_C", "Community_D", "Community_E", "Community_F", "Community_G")
  colnames(matrix)<-paste0("OTU_", 1:5)
  
  library(ape)
  library(ade4)
  tree<-rcoal(n=5, rooted=T, tip.label=colnames(matrix))
  write.tree(tree, file="tree.txt")
  tree<-paste(readLines('tree.txt'))
  tree_phylog<-newick2phylog(tree)
  
  # EXECUTE THE FUNCTION
  #test<-chao_alpha_beta(matrix, tree_phylog)
  #test$alpha_phylo # alpha diversit values in a matrix format
  #test$alpha_taxo
  #test$beta_phylo$q0 # beta values for q=0
  #test2<-chao_alpha_beta(matrix)
  #test2
  
}
####################################################################################


