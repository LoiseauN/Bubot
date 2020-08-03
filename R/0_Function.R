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