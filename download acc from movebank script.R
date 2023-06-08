#This script is downloading the ACC data from MoveBank and making it into a data frame 

#loading in libraries
library(getPass)  
library(move)
library(dplyr)

pass <- getPass::getPass() ##keep password confidential

data = read.csv("C:/Users/eliza/Documents/UNI/SS23/MPI-Bachelorarbeit/R_Thesis/Food for Thought - Comparative Frugivore Tracking Cleaned data v2.csv")
names = unique(data$individual.local.identifier)
names = strsplit(names, " ")
names = do.call(rbind,names) # ??shauhin??
names = names[,1]

loginStored <- movebankLogin(username="EliNeuhaus", password=pass) #NEED ACCESS!!!!

accdata=c()
for(i in 1:length(names)){
  accdata[i]<-try(list(getMovebankNonLocationData(study=468460067 , 
                                                 sensorID="Acceleration", 
                                                 animalName=names[i], 
                                                 login=loginStored)
                      )
      )
  if(class(accdata[[i]])=="try-error"){
    next
    }
}

accdata
accdata2=plyr::ldply(accdata, data.frame)
accdata2$ID=paste(accdata2$individual_local_identifier, accdata2$tag_local_identifier, sep="_")
