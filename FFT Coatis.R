####Load data from Movebank


library(getPass)  #allows confidientality when putting in password
library(move)     #to handle movement data of various types developed at MPI

pass <- getPass::getPass() ##keep password confidential, 2 dots mean run getpass fct from getpass library, used to specifiy from which package
loginStored <- movebankLogin(username="EliNeuhaus", password=pass)


Avery <- getMovebankNonLocationData(study=468460067 , sensorID="Acceleration",animalName="Avery", login=loginStored)

Avery$individual_local_identifier <- paste(Avery$individual_local_identifier, Avery$tag_local_identifier, sep = " ")


Carlsberg <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Carlsberg", login=loginStored)

Carlsberg$individual_local_identifier <- paste(Carlsberg$individual_local_identifier, Carlsberg$tag_local_identifier, sep = " ")


Clementina <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Clementina", login=loginStored)

Clementina$individual_local_identifier <- paste(Clementina$individual_local_identifier, Clementina$tag_local_identifier, sep = " ")


Ellie <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Ellie", login=loginStored)

Ellie$individual_local_identifier <- paste(Ellie$individual_local_identifier, Ellie$tag_local_identifier, sep = " ")


Fonta_Flora <- getMovebankNonLocationData(study=468460067 , sensorID="Acceleration",animalName="Fonta Flora", login=loginStored)

Fonta_Flora$individual_local_identifier <- paste(Fonta_Flora$individual_local_identifier, Fonta_Flora$tag_local_identifier, sep = " ")


Galena <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Galena", login=loginStored)

Galena$individual_local_identifier <- paste(Galena$individual_local_identifier, Galena$tag_local_identifier, sep = " ")


Gillian <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Gillian", login=loginStored)
Gillian$individual_local_identifier <- paste(Gillian$individual_local_identifier, Gillian$tag_local_identifier, sep = " ")


Golliath <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Golliath", login=loginStored)
Golliath$individual_local_identifier <- paste(Golliath$individual_local_identifier, Golliath$tag_local_identifier, sep = " ")


Ornette <- getMovebankNonLocationData(study=468460067 , sensorID="Acceleration",animalName="Ornette", login=loginStored)
Ornette$individual_local_identifier <- paste(Ornette$individual_local_identifier, Ornette$tag_local_identifier, sep = " ")

Peter_Nelson <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Peter Nelson", login=loginStored)
Peter_Nelson$individual_local_identifier <- paste(Peter_Nelson$individual_local_identifier, Peter_Nelson$tag_local_identifier, sep = " ")

Pliny <- getMovebankNonLocationData(study=468460067 , sensorID="Acceleration",animalName="Pliny", login=loginStored)
Pliny$individual_local_identifier <- paste(Pliny$individual_local_identifier, Pliny$tag_local_identifier, sep = " ")



Riwaka <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Riwaka", login=loginStored)
Riwaka$individual_local_identifier <- paste(Riwaka$individual_local_identifier, Riwaka$tag_local_identifier, sep = " ")

Sahti <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration",animalName="Sahti", login=loginStored)
Sahti$individual_local_identifier <- paste(Sahti$individual_local_identifier, Sahti$tag_local_identifier, sep = " ")

Sofie <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Sofie", login=loginStored)
Sofie$individual_local_identifier <- paste(Sofie$individual_local_identifier, Sofie$tag_local_identifier, sep = " ")

Thelonious <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Thelonious", login=loginStored)
Thelonious$individual_local_identifier <- paste(Thelonious$individual_local_identifier, Thelonious$tag_local_identifier, sep = " ")

Vielle <- getMovebankNonLocationData(study=468460067 ,sensorID="Acceleration", animalName="Vielle", login=loginStored)
Vielle$individual_local_identifier <- paste(Vielle$individual_local_identifier, Vielle$tag_local_identifier, sep = " ")

###Consolidate data into one dataframe
data <- rbind(Avery, Carlsberg,Clementina,Ellie,Fonta_Flora,
              Galena,Gillian,Golliath,
              Ornette,Peter_Nelson,
              Pliny,Riwaka,
              Sahti,Sofie,Thelonious,Vielle)

setwd("/Users/wilddog/Documents/eneuhaus/Coaticsv")

write.csv(data, "accdata2.csv")

mydata <- read.csv("accdata2.csv")                #I am reading the accdata2 csv file into a data frame called mydata mithilfe von read.csv
head(mydata)                                      #shows first few rows
