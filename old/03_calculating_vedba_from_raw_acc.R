#### Calculating VeDBA from raw acc data ####
library( stringr )
library( data.table )

## a function that uses a rolling window to calculate the dynamic component of a vector
##win_size is in samples, so a window size of 7 would be 7 acc samples, not units of time. Has to be an odd number
##For FFT data, data appear to be 12hz, so a windowsize of 7 is ~0.5 seconds

species="Nasua Narica"  # Change species to the species you will be working on assuming multiple species are in the dataframe

dy_acc <- function(vect, win_size = 7){
  #vect is a vector with the acc data for one axis for one burst
  #win-size can be changed, but should be default 7 for our data, in our case 7 oberservations are ca. 7 min (10 with pads)
  pad_size <- win_size/2 - 0.5 #make sure length out == length in. Adds a number of NAs to each side of vector 
  # buffers the vector, creates an odd number
  
  padded <- unlist( c(rep(NA, pad_size), vect, rep(NA, pad_size)) ) #adds the pad to the input vector
  acc_vec <- rep(NA, length = length( vect ) ) ##creates empy output vector 
  
  ## sliding window
  for(i in 1:length(vect)){
    win <- padded[i:(i+(2*pad_size))] ## subset the window, aka isolates win_size observations at a time from the input vector. 
    #":" creates a sequence from one number to the next number
    m_ave <- mean( win, na.rm = T ) ## take the average over the window
    acc_comp <- vect[ i ] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i] <- acc_comp ##Store output 
  }
  
  return( unlist( acc_vec) )
}


## read in the complete data. This is the data downloaded from Movebank with "All sensors" selected (both GPS and ACC)
#accdata2 = data    #in the case that accdata2 is in the global environment. renaming data dataframe to accdata2 (cloning it)
#rm(data)           #makes sure u only have one version so only accdata2 and not data, good for ram 
accdata2 = read.csv("C:/Users/eliza/Documents/UNI/SS23/MPI-Bachelorarbeit/R_Thesis/Coaticsv/accdata2.csv") #in the case that accdata2  isnt in the global environment
complete_data <- accdata2 #to read in data, download code safes it, then read that file in. creates copy of accdata2 called "complete_data" to avoid corrupting original
class(complete_data)                 #prints in console wethere or not it's a dataframe. if yes then use l39, if no comment out l39.
# complete_data <- as.data.frame( complete_data )    #ensures that this copy of accdata2 is in the needed format aka dataframe. if its a dataframe ALREADY an error might come up. to check if it is or not use class(mydata) and class(complete_data) etc.
complete_data = accdata2


## this makes the timestamp into local time by adding three hours
complete_data$timestamp <- as.POSIXct(x= complete_data$timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='UTC' ) ## turns the timestamp into a POSIX element
complete_data$local_timestamp=lubridate::with_tz(complete_data$timestamp, tzone = "America/Panama")   #creates new column "local timestamp" that is filled with local time zone, converted from the times in "timestamp" using the with_tz function from lubridate package.

##subset data but check for weird browser vs r download differences in column names 
complete_data_trim <- try(complete_data[ , c( 'individual_id', 'local_timestamp' , 'eobs_accelerations_raw', 'eobs_acceleration_sampling_frequency_per_axis', 'individual_taxon_canonical_name' ), ]) ## keep only the necessary columns, wrapped in try function, to exclude underscores etc.
if(class(complete_data_trim)=="try-error"){
  complete_data_trim <- complete_data[ , c( 'individual_id', 'local_timestamp' , 'eobs_accelerations_raw', 'eobs_acceleration_sampling_frequency_per_axis', 'individual_taxon_canonical_name') ] ## keep only the necessary columns. checks if the result of the previous subset ("complete_Data_trim") is an error object, if YES (indicating that column names were not found exactly), the code reverts to another try to subset "complete_data" without using the try() function
}


# rename the columns to simpler names
names( complete_data_trim ) <- c( 'tag', 'local_timestamp', 'eobs_accelerations_raw', 'sampling_freq_per_axis', 'species' )
head( complete_data_trim )

# trim the data to only the rows that contain coati data and only those rows that have ACC data
#acc_dat <- complete_data_trim[ complete_data_trim$eobs_accelerations_raw != "" & complete_data_trim$species == species, ] #deletes everything that is not coati based on 2 conditions: first one filters out every row in "eobs_acc_raw" that is empty. 
#unique(complete_data_trim$species)
acc_dat <- complete_data_trim                      #making a copy of complete_data_trim bc line 60 doesn't work. acc_dat stays empty, 0 obs of 5 variables.

# remove complete_data and complete_data_trim so that they don't take up so much ram
rm( complete_data, complete_data_trim, accdata2 ) #allows to delete things from the environment, that are not needed anymore
gc() #garbage collection to free up ram 

head( acc_dat ) #prints first 5 rows to control the run of the data worked


#tail would show the last 5 rows or str would show the character of the data or class (object)

length( unique( acc_dat$eobs_accelerations_raw ))          #calculates the number of unique values in the column "eobs_acc_raw" of "acc_dat"
nrow( acc_dat )                                            #returns number of rows in "acc_dat" dataframe which gives us the total number of observations in the dataset.

acc_dat=acc_dat[complete.cases(acc_dat$sampling_freq_per_axis),] #to remove missing information NA and ensure that only rows w/ complete information in that column are retained in data frame. (removes rows from "acc_dat" where "sampling_freq_per_axis" column has NA values) 
sum(is.na(acc_dat$sampling_freq_per_axis)) # to make sure that worked, count how many NAs are in the column => should be 0
## each acc burst is unique, as we would expect. This confirms that none of the rows have NAs or otherwise missing data where the ACC burst data should be


## What data do we have for data?

# plot(as.numeric(as.factor(acc_dat$tag))~acc_dat$local_timestamp,cex=0.3,pch=16,main = "Accelerometry bursts",xlab="",xaxt='n',yaxt='n',ylab="ID")
# axis(2,at=1:length(unique(acc_dat$tag)),labels=sort(unique(acc_dat$tag)),las=1,cex=0.3)
# axis.POSIXct(1,at=seq(min(acc_dat$local_timestamp),max(acc_dat$local_timestamp),by="1 month"),las=2)
# duration of the ACC data matches that of the GPS data

length( strsplit( acc_dat$eobs_accelerations_raw[ 1 ], " " )[[1]] ) ## 120 ACC samples collected per minute. This is 40 samples per axis. With a sampling frequency of 10.54 Hz per axis, this makes roughly 3 second bursts. the 1st element of this column is split into substrings using the SPACE delimiter " ", returning a list of character vectors. Then length() calcultes the length of those vectors which corresponds t0o the number of substrings after splitting the tsring.

# make a column for night during which the burst occurs (night is defined as the 24 hour period from noon to noon, with the date of the night being the date on which the night starts (i.e. the date of the first noon of the night) )
acc_dat$night <- lubridate::date( acc_dat$local_timestamp - lubridate::hours(12)) #each timestamp in the "local_timestamp" column gets substracted 12 hours. DARAUFHIN the date portion from the timestamps is extracted using "date" and those dates are added to the "night" column in "acc_dat"

# save the tag names by creating a vector "tag_names" 
tag_names <- as.character( unique( acc_dat$tag ) ) #include animal name and identifier. "as.character()" converts unique tag values into character format.

# make a column for time of day
acc_dat$time <- str_split_fixed(acc_dat$local_timestamp, " ", 2)[,2] #extracting time from the date colum, could use lubridate for that 
# split each string of ACC samples so that we can parse them into X, Y, and Z axes. Each row is a burst, split into each axis and each separate measurement per axis
d2 <- as.data.frame( str_split( acc_dat$eobs_accelerations_raw, " ", simplify = T ) )
#this takes super long!!

# make the columns class numeric instead of character strings. SUPER LONG TOO
for(i in 1:ncol(d2)){
  
  d2[,i] <- as.numeric(as.character((d2[,i])))
  
}

# name the columns based on whether the sample belongs to the x, y, or z axis
names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

# add the timestamp and tag data to this dataframe. We can add it directly from acc_dat because d2 is just a column from acc_dat that has been split into several column (i.e. the rows still match those of acc_dat)
d2$local_timestamp <- acc_dat$local_timestamp
d2$tag <- acc_dat$tag

# show any rows for which we don't have a complete ACC burst. This would occur, for example, if there were times of the day when the sampling frequency lower or burst duration was shorter
d2[!complete.cases(d2),]

# remove any rows for which we don't have a complete ACC burst
inds <- complete.cases(d2)
acc_dat <- acc_dat[ inds ,]
d2 <- d2[ inds ,]

## split the parsed axes into their own dataframes
x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

# calculate the average vedba per burst
# Applys the dy_acc function to all the bursts -> apply a previous function to things in the list 
acc_dat$ave_vedba <- apply( sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + 
                                    apply( y_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + 
                                    apply( z_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2) , 2, FUN = mean ) #THIS ONE TAKES SUPER LONG TOO T-T
# all x, y, z numbers and create a average mean 
# make a column for the logs of the average vedba
acc_dat$log_vedba <- log( acc_dat$ave_vedba ) #log that mean 

# create folder on computer to save vedba data in 
write.csv(acc_dat, "C:/Users/eliza/Documents/UNI/SS23/MPI-Bachelorarbeit/R_Thesis/Coaticsv/full_night_and_day_data.csv", row.names = F) #saves the vedba 


head( acc_dat )

#new accdata saved.this is useless for me. this isolates coatis, creates "IDs" dataframe, manipulates columns.
#accdata2=accdata2[which(accdata2$individual_taxon_canonical_name=="Nasua Narica"),]
#IDs = data.frame(unique(cbind(accdata2$individual_id, accdata2$individual_local_identifier, accdata2$tag_local_identifier)))
#IDs$ID = paste(IDs$X2, IDs$X3, sep = "_")
# for (i in 1: nrow(IDs)){
#   acc_dat$tag[which(acc_dat$tag == IDs$X1[i])]<- IDs$ID[i]
# }

