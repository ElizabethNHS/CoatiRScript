#### Sleep Analysis ####

  library( stats )
  library( plyr )
  library( dplyr )
  library( purrr )
  library( reshape2 )
  library( hms )
  library( data.table )
  library(multcomp)
  library(tidyr) 
  library(insight)
  library(httr)
  library( stringr )
  library( lubridate )
  library( plotrix )
  library( suncalc )
  library( sp )
  library( rgdal )
  library(rgeos)
  library( entropy )
#library(suncalc)

options( warn = 0)

################# Functions #########################
rm (d2, inds, x_d, y_d, z_d, dy_acc, tag_names, species, i, indexes, NAs, proportion, real, test2, test, testsplit, accdata2, complete_data, complete_data_trim)

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

## function for plotting times from noon to noon. It will make correct 12:00 - 24:00 to be 00:00 - 12:00 and 00:00 - 12:00 to be 12:00 to 24:00

ts_func <- function( time_vec ){
  
  num_time <- as.numeric( as_hms( time_vec ) )
  
  corr_time <- ifelse( num_time < 12*60*60, num_time + 12*60*60, num_time - 12*60*60 )
  
  return( corr_time )
  
}

#### sleep period function

## missing_mins: this is the maximum total number of minutes of data that can be missing from a day and still have that day included in the analysis

## time_gap: this is the maximum allowable time gap between two accelerometer bursts (in seconds) that can exist in a day without removing this day from the data

## move_window: this is the size of the moving window (in minutes) used in calculating the rolling median 

## percentile: this is the percentile threshold of the variable (within the noon-to-noon time period) used to classify activity vs. inactivity 

## block_size: duration in minutes of the blocks of continuous inactivity that will be considered sleep

## gap_size: maximum duration between sleep blocks that will be merged

## title: if title == T, the plot will include the tag number and the night number at the top of the plot

## x_axis: if x_axis == F, the plot will be plotted without an x axis
## waso_percentile: this is the percentile threshold of the variable (within the noon-to-noon time period) used to classify activity vs. inactivity within the sleep period

## waso_multiplier: this is the multiplier of the threshold value determined by the percentile above. Values WITHIN the sleep period below the threshold value multiplied by the multiplier are considered inactive and those equal to or above are considered active. This threshold value may be different than the one above even when waso_percentile = percentile and waso_multiplier = multiplier, because the value of the given percentile of this variable depends on the smoothing window (see waso_window below; aka waso window might not be equal to mov_window)

## waso_window: this is the size of the moving window (in minutes) used in calculating the rolling median that will be used to find periods of wake after sleeping. A waso_window of 1 is the same as using the raw data without a rolling median

## waso_block: this is the number of consecutive minutes of inactivity needed to classify a period as sleep within the sleep period. A waso_block of 1 means that anytime the value is below the threshold, the capuchin in considered sleeping and anytime the value is above the threshold the capuchin is considered awake

## las: las sets las in the plotting window



# this is for Panama: "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
lonlat_to_utm <- function( df, lon_name = 'lon', lat_name = 'lat', crs_utm = CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)){
  library( sp )
  library( rgdal )
  crs_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  df_sp <- SpatialPointsDataFrame(coords = df[, c(lon_name, lat_name)], proj4string = crs_longlat, data = df)
  
  df_sp <- spTransform(df_sp, crs_utm)
  
  df <- as.data.frame(df_sp)
  
  names(df) [ names(df) == paste( lon_name, 1, sep = '.') ] <- 'x'
  names(df) [ names(df) == paste( lat_name, 1, sep = '.') ] <- 'y'
  
  return( df )
}

#read in acc data 
#acc_dat <- fread( "/Users/wilddog/Documents/eneuhaus/Coaticsv/acc_dat.csv")
#acc_dat = acc_dat[-which(acc_dat$tag == "Da Vinci_5764"),]
unique(acc_dat$tag)

################## Read in the d1 (accelerometer burst) data ###################

## d1 is a dataframe with a row for each minute for each capuchin. Each row contains the raw (or interpolated) GPS acc burst, and several different measures calculated from bursts (like VeDBA)
d1 <- fread( "/Users/wilddog/Documents/eneuhaus/Coaticsv/full_night_and_day_data.csv" )

unique(d1$individual_local_identifier)
## turn the data table into a dataframe
d1 <- as.data.frame( d1 )

## turn timestamp into POSIX element and time into character
d1$local_timestamp <- as.POSIXct(as.character(d1$local_timestamp), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
indexes = which(is.na(d1$local_timestamp) == TRUE)
d1 <- fread( "/Users/wilddog/Documents/eneuhaus/Coaticsv/full_night_and_day_data.csv" ) 
d1$local_timestamp[indexes] = paste(d1$local_timestamp[indexes], "00:00:00", sep = " ") # !!!!lost info abt time when data was loaded. we forceibly added that info to avoid NAs when converting to POSIX by changing it while still in character format. if theres a date w no info its midnight.
d1$local_timestamp <- as.POSIXct(as.character(d1$local_timestamp), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )


# round local_timestamps to the nearest minute
d1$local_timestamp <- lubridate::floor_date( d1$local_timestamp, unit = 'minute' )

# recreate the time column with the rounded times
d1$local_time <- str_split_fixed( d1$local_timestamp, ' ', 2 )[ , 2 ]

if (length(which(month(d1$local_timestamp)<12 & month(d1$local_timestamp)>4)) != 0 ) {
  d1=d1[-which(month(d1$local_timestamp)<12 & month(d1$local_timestamp)>4),]}


## assign each minute of data to a given night. A night lasts from noon to noon. First, apply a time shift so that each night is a unit, and not each day
time_shift <- d1$local_timestamp - lubridate::hours(12)

## save the date of the first night of the study (the date of the night is always the date of the evening at the beginning of that night; so the first night of the study is 2015-12-15, although the data starts on 2015-12-14, because the data on that first morning is still technically part of the data for the previous night, as a night is noon to noon)
start_date <- lubridate::date( min( d1$local_timestamp, na.rm=TRUE )- lubridate::hours(12))  #in UTC time. "na.rm=TRUE" needed bc i had one NA value in my column and R cannot run some functions with NAs.

## assign night as number of nights from the start of the study, with all data before the first noon representing night 1
d1$night_num <- as.numeric( as.Date( time_shift, tz = "America/Panama" ) - start_date + 1 )

## show how many coati-nights there are
nrow( unique( d1[ , c( 'individual_local_identifier', 'night' ) ] ) )

## check where the night changes from one night to the next to see if it is at noon
head(d1[ ( which(diff( c( d1$night_num ) ) == 1)) ,])
head(d1[ ( which(diff( c( as.Date( d1$night ) ) ) == 1)) ,])
identical( d1[ ( which(diff( c( d1$night_num ) ) == 1)) ,], d1[ ( which(diff( c( as.Date( d1$night ) ) ) == 1)),] ) #should be true

## view the dataframe and it's summary, its column names and information about the types of columns d1 has.
head(d1)

summary(d1)

names(d1)

str(d1)

# just confirms that there are no duplicate timestamps even after the floor_date rounding above
sum( duplicated( d1[ , c( 'individual_local_identifier', 'local_timestamp' ) ] ) )   #sum is >0 so i have some duplicates.

#delete last and first tagging day 
#via night column, so it doesn't throw out only half of the night 
d1$year <- lubridate::year(d1$night)

d1=d1[-which(d1$night==unique(min(d1$night[which(as.numeric(as.character(d1$year))==2015)], na.rm = TRUE))),]
d1=d1[-which(d1$night==unique(max(d1$night[which(as.numeric(as.character(d1$year))==2016)], na.rm = TRUE))),]
d1=d1[-which(d1$night==unique(min(d1$night[which(as.numeric(as.character(d1$year))==2017)], na.rm = TRUE))),]
d1=d1[-which(d1$night==unique(max(d1$night[which(as.numeric(as.character(d1$year))==2018)], na.rm = TRUE))),]
  
#check it worked. "r there any duplicates in the individuals"
sum( duplicated( d1[ , c( 'individual_local_identifier', 'local_timestamp' ) ] ) )    #sum =/= 0 so means we have more duplicates. i need to find more time duplicates in the dataframe. ask chatgpt: show this line. "i used this line to identify dupes between these 2 columns, there are indeed dupes. how do i find row numbers that correspond to those duplicates?"
#ask chatgpt: what are the actual row numbers? are those true duplicates (same everything,timestamp locations)? or same timestamp or different locations? if truly pure duplicates then u can just remove one, if not we need to figure where they came from. 

############attempts at finding timestamp duplicates between tag and local timestamp column#############

# Assuming d1 is your data frame
duplicates <- duplicated(d1[, c('individual_local_identifier', 'local_timestamp')])
duplicates = which(duplicates == TRUE)

duplicates2 <- as.data.frame(d1[duplicates, c(2,10)])
d1$duplicates = paste(d1$individual_local_identifier, d1$local_timestamp, sep = "_" )
duplicates2$duplicates = paste(duplicates2$individual_local_identifier, duplicates2$local_timestamp, sep = "_" )
length(which(duplicates2$duplicates %in% d1$duplicates == TRUE))
length(unique(duplicates2$duplicates))     
       
evil_duplicates=c()
for(i in 1:length(duplicates2$duplicates)){
  evil_duplicates[i]=list(which(d1$duplicates==duplicates2$duplicates[i]))
} 
gc()

evil_duplicates=unlist(evil_duplicates)
duplicates2 = d1[evil_duplicates, ]
View(duplicates2)
d1=d1[-which(d1$time==""),]
sum(duplicated(d1[, c('individual_local_identifier', 'local_timestamp')]))

duplicates <- duplicated(d1[, c('individual_local_identifier', 'local_timestamp')])
duplicates = which(duplicates == TRUE)
duplicates2 <- as.data.frame(d1[duplicates, c(2,10)])
d1$duplicates = paste(d1$individual_local_identifier, d1$local_timestamp, sep = "_" )
duplicates2$duplicates = paste(duplicates2$individual_local_identifier, duplicates2$local_timestamp, sep = "_" )

evil_duplicates=c()
for(i in 1:length(duplicates2$duplicates)){
  evil_duplicates[i]=list(which(d1$duplicates==duplicates2$duplicates[i]))
}
gc()
evil_duplicates=unlist(evil_duplicates)
duplicates2 = d1[evil_duplicates, ]
View(duplicates2)

min(d1$night[which(d1$individual_local_identifier=="Pliny 4675")])

d1=d1[-which(as.character(d1$night)=="2015-12-23"),]
sum( duplicated( d1[ , c( 'individual_local_identifier', 'local_timestamp' ) ] ) )
# Sort by vector name [z] then [x]
#View(duplicates2[
#  with(duplicates2, order(individual_local_identifier, local_timestamp)), ])

############## Make a dataframe of sunrise and sunset times ###############
GPSdata=read.csv("/Users/wilddog/Documents/eneuhaus/Coaticsv/FFT_Cleaned_Final.csv")  ##Read in GPS data to get location data for sunset and sunrise information 
#GPSdata = GPSdata[-which(GPSdata$individual.local.identifier == "Da Vinci 5764"),]
unique(GPSdata$individual.local.identifier)
ave_lon=mean(GPSdata$location.long, na.rm=TRUE)
ave_lat=mean(GPSdata$location.lat, na.rm=TRUE)

## make an empty dataframe with each date of the study. For each date, we will fill in when sunset occurred, when the dark period began (the end of evening astronomical twilight), when the dark period ended the following morning (the beginning of morning astronomical twilight), and when sunrise the following morning occured
sun_dat <- data.frame( date = c(  seq( from=(min( as.Date( d1$local_timestamp ), na.rm = TRUE ) - 2 ),to = (max( as.Date( d1$local_timestamp ) , na.rm = TRUE) + 1 ), by="1 day"  ), sunset = NA, night_start = NA, night_end = NA, sunrise = NA ))

## fill in sunset time and night start time (dark period start time) on each date with the getsunlighttimes function
sun_dat[, c( 'sunset', 'night_start' ) ] <- getSunlightTimes( date = sun_dat$date, lon = ave_lon, lat = ave_lat )[, c( 'sunset', 'night' ) ]

## fill in rise time and night end time (dark period end time) on each date with those from the following date with the getsunlighttimes function. The reason we are using the following date is because we want to know when a night ended, which happens on the date following the start of that night
sun_dat[, c( 'sunrise', 'night_end' ) ] <- getSunlightTimes( date = ( sun_dat$date + 1 ), lon = ave_lon, lat = ave_lat )[, c( 'sunrise', 'nightEnd' ) ]

## put sun data in local time
sun_dat[ , 2:5 ] <- lubridate::with_tz(sun_dat[ , 2:5 ], tzone = "America/Panama")
sun_dat=data.frame(na.omit(sun_dat))


############## Make a dataframe of moon phases ###############

## save the unique dates associated with the local timestamps in d1
dates <- seq( from=(min( as.Date( d1$local_timestamp ), na.rm = TRUE ) - 2 ),to = (max( as.Date( d1$local_timestamp ) , na.rm = TRUE) + 1 ), by="1 day"  )

## using the getmoonillumination function, get information about the moon on the study dates
moon_dat <- getMoonIllumination( date = c( min( dates ) - 1, dates, max( dates ) + 1 ) )



####################### Determining sleep periods with modification of Van Hees et al. 2018 method ####################################

## save a variable denoting the total number of minutes in the day
mins_in_day <- 60*24

missing_mins <- 120 ## this is the maximum total number of minutes of data that can be missing from a day and still have that day included in the analysis (for sleep period time and sleep based analyses; i.e. not ave_vedba)

night_missing_mins <- 45 ## this is the maximum total number of minutes of data that can be missing from a dark period and still have that day included in the analysis

time_gap <- 20*60 ## this is the maximum allowable time gap between two accelerometer bursts (in seconds) that can exist in a noon-to-noon period without removing this noon-to-noon period from the data

mov_window <- 21 ## this is the size of the moving window (in minutes) used in calculating the rolling median of the average VeDBA ## I had this at 17 due to visual inspection but if we want it to match the validated algorithm, we need to keep it at 9

percentile <- 0.45 ## this is the percentile threshold of the variable (within the noon-to-noon time period) used to classify activity vs. inactivity (VeDBA here)

#multiplier <- 3 ## this is the multiplier of the threshold value determined by the percentile above. Values below the threshold value multiplied by the multiplier are considered inactive and those equal to or above are considered active

block_size <- 30 ## duration in minutes of the blocks of continuous inactivity that will be considered sleep

gap_size <- 45 ## maximum duration between sleep blocks that will be merged ## I had this at 60 due to visual inspection but if we want it to match the validated algorithm, we need to keep it at 45

waso_percentile <- 0.10 ## this is the percentile threshold of the variable (within the noon-to-noon time period) used to classify activity vs. inactivity within the sleep period

waso_multiplier <- 1.125 ## this is the multiplier of the threshold value determined by the percentile above. Values WITHIN the sleep period below the threshold value multiplied by the multiplier are considered inactive and those equal to or above are considered active. This threshold value may be different than the one above even when waso_percentile = percentile and waso_multiplier = multiplier, because the value of the given percentile of this variable depends on the smoothing window (see waso_window below; aka waso window might not be equal to mov_window)

waso_window <- 1 ## this is the size of the moving window (in minutes) used in calculating the rolling median that will be used to find periods of wake after sleeping. A waso_window of 1 is the same as using the raw data without a rolling median

waso_block <- 3 ## this is the number of consecutive minutes of inactivity needed to classify a period as sleep within the sleep period. A waso_block of 1 means that anytime the value is below the threshold, the capuchin in considered sleeping and anytime the value is above the threshold the capuchin is considered awake

nap_block <- waso_block ## this is the number of consecutive minutes of inactivity needed to classify a period as sleep during the waking period. A nap_block of 1 means that anytime the value is below the threshold, the capuchin in considered sleeping and anytime the value is above the threshold the capuchin is considered awake

frag_block <- 2 ## this is the number of minutes of waking that need to be consecutive to be considered a wake bout during the night (other epochs of wake that do not meet this criterion will still be considered wake for WASO and wake_bouts, but not frag_wake_bouts)


sep_day_night <- F ## this determines whether sleep periods and non-sleep periods are separated before finding runs of inactivity to consider as sleep

## shows the time (as well as one previous time and one later time) where a minute is skipped
#sort( unique( d1$local_time ) ) [ which( diff( as_hms( sort( unique( d1$local_time ) ) ) ) != as_hms( '00:01:00' ) ) + -1:1 ]
time_values <- strptime(d1$local_time, format = "%H:%M:%S")
#time_values = (lubridate::hms(d1$local_time))
#time_values <- sprintf('%02d:%02d:%02d', time_values$hour, time_values$minute, time_values$second)
sorted_time_values <- sort(unique(time_values))
skipped_times <- sorted_time_values[which(diff(sorted_time_values) != as.difftime("00:01:00")) + -1:1]

#sort(unique(d1$local_time) )[which (diff( as_hms(sort(unique(d1$local_time)))) != as_hms("00:01:00")) + -1:1] 

## again confirms that every minute is represented in the data except for one (can tell this by comparing this number to the minutes_in_day variable above)
length( unique(d1$local_time) ) #passt

## create a vector containing the names of each coati
tag_names <- unique( d1$individual_local_identifier )

d1$log_vedba <- log( d1$ave_vedba )

## make a copy of d1. We will fill in this new dataframe with information about if the capuchin was asleep in each epoch
#maybe itll be needed to add individual_local_identifier column
full_dat <- d1
d1$dark = NA
full_dat$sleep_per <- NA ## binary indicating whether a row belongs to the sleep period window
full_dat$sleep_per_poly <- NA
full_dat$sleep_bouts <- NA ## binary indicating whether the row is considered sleep, based on the waso or nap requirements
full_dat$n_bursts <- NA ## the number of bursts collected in a given noon-to-noon period (to be compared to the total number of minutes in the day). This column will indicate whether the data for a given night is insufficient to calculate the sleep period (and thus: onset, waking, SPT, sleep_eff, TST, sleep_bouts -- because this depends on a percentile of bursts' log vedba, WASO, wake_bouts, summed_VeDBA, night_VeDBA_corr, dark_TST, prev_naps, prev_day_sleep)
full_dat$max_time_diff <- NA ## the maximum difference between consecutive fixes in a given noon-to-noon period. With the previous column, this column will indicate whether the data is insufficient to calculate the sleep period (and thus: onset, waking, SPT, sleep_eff, TST, WASO, wake_bouts, summed_VeDBA, night_VeDBA_corr, prev_naps )
full_dat$dark <- NA ## a binary indicating whether a row belongs to the period of darkness (between the end of astrological evening twilight and the beginning of astrological morning twilight)
full_dat$poss_dark_bursts <- NA ## the number of potential bursts in the dark period on this night, i.e. the number of minutes between the start and end of the night
full_dat$n_dark_bursts <- NA ## the total number of bursts actually taken during the dark period on this night. This will be compared to the previous column to determine whether the data during the dark period is sufficient to calculate the dark_TST and ave_vedba
full_dat$max_dark_time_diff <- NA ## the maximum difference between consecutive fixes in a given dark period. With the previous column, this column will indicate whether the data is insufficient to calculate the dark_TST and ave_vedba
full_dat$poss_day_bursts <- NA ## the number of potential bursts in preceding day (light) period, i.e. the number of minutes between the end of morning astrological twilight (maybe this should be changed to sunrise?) and the start of evening astrological twilight (maybe this should be changed to sunset?)
full_dat$n_day_bursts <- NA ## the total number of bursts actually taken during the preceding day (light) period. This will be compared to the previous column to determine whether the data during the day period is sufficient to calculate the prev_day_sleep and prev_day_ave_vedba.
full_dat$max_day_time_diff <- NA ## the maximum difference between consecutive fixes in a given day (light) period. With the previous column, this column will indicate whether the data is insufficient to calculate the prev_day_sleep and prev_day_ave_vedba
## prev_naps depends on having sufficient data to calculate both the current SPT as well as the previous day's SPT

## create a vector containing the names of each coati
tag_names <- unique( d1$individual_local_identifier )

options( warn = 2 )


thresh=c()
for( i in 1:length(tag_names) ){
  ## subset the data to just this individual's data
  id_dat <- d1[ which(d1$individual_local_identifier == tag_names[i]), ]
  
  ## create a vector the nights for which this individual has data
  nights <- unique( id_dat$night )
  med_vedba=c()
  for( j in 1:length(nights) ){
    night_dat <- id_dat[ which(id_dat$night == nights[j]), ]
    
    med_vedba[j] <- list(zoo::rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' ))
    
  }
  med_vedba=unlist(med_vedba)
  thresh[i]=list(quantile( med_vedba, percentile, na.rm = T))#* multiplier
}
gc()

## for each individual...
for( i in 1:length(tag_names) ){

  ## subset the data to just this individual's data
  id_dat <- d1[ which(d1$individual_local_identifier == tag_names[i]), ]

  ## create a vector the nights for which this individual has data
  nights <- unique( id_dat$night )

  ## for each night on which this individual has data
  for( j in 1:length(nights) ){
    
    ## subset this individual's data to just that night
    night_dat <- id_dat[ which(id_dat$night == nights[j]), ]

    ## create empty columns for the sleep period and sleep bout binary variables
    night_dat$sleep_per <- NA
    night_dat$sleep_bouts <- NA
    night_dat$sleep_per_poly <- NA

    ## save a column of the total number of bursts for that day. This will also make it easier to remove these days from the dataframe later
    night_dat$n_bursts <- nrow( night_dat )

    ## sort the timestamps (they are probably already sorted)
    sorted_times <- c( '00:00:00', sort( night_dat$local_time ), '23:59:00' )

    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( as_hms( sorted_times ) ), units = 'secs' )

    if( length( time_diffs ) > 0 ){ ### There is one night for one coati with only one single burst, which is why this if statement is needed

      ## save a column of the maximum time difference between burst for that day (this will make it easier to pull out days with insufficient data later)
      night_dat$max_time_diff <- max( unique(as.numeric(time_diffs )))

    }else{

      night_dat$max_time_diff <- NA

    }
    
    
    ## take the rolling median of the log VeDBA and save it as a column
    roll_log_vedba <- zoo::rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' )
    
    if( sum( !is.na( roll_log_vedba ) ) == 0 ) next

    ## determine the threshold activity vs. inactivity threshold based on the percentile, and the rolling median just produced
    #plot(x=night_dat$local_timestamp, roll_log_vedba) UNCOMMENT TO SEE THE PLOTS - THIS IS COMMENTED OUT TO RUN FASTER
    #abline(h=thresh[[i]])

    ### find blocks of continuous inactivity

    ## find the run length encoding of periods above and below the threshold
    temp <- rle(as.numeric( roll_log_vedba < thresh[[i]] ) )

    ## mark the rows that are part of runs (i.e. part of chunks that are greater than the block_size of either continuous activity or continuous inactivity )
    sleep_per_runs <- as.numeric( rep( temp$lengths > block_size, times = temp$lengths ) )

    ## mark the rows corresponding to sleep bouts. These sleep bouts are runs of inactivity
    sleep_per_sleep_bouts <- as.numeric( roll_log_vedba < thresh[[i]] & sleep_per_runs == 1 )
    range(sleep_per_sleep_bouts)
    
    ## find when sleep bouts start and end
    diffs <- diff( c(0, sleep_per_sleep_bouts ) )
    starts <- which( diffs == 1 ) [ -1 ]
    ends <- which( diffs == -1 )

    ## if there are any sleep bouts...
    if( length( which( diffs == 1 ) ) != 0 ){

      ## find the duration of the gaps between each sleep bout (the end of one sleep bout and the start of the next)
      gaps <- as.numeric( night_dat$local_timestamp [ starts ] - night_dat$local_timestamp [ ends[ 1: length( starts ) ] ], units = 'mins' )

      ## sleep bouts separated by gaps that are shorter than that specified by gap_size will be merged. Note which of these gaps are shorter than the gap_size
      inds_to_remove <- which( gaps < gap_size )

      ## if there are NO gaps between sleep bouts that are to be removed...
      if( length( inds_to_remove ) == 0 ){

        ## set sleep onset index to be the start of sleep bouts
        onset <- which( diffs == 1 )

        ## set waking index to be the end of sleep bouts
        wake <- ends

      }else{ ## if there ARE gaps between sleep bouts that are to be removed...

        ## set sleep onset index to be the start of sleep bouts that do not correspond to the gaps to be removed (because these will be within sleep periods, not a start of a new bout)
        onset <- which( diffs == 1 ) [ - (inds_to_remove + 1) ]

        ## set waking index to be the end of sleep bouts that do not correspond to the gaps to be removed
        wake <- ends [ - inds_to_remove ]

      }

      ## determine which sleep period is the longest
      per_ind <- which.max( as.numeric( night_dat$local_timestamp[ wake ] - night_dat$local_timestamp[ onset ], units = 'secs' ) )

      ## fill in the sleep period data frame with the sleep onset and waking time associated with the longest sleep period in the day (noon to noon)

      night_dat$sleep_per <- as.numeric( night_dat$local_timestamp >= night_dat$local_timestamp[ onset[ per_ind ] ] & night_dat$local_timestamp <= night_dat$local_timestamp[ wake[ per_ind ] ] )

      #abline(v=night_dat$local_timestamp[ onset[ per_ind ] ])
     #abline(v=night_dat$local_timestamp[ wake[ per_ind ] ])
      
      for( ons in 1:length( onset ) ){
        
        night_dat$sleep_per_poly <- as.numeric( night_dat$local_timestamp >= night_dat$local_timestamp[ onset[ ons ] ] & night_dat$local_timestamp <= night_dat$local_timestamp[ wake[ ons ] ] )
        
        #abline( v = c( night_dat$local_timestamp[ onset[ ons ] ], night_dat$local_timestamp[ wake[ ons ] ] ), col = 'orange' )

      }
      
    }else{ ## if there aren't any sleep bouts, record all rows as a 0 in the sleep_per column

      night_dat$sleep_per <- 0

    }

    ## take the rolling median of the log VeDBA
    night_dat$roll_log_vedba <- zoo::rollmedian( night_dat$log_vedba, waso_window, fill = NA, align = 'center' )

    ### find blocks of continuous inactivity

    
    ## find the run length encoding of periods above and below the threshold
    temp <- rle( as.numeric( night_dat$roll_log_vedba < thresh[[i]] ) )
    
    ## mark which rows are part of runs of activity vs. inactivity, as determined by waso_block
    runs <- as.numeric( rep( temp$lengths >= waso_block, times = temp$lengths ) )
    
    sleep_bouts <- as.numeric( night_dat$roll_log_vedba < thresh[[i]] & runs == 1 )
    
    ## make which rows are part of runs of inactivity. These are the periods of sleep within the sleep period
    night_dat$sleep_bouts <- sleep_bouts
    
    #print( max( sleep_bouts ) ) UNCOMMENT AFTER
    #print( max( night_dat$sleep_bouts ) )

    dark_start <- sun_dat$night_start[ which(sun_dat$date == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) ]

    dark_end <- sun_dat$night_end[ which(sun_dat$date == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) ]

    night_dat$dark <- as.numeric( night_dat$local_timestamp >= dark_start & night_dat$local_timestamp <= dark_end )

    night_dat$poss_dark_bursts <- floor( as.numeric( dark_end - dark_start, units = 'mins' ) )

    night_dat$n_dark_bursts <- sum( night_dat$local_timestamp >= dark_start & night_dat$local_timestamp <= dark_end )

    ## sort the timestamps (they are probably already sorted)
    sorted_times <- c( dark_start, sort( night_dat$local_timestamp[ which(night_dat$dark == 1)] ), dark_end )

    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( sorted_times ), units = 'secs' )

    if( length( time_diffs ) != 0 ){
      ## save the maximum time difference during the night
      night_dat$max_dark_time_diff <- max( time_diffs )  ## so if this variable is NA, that means that there are no bursts taken from the nighttime period on this day for this individual
    }else{

      night_dat$max_dark_time_diff <- NA
    }



    first_light <- sun_dat$night_end[ which(sun_dat$date == ( as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )  - 1 )) ]

    night_dat$poss_day_bursts <- floor( as.numeric( dark_start - first_light, units = 'mins' ) )

    day_dat <- id_dat[ which(id_dat$local_timestamp > first_light & id_dat$local_timestamp < dark_start), ]

    night_dat$n_day_bursts <- nrow( day_dat )


    ## sort the timestamps (they are probably already sorted)
    sorted_times <- c( first_light, sort( day_dat$local_timestamp ), dark_start )

    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( sorted_times ), units = 'secs' )

    if( length( time_diffs ) != 0 ){
      ## save the maximum time difference during the night
      night_dat$max_day_time_diff <- max( time_diffs )  ## variable is NOT NA!! but if this variable is NA, that means that there are no bursts taken from the nighttime period on this day for this individual
    }else{

      night_dat$max_day_time_diff <- NA
    }



    night_dat <- as.data.frame(night_dat)
    full_dat=as.data.frame(full_dat)
    #which(names( night_dat ) == 'roll_log_vedba' )
    night_dat <- night_dat[ ,  -which(names( night_dat ) == 'roll_log_vedba' )]


    night_dat <- night_dat[ , names( full_dat ) ]
    
    ### put the night data back into full_dat
    tempind=which(full_dat$individual_local_identifier == tag_names[i] & full_dat$night == nights[j])
    full_dat[tempind,  ] <- night_dat

    
    if( max( full_dat$sleep_bouts, na.rm = T ) > 1 ) stop( 'asdf' )
  }

}
gc()

pre_clean_full <- full_dat

study_nights <- as.Date( min( d1$night):max( d1$night ), origin = "1970-01-01", tz = "America/Panama" )

day_lim_start_time <- "07:30:00"

day_lim_end_time <- "17:30:00"


sleep_per <- data.frame( tag = rep( unique( d1$individual_local_identifier ), each = length( study_nights ) ), night = rep( study_nights, times = length( unique( d1$individual_local_identifier ))), onset = NA, waking = NA, SPT = NA, WASO = NA, TST = NA, TST_in_median_SPT = NA, TST_at_sleep_site = NA, sleep_eff = NA, wake_bouts = NA, frag_wake_bouts = NA, frag_wake_bouts_in_median_SPT = NA, frag_wake_bouts_at_sleep_site = NA, summed_VeDBA = NA, night_VeDBA_corr = NA, ave_vedba = NA, dark_TST = NA, light_TST = NA, dark_sleep_eff = NA, light_sleep_eff = NA, prev_naps = NA, prev_day_sleep = NA, prev_day_sleep_lim = NA, prev_day_ave_vedba = NA, poss_dark_bursts = NA, n_dark_bursts = NA, poss_day_bursts = NA, n_day_bursts = NA, max_time_diff = NA, n_bursts= NA, max_dark_time_diff = NA, max_day_time_diff = NA, SPT_poly = NA, TST_poly = NA )


## create empty vectors for the durations of sleep and wake bouts. We will fill these in to see if the distributions of the durations of these bouts later
sleep_durs <- c()
wake_durs <- c()

## for each individual...
for( i in 1:length(tag_names) ){
  print(tag_names[i])

  ## subset the data to just this individual's data
  id_dat <- full_dat[ full_dat$individual_local_identifier == tag_names[i], ]

  ## create a vector the nights for which this individual has data
  nights <- unique( id_dat$night )

  ## for each night on which this individual has data
  pb = txtProgressBar(min = 0, max = length(nights),  style = 3, char="=") 
  for( j in 1:length(nights) ){

    ## subset this individual's data to just that night
    night_dat <- id_dat[ id_dat$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" ), ]
    
    if( sum(is.na(night_dat$max_time_diff)) == length(night_dat$max_time_diff)){              #skips this night if the column has NAs which is the case for fonta flora individual
      print(paste("max_time_diff is empty for", tag_names[i], sep=" "))
      next
    }
    
    ## should already be in order, but just in case
    night_dat <- night_dat[ order( night_dat$local_timestamp ), ]

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$n_bursts <- unique( night_dat$n_bursts, na.rm = TRUE )

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$max_time_diff <- max(unique( night_dat$max_time_diff , na.rm = TRUE), na.rm = TRUE)

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$poss_dark_bursts <- unique( night_dat$poss_dark_bursts, na.rm = TRUE )

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$n_dark_bursts <- unique( night_dat$n_dark_bursts, na.rm = TRUE )

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$max_dark_time_diff <- max(unique( night_dat$max_dark_time_diff, na.rm = TRUE ), na.rm = TRUE)

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$poss_day_bursts <- unique( night_dat$poss_day_bursts, na.rm = TRUE )

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$n_day_bursts <- unique( night_dat$n_day_bursts, na.rm = TRUE )

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$max_day_time_diff <- unique( night_dat$max_day_time_diff, na.rm = TRUE )


    SPT_dat <- night_dat[ which(night_dat$sleep_per == 1), ]

    SPT_dat_poly <- night_dat[ which(night_dat$sleep_per_poly == 1), ]
    
    
    if( nrow( SPT_dat ) > 0 ){

      onset <- min( SPT_dat$local_timestamp )

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$onset <- as.character(onset)

      waking <- max( SPT_dat$local_timestamp, na.rm = TRUE )

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$waking <- as.character(waking)

      SPT <- as.numeric( waking - onset, units = 'mins' ) + 1 # SPT = sleep period time (duration of the sleep period)

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$SPT <- SPT

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$SPT_poly <- nrow( SPT_dat_poly )
      
      WASO <- sum( SPT_dat$sleep_bouts == 0 ) # WASO = wake after sleep onset

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$WASO <- WASO

      TST <- sum( SPT_dat$sleep_bouts == 1 )

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$TST <- TST

      
      TST_poly <- sum( SPT_dat_poly$sleep_bouts == 1 )
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$TST_poly <- TST_poly
      
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$sleep_eff <- TST/ nrow( SPT_dat )

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$summed_VeDBA <- sum( SPT_dat$log_vedba, na.rm = TRUE)

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$night_VeDBA_corr <- sum( SPT_dat$log_vedba, na.rm = TRUE ) / SPT

      temp <- rle( SPT_dat$sleep_bouts )

      runs <- as.numeric( rep( temp$lengths >= frag_block, times = temp$lengths ) )

      frag_wake_bouts <- as.numeric( SPT_dat$sleep_bouts == 0 & runs == 1 )

      diffs <- diff( c( 1, frag_wake_bouts ) )

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$frag_wake_bouts <- sum( diffs == 1, na.rm = TRUE )

      ## find the distinct sleep bouts (i.e. epochs of sleep separated by waking)
      diffs <- diff( c( 0, SPT_dat$sleep_bouts ) )

      ## save the number of distinct wake bouts
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$wake_bouts <- sum( diffs == -1 , na.rm = TRUE)

      ## find durations of sleep and wake bouts
      temp <- rle( SPT_dat$sleep_bouts )

      ## add the duration of sleep bouts to the sleep bout duration vector
      sleep_durs <- c( sleep_durs, temp$lengths[ which(temp$values == 1) ] )
      ## add the duration of wake bouts to the wake bout duration vector
      wake_durs <- c( wake_durs, temp$lengths[ which(temp$values == 0 )] )


      waking_dat <- id_dat[ which(id_dat$local_timestamp > unique(sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == ( as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" ) - 1 )), ]$waking) & id_dat$local_timestamp <  onset   ), ]

      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$prev_naps <- sum( waking_dat$sleep_bouts , na.rm = TRUE)


    }


    dark_dat <- night_dat[ which(night_dat$dark == 1), ]

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$ave_vedba <- mean( na.omit(dark_dat$log_vedba))

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$dark_TST <- sum( dark_dat$sleep_bouts == 1 , na.rm = TRUE)

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$dark_sleep_eff <- sum( na.omit(dark_dat$sleep_bouts == 1)) / nrow( dark_dat )

    light_dat <- night_dat[ which(night_dat$dark == 0 & night_dat$sleep_per == 1), ]

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$light_TST <- sum( light_dat$sleep_bouts == 1 , na.rm = TRUE)

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$light_sleep_eff <- sum( na.omit(light_dat$sleep_bouts == 1) ) / nrow( dark_dat )


    first_light <- sun_dat$night_end[ which(sun_dat$date == ( as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" ) - 1 )) ]

    last_light <- sun_dat$night_start[ which(sun_dat$date == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )) ]

    day_dat <- id_dat[ which(id_dat$local_timestamp > first_light & id_dat$local_timestamp < last_light), ]

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$prev_day_sleep <- sum( day_dat$sleep_bouts , na.rm = TRUE)

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$prev_day_ave_vedba <- mean( day_dat$log_vedba , na.rm = TRUE)

    day_lim_start <- as.POSIXct( paste( str_split_fixed( first_light, " ", 2 )[ , 1 ], day_lim_start_time ), tz = 'America/Panama' )

    day_lim_end <- as.POSIXct( paste( str_split_fixed( last_light, " ", 2 )[ , 1 ], day_lim_end_time ), tz = 'America/Panama' )

    day_lim <- id_dat[ which(id_dat$local_timestamp > day_lim_start & id_dat$local_timestamp < day_lim_end), ]

    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$night == as.Date( nights[j], origin = "1970-01-01", tz = "America/Panama" )), ]$prev_day_sleep_lim <- sum( day_lim$sleep_bouts )
    setTxtProgressBar(pb,j)
  }
  close(pb)
  
}
gc()


sleep_per

# make a column for the number of nights since the beginning of the study, just for easy notation and subsetting later
sleep_per$night_num <- as.numeric( sleep_per$night - min( sleep_per$night ) + 1 )

## reformat sleep timestamp
sleep_per$onset <- as.POSIXct( sleep_per$onset, origin = "1970-01-01 00:00:00", tz = "America/Panama" )

## reformat waking timestamp
sleep_per$waking <- as.POSIXct( sleep_per$waking, origin = "1970-01-01 00:00:00", tz = "America/Panama" )

checkdates=data.frame(unique(cbind(acc_dat$individual_local_identifier, as.character(acc_dat$night))))
colnames(checkdates)=c("tag","night")
checkdates$night <- as.POSIXct( checkdates$night, origin = "1970-01-01", tz = "America/Panama" )

sleep_per_save <- sleep_per[which(paste(sleep_per$tag,sleep_per$night)%in%paste(checkdates$tag,checkdates$night)==TRUE),]
sleep_per <- sleep_per[which(paste(sleep_per$tag,sleep_per$night)%in%paste(checkdates$tag,checkdates$night)==TRUE),]

########### Add in moon phase data ###########

sleep_per <- merge( x = sleep_per, y = moon_dat, by.x = 'night', by.y = 'date', all.x = T, all.y = F, sort = F )

sum( !is.na( sleep_per$SPT ) )

### check number of nights for which sleep period was calculated and inspect those for which no sleep period was calculated ###

sleep_per_nona <- sleep_per[ !is.na( sleep_per$SPT ), ]


############# Cleaning the dataframes of data on nights with insufficient data ################

## for days on the later side of a noon-to-noon period with a lot of missing data, we can't have a reliable threshold for what was considered sleep in the morning, and we may be missing a lot of epochs. Therefore, remove: sleep time during the day (day defined by SPT (wake time to onset time)), sleep time during the day (day defined as prev dark period end to dark period start), sleep time during the day (defined as 0730 to 1730)

sleep_per[ paste( sleep_per$tag, sleep_per$night, sep = '_' ) %in% paste( sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), 'tag' ], ( sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ) , 'night' ] + 1 ), sep = '_' ), c( 'prev_naps', 'prev_day_sleep', 'prev_day_sleep_lim' ) ] <- NA

## for days on the later side of a noon-to-noon period with large gaps of missing data, we can't have a reliable waking time. Therefore, remove: sleep time during the day (day defined by SPT (wake time to onset time))
sleep_per[ paste( sleep_per$tag, sleep_per$night, sep = '_' ) %in% paste( sleep_per[ sleep_per$max_time_diff > time_gap & !is.na( sleep_per$max_time_diff ), 'tag' ], ( sleep_per[ sleep_per$max_time_diff > time_gap & !is.na( sleep_per$max_time_diff ) , 'night' ] + 1 ), sep = '_' ), c( 'prev_naps' ) ] <- NA

## remove all these variable from the night, and from the days on the early side of the noon-to-noon period if the noon-to-noon period is missing a lot of data (because then we might not be able to reliably calculate the sleep VeDBA threshold, and a lot of epochs might be missing, which would skew TST and such)
sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), c( 'onset', 'waking', 'SPT', 'sleep_eff', 'TST', 'WASO', 'wake_bouts', 'summed_VeDBA', 'night_VeDBA_corr', 'dark_TST', 'dark_sleep_eff', 'light_TST', 'light_sleep_eff', 'prev_naps', 'prev_day_sleep', 'prev_day_sleep_lim', 'TST_at_sleep_site', 'frag_wake_bouts', 'frag_wake_bouts_in_median_SPT', 'frag_wake_bouts_at_sleep_site' ,'SPT_poly', 'TST_poly') ] <- NA


sleep_per_nona <- sleep_per[ !is.na( sleep_per$SPT ), ]

nrow( sleep_per_nona )


## remove all these variable from the night, and from the days on the early side of the noon-to-noon period (only for those depending on SPT) if the noon-to-noon period has large gaps of missing data (because then we can't reliably calculate the SPT)
sleep_per[ sleep_per$max_time_diff > time_gap & !is.na( sleep_per$max_time_diff ), c( 'onset', 'waking', 'SPT', 'sleep_eff', 'TST', 'WASO', 'wake_bouts', 'summed_VeDBA', 'light_TST', 'light_sleep_eff', 'night_VeDBA_corr', 'prev_naps', 'TST_in_median_SPT', 'TST_at_sleep_site', 'frag_wake_bouts', 'frag_wake_bouts_at_sleep_site','SPT_poly', 'TST_poly' ) ] <- NA

sleep_per_nona <- sleep_per[ !is.na( sleep_per$SPT ), ]

nrow( sleep_per_nona )

## remove data for sleep period and sleep bouts on days when there is a lot of missing data, because we cannot reliably calculate the sleep VeDBA threshold and there may be a lot of missing epochs
full_dat[ which(full_dat$n_bursts < ( mins_in_day - missing_mins )), c( 'sleep_per', 'sleep_bouts' ) ] <- NA

## remove data for sleep period on days when there are large gaps of missing data, becasue we can't reliably calculate the SPT with gaps in the data
full_dat[ which(full_dat$max_time_diff > time_gap), 'sleep_per'  ] <- NA


## make columns for just the time part of the sleep onset and waking timestamps
sleep_per$onset_time <- as_hms( sleep_per$onset )
sleep_per$waking_time <- as_hms( sleep_per$waking )

### save the created dataframes as csv
write.csv( sleep_per, '/Users/wilddog/Documents/eneuhaus/Coaticsv/sleep_per425_all.csv', row.names = F )
write.csv( full_dat, '/Users/wilddog/Documents/eneuhaus/Coaticsv/full_dat425_all.csv', row.names = F )
write.csv( sleep_per_nona, '/Users/wilddog/Documents/eneuhaus/Coaticsv/sleep_per_nona425_all.csv', row.names = F )

## look at TST 
hist( sleep_per$TST, breaks = 100, xlim = c( 0, 900 ) )

for( tag in unique( sleep_per$tag ) ) {

  tag_dat <- sleep_per[ sleep_per$tag == tag, ]

  hist( tag_dat$TST, breaks = 100, xlim = c( 0, 900 ), main = tag )
}
gc()






