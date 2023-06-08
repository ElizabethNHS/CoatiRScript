##loading the libraries
library(data.table)
install.packages("ggplot2")
library(ggplot2)


##reading in our csv files
sleep_per_nona <- fread("/Users/wilddog/Documents/eneuhaus/Coaticsv/sleep_per_nona425_all.csv")
temp= read.csv("/Users/wilddog/Documents/eneuhaus/WEATHER/AirTemperature/bci_cl_at_elect.csv")     #CLEARANCE interval average temp
humidity <- fread( "/Users/wilddog/Documents/eneuhaus/WEATHER/BCIweather/LutzTower/RelativeHumidity/1m/bci_lutz01m_rh_elect.csv") #LUTZ TOWER interval average rh 1m above ground. uses data from lutz 20om
rain <- fread("/Users/wilddog/Documents/eneuhaus/WEATHER/Rain/bci_cl_ra_elect.csv") #CLEARANCE rain records for the entire period

### if date and time are separate columns, make new column using paste() 
##rain$timestamp=paste(rain$date, rain$time, sep=" ")
#EST same as America/Panama
#UTC is not local time
#lubridate::with_tz() to shift between EST and UTC

## Initialize columns
sleep_per_nona$rain <- NA
sleep_per_nona$temp <- NA
sleep_per_nona$humidity <- NA
sleep_per_nona$onset_hour <- NA

## Adding the hours that coatis fall asleep as a new column
class(sleep_per_nona$onset)
sleep_per_nona$onset_hour = lubridate::hour(sleep_per_nona$onset)
sleep_per_nona$onset_hour = lubridate::hour(sleep_per_nona$onset) + ((lubridate::minute(sleep_per_nona$onset))/60) + (((lubridate::second(sleep_per_nona$onset))/60)/60) #extract minute and second from timestamp and dividing it by 60 ONCE and TWICE to convert the minutes and seconds to decimals. result is DECIMAL TIME and this is plottable, time format makes it hard to plot
class(sleep_per_nona$onset_hour)

## converting to date class and filtering years
rain$date = as.Date(rain$date, format=c("%d/%m/%Y"))
rain = rain[which(rain$date >= as.Date("2015/12/01", format=c("%Y/%m/%d"))), ]     #square brackets are used to access sth inside of an object or dataframe. meanwhile () are used for a function

humidity$date = as.Date(humidity$date, format=c("%d/%m/%Y"))
humidity = humidity[which(humidity$date >= as.Date("2015/12/01", format=c("%Y/%m/%d"))), ]

temp$date = as.Date(temp$date, format=c("%d/%m/%Y"))
temp = temp[which(temp$date >= as.Date("2015/12/01", format=c("%Y/%m/%d"))), ]

## converting to the POSIX elements and into the right timezone.
rain$local_datetime <- as.POSIXct(x= rain$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz="America/Panama" ) ## turns the timestamp into a POSIX element. doesnt work, creates a column full of NAs.
class(rain$local_datetime)

humidity$local_datetime <- as.POSIXct(x= humidity$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz="America/Panama" ) ## turns the timestamp into a POSIX element. doesnt work, creates a column full of NAs.
class(humidity$local_datetime)

temp$local_datetime <- as.POSIXct(x= temp$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz="America/Panama" ) ## turns the timestamp into a POSIX element. doesnt work, creates a column full of NAs.
class(temp$local_datetime)

## only keeping good and adjusted
rain = rain[which(rain$chk_note =="good" | rain$chk_note == "adjusted"), ]
humidity = humidity[which(humidity$chk_note =="good" | humidity$chk_note == "adjusted"), ]
temp = temp[which(temp$chk_note =="good" | temp$chk_note == "adjusted"), ]
#filtered_temp <- subset(temp, CHK_NOTE %in% c("adjusted", "good"))

## Calculate sum of rain data
for (i in 1:nrow(sleep_per_nona)) {
  start <- sleep_per_nona$onset[i]
  end <- sleep_per_nona$waking[i]
  rainvalues <- rain$ra[which(rain$local_datetime > start & rain$local_datetime <= end)]
  rainvalues <- sum(rainvalues, na.rm = TRUE)
  sleep_per_nona$rain[i] <- rainvalues
}

## Calculate mean of humidity data
for (i in 1:nrow(sleep_per_nona)) {
  start <- sleep_per_nona$onset[i]
  end <- sleep_per_nona$waking[i]
  humidityvalues <- humidity$rh[which(humidity$local_datetime > start & humidity$local_datetime <= end)]
  humiditymean <- mean(humidityvalues, na.rm = TRUE)
  sleep_per_nona$humidity[i] <- humiditymean
}

## Calculate mean of temperature data
for (i in 1:nrow(sleep_per_nona)) {
  start <- sleep_per_nona$onset[i]
  end <- sleep_per_nona$waking[i]
  tempvalues <- temp$at[which(temp$local_datetime > start & temp$local_datetime <= end)]
  tempmean <- mean(tempvalues, na.rm = TRUE)
  sleep_per_nona$temp[i] <- tempmean
}

#save after running
##Plot histograms of SPT, TST, sleep_eff, rain, temp, humidity
#either use hist() or ggplot. plot(density(sleeppernona$TST)) and do the same for the rest. for all individuals at once. using plot(density()) instead of hist(,breaks) for better visuals

## SPT plot (sleep period time, total duration of sleep period). SPT not SPT_poly!!
plot(density(sleep_per_nona$SPT),
     main = "Density plot of sleep period time (SPT) for all individuals",
     xlab = "SPT values"
     )

## TST plot (not poly!)
plot(density(sleep_per_nona$TST),
     main = "Density plot of the total sleep time for all individuals",
     xlab = "TST values"
)

## sleep efficiency plot
plot(density(sleep_per_nona$sleep_eff),
     main = "Density plot of sleep efficiency for all individuals",
     xlab = "Sleep efficiency"
)

## rain plot
plot(density(sleep_per_nona$rain),
     main = "Density plot of the rain sum at BCI for all individuals",
     xlab = "Rain sum values in mm"
)

## temperature plot
plot(density(sleep_per_nona$temp),
     main = "Density plot of the mean BCI temperature for all individuals",
     xlab = "Mean temperature values"
)

## humidity plot
plot(density(sleep_per_nona$humidity),
     main = "Density plot of the mean relative humidity at BCI for all individuals",
     xlab = "Relative humidity"
)

## sleep onset times plot
plot(density(sleep_per_nona$onset_hour),
     main = "Density plot of the onset hours for all individuals",
     xlab = "Onset hour values"
)

## scatter plots of all our variables against onset hour to check for effect of onset hour.
plot(x = sleep_per_nona$onset_hour,
     y = sleep_per_nona$SPT,
     main = "SPT over onset hour")

plot(x = sleep_per_nona$onset_hour,
     y = sleep_per_nona$TST,
     main = "TST over onset hour")

plot(x = sleep_per_nona$onset_hour,
     y = sleep_per_nona$sleep_eff,
     main = "Sleep efficiency over onset hour")

plot(x = sleep_per_nona$onset_hour,
     y = sleep_per_nona$rain,
     main = "Sum of rain values over onset hour")

plot(x = sleep_per_nona$onset_hour,
     y = sleep_per_nona$temp,
     main = "Mean of BCI average temperature over onset hour")

plot(x = sleep_per_nona$onset_hour,
     y = sleep_per_nona$humidity,
     main = "Mean of BCI relative humidity over onset hour")

#plotting rain vs temp
plot(x = sleep_per_nona$rain,
     y = sleep_per_nona$temp,
     main = "Plot of recorded rain values against temperature at BCI")

#rain vs humidity
plot(x = sleep_per_nona$rain,
     y = sleep_per_nona$humidity,
     main = "Plot of recorded rain values against relative humidity at BCI")

#temp vs humidity
plot(x = sleep_per_nona$temp,
     y = sleep_per_nona$humidity,
     main = "Plot of temperature against relative humidity measured at BCI")

#################################################################################
## plotting the ChatGPT way
plot_data <- sleep_per_nona[, c("SPT", "TST", "sleep_eff", "rain", "temp", "humidity", "tag")]

plot <- ggplot(data = plot_data)

plot <- plot +
  geom_density(aes(x = SPT, fill = tag), alpha = 0.5) +
  geom_density(aes(x = TST, fill = tag), alpha = 0.5) +
  geom_density(aes(x = sleep_eff, fill = tag), alpha = 0.5) +
  geom_density(aes(x = rain, fill = tag), alpha = 0.5) +
  geom_density(aes(x = temp, fill = tag), alpha = 0.5) +
  geom_density(aes(x = humidity, fill = tag), alpha = 0.5)

plot <- plot +
  xlab("Value") +
  ylab("Density") +
  ggtitle("Density Plots of Sleep Variables by Individual")

print(plot)