#Library to manipulate dates and excel and dataframes
#if you don't have any of these, install them
library(readxl)
library(lubridate)
library(dplyr)
# for cleaning global environment
#rm(list=ls())

# ensuring correct work directory
setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")
getwd()
dir("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework")

# importing the Excel file
wbpath <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Time Series\\Homework\\G_561_T.xlsx"
G_561_T <- read_excel(wbpath, sheet=3) # need the full filepath to make this work

#building ideal vector of dates
#necessary to convert it to data.frame for merge later
vdate <- data.frame(seq(
  from=as.POSIXct("2007-10-05 0:00", tz="EST"),
  to=as.POSIXct("2018-06-13 0:00", tz="EST"),
  by="hour"
) )
length(vdate[[1]])
colnames(vdate) <- 'date_time' #renaming vector to match other merge datafram

# Rounding dates to their hour, Frankensteining date & hour together, then converting to POSIX
# Assumes all times are EST
# TODO: Correct EST assumption for timestamps in EDT 
G_561_T$date_time <- paste(G_561_T$date," ", lubridate::hour(G_561_T$time),":00:00", sep = "")
G_561_T$date_time <- as.POSIXct(G_561_T$date_time, tz="EST")

#grouping water levels by taking average of each hour
clean_well <- G_561_T %>%
  group_by(date_time) %>%
  summarise(mean_corr=mean(Corrected)) %>%
  select(date_time, mean_corr)

#THE MERGES
#If you didn't run 'Bill's Code.R' prior to this, you'll hit errors here.
final_P <- left_join(vdate, clean_well, by='date_time')
final_J <- left_join(vdate, new_well_data, by='date_time')



length(new_well_data$well) #Jenista 93486
length(vdate[[1]]) #Ideal 93697
length(clean_well$mean_corr) #Powell 93442
length(vdate[[1]]) -length(new_well_data$well) #211
length(vdate[[1]])-length(clean_well$mean_corr) #255
length(final$mean_corr)-length(vdate[[1]]) #confirming merge created expected length
View(final)


# Taking summaries of both datasets for comparison
# Powell Dataset...taking averages of averages in the mean function, but whatever
final_P_summary <- final_P %>%
  group_by(year=lubridate::year(date_time), month=lubridate::month(date_time)) %>%
  summarize(mean = mean(mean_corr, na.rm = TRUE), count = n())

# Jenista Dataset
final_J_summary <- final_J %>%
  group_by(year=lubridate::year(date_time), month=lubridate::month(date_time)) %>%
  summarize(mean = mean(well, na.rm = TRUE), count = n())


#subtracting: Jenista - Powell to identify areas of error
comparison <- final_P_summary - final_J_summary
View(comparison)
sum(comparison$count) # distributed diffences match the previous difference(sum=44)
