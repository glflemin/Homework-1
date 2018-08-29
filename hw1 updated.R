install.packages("readxl")

library(readxl)
rm(list=ls())

G_561_T <-read_excel("C:\\Users\\molly\\OneDrive\\Desktop\\G-561_T.xlsx", sheet= "Well")
View(G_561_T)



install.packages("haven") 
library("haven")

install.packages("SASxport") 
library("SASxport")


#install.packages("lubridate")

#Library to manipulate dates

library (lubridate)



#initialize lists for holding date-times and well depth data for first chuck of data (hourly section)

datetime1 = list(Sys.time())

well1 = list(0)



#Loop through hourly data and combine date and time into a single variable

for (i in 1:90799){
  
  ye <- toString(year(G_561_T$date[i]))
  
  mo <- toString(month(G_561_T$date[i]))
  
  da <- toString(day(G_561_T$date[i]))
  
  ho <- toString(hour(G_561_T$time[i]))
  
  datet <- paste(ye, mo, da, ho, sep = " ")
  
  datetime1[[i]] <- as.POSIXct(datet, format="%Y %m %d %H")
  
}



#Isolate well depth data for hourly data

well1 = G_561_T$Corrected[1:90799]



#Initialize counter variables and lists for the 15-min increment data

j=1

k=1

depths = c(0, 0, 0, 0)    #vector variable that holds the depths to be averaged for the hour

well2 <- list(0)

hold <- c(year(G_561_T$date[90800]), month(G_561_T$date[90800]), day(G_561_T$date[90800]), hour(G_561_T$time[90800]))  #initialize first date/hour

datetime2 = list(Sys.time())



#Loop through 15-min data

for (i in 90800:101547){
  
  current <- c(year(G_561_T$date[i]), month(G_561_T$date[i]), day(G_561_T$date[i]), hour(G_561_T$time[i]))  #get current date/hour
  
  if (hold[1] == current[1] & hold[2] == current[2] & hold[3] == current[3] & hold[4] == current[4]){   #check if hold and current date/hour are the same
    
    depths[j] <- G_561_T$Corrected[i]   #if the date/hour are the same, add the well depth to list of depths for the hour
    
    j=j+1
    
  }
  
  if (i == 101547){                     #if the loop is at the last entry, calculate average depth and add corresponding datetime to list
    
    well2[k] <- sum(depths)/(j-1)
    
    ye <- toString(year(G_561_T$date[i]))
    
    mo <- toString(month(G_561_T$date[i]))
    
    da <- toString(day(G_561_T$date[i]))
    
    ho <- toString(hour(G_561_T$time[i]))
    
    datet <- paste(ye, mo, da, ho, sep = " ")
    
    datetime2[[k]] <- as.POSIXct(datet, format="%Y %m %d %H")
    
    
    
  } else if (hour(G_561_T$time[i]) != hour(G_561_T$time[i+1])){    #if the next date/hour is different, average depth, record date/hour, and reset appropriate variables
    
    hold <- c(year(G_561_T$date[i+1]), month(G_561_T$date[i+1]), day(G_561_T$date[i+1]), hour(G_561_T$time[i+1]))
    
    well2[k] <- sum(depths)/(j-1)
    
    j=1
    
    depths <- c(0,0,0,0)
    
    ye <- toString(year(G_561_T$date[i]))
    
    mo <- toString(month(G_561_T$date[i]))
    
    da <- toString(day(G_561_T$date[i]))
    
    ho <- toString(hour(G_561_T$time[i]))
    
    datet <- paste(ye, mo, da, ho, sep = " ")
    
    datetime2[[k]] <- as.POSIXct(datet, format="%Y %m %d %H")
    
    k=k+1
    
  }
  
}



#combine the two halves of the dates and well depths

well <- unlist(c(well1, well2))

datetime <- c(datetime1, datetime2)



#combine the two variables into a dataframe

new_well_data <- data.frame(as.POSIXct(unlist(datetime), origin = "1970-01-01"), well)

View(new_well_data)

#calculate mean and stdev of well variable

meandepth = mean(well)

stdevdepth = sd(well)



#bills final results

View(new_well_data)

#creating dataset of all dates and hours
#, tz="EST", isdst
blank <-data.frame(seq(
  from=as.POSIXct("2007-10-5 0:00"),
  to=as.POSIXct("2018-6-12 23:00"),
  by="hour"
) )
colnames(blank) <- ("datetime")
View(blank)
dim(blank)

colnames(new_well_data) <- c("datetime", "well")


final <- left_join(blank, new_well_data,"datetime")


#final1<-semi_join(blank, new_well_data, "datetime")

View(final)


str(blank)
str(new_well_data)
str(justNA)

justNA <- final %>%
  filter(is.na(well)) 

View(justNA)


write_sas(blank, "blank.sas7bdat")
write_sas(new_well_data, "new_well_data.sas7bdat")


