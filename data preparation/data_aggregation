################################################################################
################################################################################
###############                                                  ###############
###############     ######       #     ##### ####      #         ###############
###############     #     #     # #    #     #   #    # #        ###############
###############     ########   #   #   ###   #   #   #   #       ###############
###############     #      #  #######  #     #   #  #######      ###############
###############     ######## #       # ##### ####  #       #     ###############
###############                                                  ###############
################################################################################
################################################################################

###############                 data aggregation                 ###############

rm(list = ls())
library(plyr)
library(lubridate)

#load dataset
path<-"C:/Users/Dan/Documents/GitHub/Manual/data/"
df<-read.csv2(paste0(path,"data.csv"),sep=";",dec=",")

#set the date time format as POSIXct
df$date_time <- as.POSIXct(df$date_time, origin = "1970-01-01 00:00:00",
                           format = "%Y-%m-%d %H:%M:%S",
                           tz = "Etc/GMT+12")
                           
#round the date time variable at the interval of aggregation desired 
df$date_time <-round_date(date,"15 minutes")

#aggregate data calculating the mean in the aggregation interval

#2 cases:

#case 1:"ddply"
# it can be parallelized easily
# it is possible to easily apply different functions on the different columns

df<-ddply(.(date_time),colwise,mean) # compact form

df<-ddply(.(date_time),summarize,
          power_mean=mean(power,na.rm=TRUE),
          temperature_mean=mean(temperature,na.rm=TRUE),
          temperature_max=max(temperature,na.rm=TRUE)
          ) # form useful for different functions application

#case 2: "summarise"
df%<>%
  group_by(date_time) %>%
  summarise_each(funs(mean= mean(., na.rm=TRUE)))
colnames(df)<-gsub(pattern ="_mean" ,replacement ="" ,x = colnames(df))
