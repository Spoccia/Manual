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

###############                   carpet plot                    ###############


library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
#### loading data ####
# case 1: working in a project

# modify the following line to add your path 
df <- read.csv("data/data.csv", sep = ";", dec = ",")

# case 2: outside of a project

# file_path <- "C:/"
# file_name <- "data.csv
# df <- read.csv(paste0(file_path, file_name), sep = ";", dec = ",")

#### variables setting ####

# To produce the carpet plot is necessary to decompose date_time into
# date and time

# transform datetime in POSIXct format
df$date_time <- as.POSIXct(df$date_time, origin = "1970-01-01 00:00:00",
                           format = "%Y-%m-%d %H:%M:%S",
                           tz = "Etc/GMT+12") # tz 

df <- df %>%
  mutate(
    date = as.Date(date_time),
    time = strftime(date_time , 
                    format = "%H:%M" ,    # change the format if needed
                    tz = "Etc/GMT+12")   # change tz according to date_time
  )

#### plot ####

# Load the predefined theme for carpet plot
source("theme_carpet.R")

# palette definition
cols <- brewer.pal(9, "Spectral")

carpet <- ggplot()+
  geom_tile(data= df, aes(x = as.POSIXct(time, format = "%H:%M:%S", tz = "Etc/GMT+12"),
                           y = date, fill = power))+    
  scale_fill_gradientn(colours = rev(cols), 
                       # limits=c(0,ceiling(max(df$Power)/100)*100),                        # this argument are used
                       # breaks = c(0, ceiling(max(df$Power)/200)*100,                      # to set the color distance
                       #            ceiling(max(df$Power)/100)*100)
                       )+
  theme_bw()+
  scale_y_date(breaks = date_breaks("1 month"), 
               labels = date_format("%Y-%m" , tz="Etc/GMT+12"),                             # define labels on date axis
               expand = c(0,0))+
  scale_x_datetime(expand = c(0,0), 
                   labels = date_format(("%H:%M") , tz="Etc/GMT+12"),                       # define labels in time axis
                   breaks = date_breaks("4 hour"))+                                         # define braks between hours
  theme_bw()+
  theme_carpet()+
  labs(x = "Hour" , y="Date", fill="", 
        caption = "(a)"                                                                     
  )+
  ggtitle("Electrical Load [kW]")

# Fixed plotting options for high quality image

# jpeg(filename = "",
#      width = 140, height = 100, units = "mm",res = 500)
# plot(carpet)
# dev.off()