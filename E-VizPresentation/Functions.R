library(readxl)
library(ggplot2)
 TimeColName="Date/Time"

readData <- function(LocalPath, filename) {
  temp <- paste0(LocalPath,filename )
  MyData <-read_excel(temp)
  MyData[TimeColName]<-as.POSIXct((MyData[[TimeColName]]-719529)*86400, origin = '1970-01-01', tz='UCT')
  return(MyData)
}

# this function  retiun the selected varaibel i.e. columnname 
selectedColumn <- function(dataVaraible, columName){
    temp<-dataVaraible[c(columName,TimeColName)]
    colnames(temp)<-c("variable","time")
    return(temp)
}

rowvisualization <- function(dataVaraible){
  grafoCartesianoesteso<-ggplot()+
    geom_line(data = dataVaraible, aes(x = time,y=variable))
  return(grafoCartesianoesteso)
}

plotbyDays <-function(dataVaraible,InterestingVaraible){
  temp<-dataVaraible
  temp$time <- as.POSIXct(temp$time, origin = "1970-01-01 00:00:00",
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "Etc/GMT+12")
 # temp$time<-as.Date(temp$time)
  temp1 <- temp %>%
      mutate(
          date = as.Date(time),
          time = format(as.POSIXct(temp$time,format="%H:%M:%S"),"%H:%M")#as.POSIXct(time, format = "%H:%M:%S", tz = "Etc/GMT+12")#format(as.POSIXct(temp$time,format="%H:%M:%S"),"%H:%M")
                  # strftime(time ,
                  #        format = "%H:%M" ,    # change the format if needed
                  #        tz = "Etc/GMT+12")   # change tz according to date_time
                  #
          )



  # palette definition
  cols <- brewer.pal(9, "Spectral")
  #temp1$time <- as.POSIXct(temp1$time, format = "%H:%M", tz = "Etc/GMT+12")
  heatmapgraph <- ggplot()+
    geom_tile(data= temp1, aes(x = as.POSIXct(time, format = "%H:%M", tz = ""),
                            y = date, fill = variable))+
    scale_fill_gradientn(colours = rev(cols)#,
                      #    limits=c(0,ceiling(max(temp1$Variable)/100)*100),                        # this argument are used
                      #    breaks = c(0, ceiling(max(temp1$Variable)/200)*100,                      # to set the color distance
                      #               ceiling(max(temp1$Variable)/100)*100)
    )+
    theme_bw()+
    scale_y_date(breaks = date_breaks("1 day"),
                 labels = date_format("%Y-%m-%d" , tz="Etc/GMT+12"),                             # define labels on date axis
                 expand = c(0,0))+
    scale_x_datetime(expand = c(0,0),
                     labels = date_format(("%H:%M") , tz="Etc/GMT+12"),                       # define labels in time axis
                     breaks = date_breaks("4 hour"))+                                          # define braks between hours
  #  theme_bw()+
    theme_carpet()+
    labs(x = "Hour" , y="Date", fill="")+
    ggtitle(InterestingVaraible)
  return(heatmapgraph)
}
minimumTime<- function(dataVaraible){
  return(min(as.Date(dataVaraible$"time")))
}
maximumTime<- function(dataVaraible){
  return(max(as.Date(dataVaraible$"time")))
}
boxPlotByDay <-function(dataVaraible,InterestingVaraible)  {  
  dataVaraible$time <- as.Date(dataVaraible$time)
  ## need og a ggplot for downloading
  # p <-ggplot(dataVaraible, aes(x=time, y=variable)) + 
  #   geom_boxplot()
  return(boxplot(dataVaraible$variable ~ dataVaraible$time))#p)#
}

heatmapByWeekDays <-function(dataVaraible,InterestingVaraible){
#  dataVaraible<- dataVaraible
  dataVaraible$time <- as.POSIXct(dataVaraible$time, origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "Etc/GMT+12")
  # Convert the datetime from POSIXlt to Date. This makes the next step smoother.
  dataVaraible<- dataVaraible%>% 
    mutate(
      date = as.Date(time),
      daytime = format(as.POSIXct(dataVaraible$time,format="%H:%M:%S"),"%H:%M")
    )
  #dataVaraible$time <- as.Date(dataVaraible$time)
  # Add a column with the weekday and then abbreviate the weekday to be just
  # the first 3 letters. Then, make that column a factor with the 
  # values in a specific order (the normal order for days of the week)
  dataVaraible <- dataVaraible %>% 
    mutate(weekday = weekdays(date)) %>% 
    mutate(weekday = factor(weekday,
                            levels =  c("Sunday", "Monday", "Tuesday", "Wednesday",
                                        "Thursday", "Friday", "Saturday")))
  #Add a column for the week number x axis
  dataVaraible <- dataVaraible %>% 
    mutate(weeknumber = strftime(date, format = "%V"))
  
  dataVaraible <- dataVaraible %>%
    select(weekday, daytime, variable)
  
  # # Group  the data in someway
  # dataVaraible_total <- dataVaraible %>% 
  #   group_by(weekday, daytime) 
  
  # palette definition
  cols <- brewer.pal(9, "Spectral")
  
  # Create the plot.
  heatmapgraph <- ggplot(dataVaraible, mapping = aes(x = as.POSIXct(daytime, format = "%H:%M", tz = ""), y = weekday, 
                                             fill = variable))+
    scale_fill_gradientn(colours = rev(cols))+
    theme_bw()+
   # theme_carpet()+
    scale_y_discrete(expand = c(0,0)) +
    scale_x_datetime(expand = c(0,0),
                   labels = date_format(("%H:%M") , tz="Etc/GMT+12"),                       # define labels in time axis
                   breaks = date_breaks("4 hour"))+                                          # define braks between hours
    labs(x = "Hours" , y="week days", fill="")+
    ggtitle(InterestingVaraible)
  return(heatmapgraph)
}

boxPlotByweekDay <-function(dataVaraible,InterestingVaraible)  {  
  
  dataVaraible<- dataVaraible%>% 
    mutate(
      date = as.Date(time),
   #   daytime = format(as.POSIXct(dataVaraible$time,format="%H:%M:%S"),"%H:%M")
    )
  dataVaraible <- dataVaraible %>% 
    mutate(weekday = weekdays(date)) %>% 
    mutate(weekday = factor(weekday,
                            levels =  c("Sunday", "Monday", "Tuesday", "Wednesday",
                                        "Thursday", "Friday", "Saturday")))
  #dataVaraible$time <- as.Date(dataVaraible$time)
  p <-ggplot(dataVaraible, aes(x=weekday, y=variable)) + 
    geom_boxplot()
  return(p)#boxplot(dataVaraible$variable ~ dataVaraible$weekday))
}