library(readxl)
library(ggplot2)
TimeColumnName="Date/Time"

readData <- function(LocalPath, filename) {
  temp <- paste0(LocalPath,filename )
  MyData <-read_excel(temp)
  MyData[TimeColumnName]<-as.POSIXct((MyData[[TimeColumnName]]-719529)*86400, origin = '1970-01-01', tz='UCT')
  return(MyData)
}

# this function  retiun the selected varaibel i.e. columnname 
selectedColumn <- function(data, columName){
    temp<-myData[c(columName,TimeColumnName)]
    colnames(temp)<-c("variable","time")
    return(temp)
}

rowvisualization <- function(data){
  grafoCartesianoesteso<-ggplot()+
    geom_line(data = data, aes(x = time,y=variable))
  return(grafoCartesianoesteso)
}

plotbyDays <-function(data,InterestingVaraible){
  temp<-data
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
  carpet <- ggplot()+
    geom_tile(data= temp1, aes(x = as.POSIXct(time, format = "%H:%M", tz = ""),#time,#as.POSIXct(time, format = "%H:%M", tz = "Etc/GMT+12"),
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
}