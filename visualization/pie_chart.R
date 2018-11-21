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

###############                   pie chart plot                 ###############
rm(list = ls())
library(dplyr)
library(plyr)
library(lubridate)
library(scales)
library(ggplot2)
library(grDevices)

#load dataset
path<-"C:/Users/Dan/Documents/GitHub/Manual/data/"
df<-read.csv2(paste0(path,"data.csv"),sep=";",dec=",")
df$date_time <- as.POSIXct(df$date_time, origin = "1970-01-01 00:00:00",
                           format = "%Y-%m-%d %H:%M:%S",
                           tz = "Etc/GMT+12") # tz 
df <- mutate(df,
             HOUR = hour(date_time),
             MIN = minute(date_time),
             DAY=day(date_time),
             MONTH = month(date_time),
             YEAR = year(date_time)
)


df_pie<-ddply(df,.(MONTH),summarize,
              power=sum(power,na.rm=TRUE))
df_pie$MONTH<-factor(df_pie$MONTH)
df_pie$MONTH<-factor(df_pie$MONTH,levels=rev(df_pie$MONTH))


df_pie<-df_pie%>%
  group_by(MONTH)%>%
  mutate(per=round(power/sum(power),3),y=cumsum(per)-per/2)

#palette
reggae<-colorRampPalette(c("forestgreen","gold","red3"))




ggpie<-ggplot(data=df_pie)+
  geom_bar(aes(x="", y=power,fill=MONTH),
           alpha=0.5, stat="identity",position=position_fill(),
           color="grey50",size=1, width = 1)+
  coord_polar("y",start=0)+
  geom_text(aes(x=1.9,y=y,
                label=percent(per)),size=7)+
  scale_fill_manual(values=reggae(12))+
  labs(title="Pie chart example",fill=NULL,
       x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom",
        legend.direction = "horizontal",
        title=element_text(size=19),
        panel.border = element_blank(),
        panel.background = element_rect(fill="white")
        )

# jpeg(filename = paste0(path,"pie_chart.jpg"),
#      width = 400, height = 300,units = "mm",res = 200)
# plot(ggpie)
# dev.off()
