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

############### arrows,text and mathematical expressions in plot ###############
rm(list = ls())
library(cwhmisc)
library(ggplot2)


set.seed(123)#necessary to replicate always the same random generation
df<-data.frame(x=10+10*rnorm(100),y=3+5*rnorm(100),clr=rep(0,100))
set.seed(123)#set each time a random number generation is performed
df[sample(seq_along(df$x),5),"clr"]<-1
df$clr<-as.factor(df$clr)
df_ones<-subset(df,clr==1)
df_ones<-df_ones[order(df_ones$x),]
df_ones$xe<-NA
df_ones$ye<-NA
df_ones$xe[1:(length(df_ones$x)-1)]<-df_ones$x[2:(length(df_ones$x))]
df_ones$ye[1:(length(df_ones$y)-1)]<-df_ones$y[2:(length(df_ones$y))]
#plotting of random points, arrow along the red dot's path and text with 
#no sense mathematical expressions
gg<-ggplot(data=df)+
  geom_point(aes(x=x,y=y,color=clr),
             size=6)+
  geom_line(data=df_ones,
            aes(x=x,y=y),
            arrow = arrow(ends = "last",type = "closed"),
            lwd=2,color="gold2")+#follow the red dot's path
  geom_segment(data=df_ones[c(1,3),],
               aes(x=x,xend=(xe-2),y=y,yend=ye+c(9,6)),
               arrow = arrow(angle=15,ends = "first",type = "open"),
               lwd=1,color="forestgreen")+#highlight the first and the last
  geom_text(data=df_ones[c(1,3),],
            aes(x=(xe-2),y=ye+c(9,6),
                label=c("start",
                        "prod(60*minute*30*second*bar(phi1),i=1,n)*integral(f(x)*dx,a,b)")),
            parse=T,
            hjust=0.5,vjust=0,size=16,color="forestgreen")+#label the arrow
  scale_color_manual(values=c("1"="red3","0"="grey15"),
                     labels=c("1"="random path","0"="other points"))+
  labs(color="Legend title",
       title="Example of arrows,text and math expressions",
       x=expression(frac(Delta*T,alpha^2) *~ ~ phi1 %prop% Omega[3]*"["*degree*paste("C]")),
       y=expression(sqrt(sum(x[i]^2,i=1,n)*cos(2*pi*i),3))
  )+
  theme_minimal()+
  theme(title=element_text(size=18),
        axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.title = element_text(size=16)
  )
dev.new()
plot(gg)

