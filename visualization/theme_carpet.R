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

###############                   theme carpet                  ###############


theme_carpet <- function(){
  library(ggplot2)
  
  # axis text and title, change it if needed
  ttl <- element_text(size=10, 
                      # margin = margin(t = 1, r = 1, b = 0, l = 0, unit = "mm") # modify margin if needed
                      )
  txt <- element_text(size=8, 
                      # margin = margin(t = 1, r = 1, b = 0, l = 0, unit = "mm")
                      )
  theme(
    plot.title = element_text(size=10, hjust = 0.45),                                                
    legend.text= txt,
    legend.position = "top", 
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(1.5, "mm"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.caption = ttl,
    legend.title = ttl,
    axis.title.x = ttl,
    axis.title.y = ttl,
    axis.text.x = txt,
    axis.text.y = txt,
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,3,1,1), "mm")
  )
  
}

