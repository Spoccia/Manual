library(shiny)
library(lubridate)
library(segmented)
library(plyr)
library(dplyr)
library(knitr)
library(plotly)
library(magrittr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(dendextend)
library(rpart)
library(partykit)
library(ggsci)
library(shinydashboard)
library(tidyverse)
library(repurrrsive)
library(proxy)
# Load the predefined theme for carpet plot
source("theme_carpet.R")
source("Functions.R")


body<- dashboardBody(
  tabItems( 
    # First tab content
    tabItem(tabName = "E-Viz",
            fluidPage(
              titlePanel("E-ViZ: Energy visual Inspector"),
              sidebarLayout(
                sidebarPanel( 
                  uiOutput("choose_dataset"),
                  # fileInput(inputId = "file", label = "Choose file"),
                  uiOutput("SelectColumn"),
                  uiOutput("SelectDate"),
                  #        uiOutput("ApplyDate"),
                  uiOutput("KindGraph"),
                  uiOutput("OrganizeBy")
                ),
                mainPanel(
                  plotOutput("presentation"),
                  uiOutput("Download")
                )
              )
            )
    ),
    tabItem(tabName = "Inspector",
            fluidRow(
              column(4,
                     uiOutput('box3_1'
                     ),
                     uiOutput('box3_2'),
                     uiOutput('box3_3'),
                     uiOutput('box3_4'),
                     uiOutput('box3_5')),
              column(2,
                     box(tableOutput("clus_tab"), width = "100%",
                         solidHeader = T, status = "primary", title = "Counter")
              ),
              column(6,
                     uiOutput('box3_7'),
                     box(plotOutput("clus_plot", width = "580px", height = "600px"), width = "100%",
                         solidHeader = T, status = "primary", title = "Cluster Plot"),
                     uiOutput('box3_6')
              )
            )
    )
    
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Energy Inspector"),
  dashboardSidebar(
    sidebarMenu(id="items",
                menuItem("Visualizer", tabName = "E-Viz", icon = icon("dashboard")),
                menuItem("Inspector", tabName = "Inspector", icon = icon("th"))
    )
  ),
  body
)



myData<-NULL
TimeColumnName="Date/Time"
OneDayStep <-96
InterestingVaraible<-NULL
imageContent <-NULL
columnNames<-NULL
values<-NULL


server <- function(input, output, session) {
  
  values <- reactiveValues(df_data = NULL, df_out = NULL, df_clus = NULL,list_var= NULL, cl_plot = NULL)
  output$choose_dataset <- renderUI({
    fileInput(inputId = "file", label = "Choose file")
  })
  
  observeEvent(input$file,{
    output$SelectColumn <- renderUI({
      if(is.null(input$file))
        varnames=NULL
      else{
        inFile <- input$file
        #            myData <- reactiveValues(data = readData(inFile$datapath,""))
        myData <<-readData(inFile$datapath,"")
        columnNames<<-colnames(myData[,6:ncol(myData)])
      }
      selectInput("Variable", "Variable",choices =columnNames)# as.list(data_sets))
      
      # output[["box3_1"]] <- renderUI({
      #   
      #   # if(is.null(clusindex)) return()
      #   
      #   varname<-columnNames
      #   
      #   box(h4("Select variable to analyze:"),
      #       # clusterVariable was y
      #       selectInput('clusterVaraiable', 'Variable:', choices = NULL),
      #       h4("Select normalization type:"),
      #       selectInput('norm_3', 'Normalization:', choices = c("none","max","max-min","z-score")),
      #       h4("Select distance type:"),
      #       selectInput('distance3', 'Distance:', choices = c("Euclidean","PCC")), width = "100%",
      #       background = "black"
      #   )
      # })
      
    })
    output$SelectDate  <- renderUI({
      if(is.null(input$file))
        varnames=NULL
      else{
        matrix<-myData
        InterestingVaraible<-TimeColumnName
        ACTdata <- selectedColumn(matrix, InterestingVaraible)
        #tempDate$time=as.Date(myData$TimeColumnName)
        
        
        MinDate <- as.Date(matrix[[1,TimeColumnName]])
        MaxDate <- as.Date(matrix[[nrow(matrix),TimeColumnName]])
      }
      dateRangeInput("daterange", "Date range:",
                     start  = MinDate,
                     end    = MaxDate,
                     min    = MinDate,
                     max    = MaxDate,
                     format = "yyyy-mm-dd",
                     separator = " - ")
    })
    #    output$ApplyDate <- renderUI({actionButton("applytimeperiod", "Filter Time")})
    
    
    output$KindGraph <-renderUI({
      radioButtons("graph", "Select the Graph",
                   c("2D plot",
                     "Heatmap",
                     "boxPlotByDay"))
    })
    output$OrganizeBy <-renderUI({
      radioButtons("organizer", "Show data grouped by:",
                   c("full period",
                     "time day",
                     "week day"))
    })},
    ignoreInit = TRUE)
  
  observeEvent(input$graph,{
    output$OrganizeBy <-renderUI({
      if(input$graph == "2D plot"){
        radioButtons("organizer", "Show data grouped by:",
                     c("full period"))
      }else if(input$graph == "Heatmap" | input$graph == "boxPlotByDay"){
        radioButtons("organizer", "Show data grouped by:",
                     c("time day",
                       "week day"))
      }
    })
    output$Download <-renderUI({
      downloadButton('downloadImage', 'Download image')
    })
  },
  ignoreInit = TRUE)
  
  observeEvent(input$Variable,#c(input$Variable , input$applytimeperiod),
               {output$presentation <-renderPlot({
                 # Create a local variable myData  that is a local copy of the global varaible myData
                 matrix<-myData 
                 InterestingVaraible<- input$Variable
                 ActData <- selectedColumn(matrix, InterestingVaraible)
                 timestart <-input$daterange[1]
                 timeEnd   <-input$daterange[2]
                 intervalTime= interval(as.POSIXct(timestart),as.POSIXct(timeEnd))
                 mysubsetData <- ActData[ActData$time %within% intervalTime,]
                 if(input$graph == "2D plot"){
                   if(input$organizer=="full period"){
                     imageContent<<-rowvisualization(mysubsetData)
                     return(imageContent)
                   } 
                   # else if(input$organizer=="time day"){
                   #    return(plotbyDays(ActData,InterestingVaraible))
                   #  }
                 }else if(input$graph == "Heatmap"){
                   if(input$organizer=="time day"){
                     imageContent<<-plotbyDays(mysubsetData,InterestingVaraible)
                     return(imageContent)
                   }else if(input$organizer=="week day"){
                     imageContent<<-heatmapByWeekDays(mysubsetData,InterestingVaraible)
                     return(imageContent)
                   }
                 }
                 else if(input$graph == "boxPlotByDay"){
                   if(input$organizer=="time day"){
                     imageContent<<-boxPlotByDay(mysubsetData,InterestingVaraible)
                     return(imageContent)
                   }else if(input$organizer=="week day"){
                     imageContent<<-boxPlotByweekDay(mysubsetData,InterestingVaraible)
                     return(imageContent)
                   }
                 }
                 #       output$test <- (print(input$Variable))
                 #       return(rowvisualization(ActData))
                 #    }
                 return()
               })
               },
               ignoreInit = TRUE
  )
  
  output$downloadImage <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, imageContent, width = 16, height = 10.4)
    }
  )
  

  
  observeEvent(input$items, {
    
    if(input$items == "Inspector"){
      output[["box3_1"]] <- renderUI({
        
        # if(is.null(clusindex)) return()
        
        varname<-columnNames 
        
        box(h4("Select variable to analyze:"),
            # clusterVariable was y
            selectInput('clusterVaraiable', 'Variable:', choices = varname),
            h4("Select normalization type:"),
            selectInput('norm_3', 'Normalization:', choices = c("none","max","max-min","z-score")),
            h4("Select distance type:"),
            selectInput('distance3', 'Distance:', choices = c("Euclidean","PCC")), width = "100%",
            background = "black"
        )
      })
      
      output[["box3_2"]] <- renderUI({
        tempvalue<-input$distance3
        if(input$distance3 == "PCC"){
          box(
            h4("Select desired clustering method:"),
            selectInput('clus', 'Method:', choices = c("ward.D2","average","single","complete")), width = "100%",
            background = "black"
          )
        }else {
          
          box(
            selectInput('clus', 'Method:', choices = c("ward.D2","average","single","complete",
                                                       "FDL","kmeans","kmeans++")), width = "100%",
            background = "black"
          )
        }
      })
      
      output[["box3_3"]] <- renderUI({
        
        if(is.null(input$clus)) return()
        if(input$clus == "FDL"){
          box(
            h3("Set max distance"),
            selectInput("D_3", "Distance:", choices = seq(20,100,5)),
            
            tags$hr(),
            actionButton("submit3", "start clUUUster"), width = "100%",
            background = "black"
          )
          
          
        }else {
          
          box(
            h3("Select desired number of cluster"),
            selectInput('K_3', 'Clusters:', choices = c(2:50)),
            tags$hr(),
            actionButton("submit3", "start clUUUster"), width = "100%",
            background = "black"
          )
        }
      })
      output[["box3_5"]] <- renderUI({
        
        if(is.null(input$clus))
        {
          return()
        }
        
        data <- values$df_clus
        
        box(
          h3("Select cluster to merge"),
          selectInput("K_merge", "Clusters:", choices = unique(data$cluster), multiple = T),
          tags$hr(),
          actionButton("merge", "FU-SIO-NEEEEEE"), width = "100%",
          background = "black"
        )
      })
    }
    
    output[["box3_6"]] <- renderUI({
      
      box(
        actionButton("reuse", "L'ACCENDIAMO?"), width = "100%",
        solidHeader = T, status = "warning", title = "Save cluster labels"
      )
      
    })
    
    output[["box3_7"]] <- renderUI({
      
      cList <- "intensive"
      if(input$norm_3 != "none") cList <- c(cList,"normalized")
      
      if(input$clus %in% c("average","ward.D2","single","complete")) cList <- c(cList,"dendrogram")
      
      box(
        selectInput("plot_type2sheet", "Choices:", choices = cList, selected = "intensive"),
        downloadButton('downloadPlot', 'Download Plot'),
        width = "100%",
        solidHeader = T, status = "primary", title = "Select visualization type"
      )
      
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste(input$plot_type2sheet, '.jpg', sep='') },
      content = function(file) {
        ggsave(file,clus_plot_input())
      }
    )
  })
  
  observeEvent(input$submit3, {
    if(is.null(input$clus)) 
    {
      return()
    }
    y <- input$clusterVaraiable
    norm_method <- input$norm_3
    
    df_data<-selectedColumn(myData,y)
    df_data$time <- as.POSIXct(df_data$time, origin = "1970-01-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "Etc/GMT+12")
    df_data <- df_data %>%
      mutate(
        DATE = as.Date(time),
        TIME = format(as.POSIXct(df_data$time,format="%H:%M:%S"),"%H:%M")
      )
    
    data <- df_data[,c("DATE","TIME","variable")]
    
    colnames(data)[3] <- "y"
    
    if(norm_method == "z-score"){
      
      data$y <- (data$y-mean(data$y))/sd(data$y)
      
      data <- data %>%
        spread(TIME,y)
      
      
    }else if(norm_method == "none"){
      
      data <- data %>%
        spread(TIME,y)
      
      
      
    }else {
      
      data <- data %>%
        spread(TIME,y)
      
      if(norm_method == "max"){
        
        data[2:ncol(data)] <- data[2:ncol(data)]/apply(data[2:ncol(data)], 1, max)
        
      }
      
      if(norm_method == "max-min"){
        
        data[2:ncol(data)] <- (data[2:ncol(data)] - apply(data[2:ncol(data)], 1, min))/(apply(data[2:ncol(data)], 1, max) - apply(data[2:ncol(data)], 1, min))
        
      }
      
      # values$data_clus <- data0
      
    }
    
    clus_method <- input$clus
    
    if(clus_method == "FDL"){
      
      ro_run <- as.numeric(input$D_3)*24
      
      FDL_res <- FDL_clus(data = (data[2:ncol(data)]), l_feat = (ncol(data)-1), ro = ro_run)
      
      cluster <- FDL_res$it_matr[,ncol(FDL_res$it_matr)]
      
      data <- as.data.frame(cbind(data, cluster))
      
      data <- data%>%
        select(DATE, cluster)
      
      values$df_clus <<- data
     
      
    }else if(clus_method %in% c("ward.D2","average","single","complete")){
      
      if(input$distance3 == "PCC"){
        
        hcl_dist <- cor(t(data[2:ncol(data)]))
        hcl_dist <- as.dist(1 - hcl_dist)
        
      }else{
        
        hcl_dist <- dist(data[2:ncol(data)])
        
      }
      
      hcl <- hclust(hcl_dist, method = clus_method)
      
      values$cl_plot <<- hcl
      
      cluster <- cutree(hcl, k = input$K_3)
      
      data <- cbind(data, cluster)
      
      data <- data%>%
        select(DATE, cluster)
      
      values$df_clus <<- data
      values$df_data<<-df_data
    }
    
    output$clus_tab <- renderTable({
      
      if(is.null(input$clus)) return()
      
      # df <- as.data.frame(res_clus())
      values<-values
      df <- values$df_clus
      
      df <- df %>%
        split(.$cluster) %>%
        map(function(x) as.data.frame(t(c("profiles" = nrow(x)
        )))) %>%
        ldply(data.frame, .id = "cluster")
    })
    
    output$clus_plot <- renderPlot(
      print(clus_plot_input())
    )
  })

  
  clus_plot_input <- reactive( {
    values<-values
    if(is.null(input$clus)) return()
    
    if(input$plot_type2sheet == "dendrogram"){
      
      hcl <- as.dendrogram(values$cl_plot)
      
      hcl %>% set("branches_k_color", k = input$K_3)%>% set("branches_lwd", 3) %>%
        plot()
      
      
    }else{
      
      y <- "variable"#input$clusterVaraiable
      
      data <- values$df_data[,c("DATE","TIME",y)]
      
      if(input$plot_type2sheet == "normalized"){
        
        if(input$norm_3 == "z-score"){
          
          data$y <- (data$y-mean(data$y))/sd(data$y)
          
        }else {
          
          data <- data %>%
            spread(TIME,y)
          
          if(input$norm_3 == "max"){
            
            data[2:ncol(data)] <- data[2:ncol(data)]/apply(data[2:ncol(data)], 1, max)
            
          }
          
          if(input$norm_3 == "max-min"){
            
            data[2:ncol(data)] <- (data[2:ncol(data)] - apply(data[2:ncol(data)], 1, min))/(apply(data[2:ncol(data)], 1, max) - apply(data[2:ncol(data)], 1, min))
            
          }
          
          data <- data %>%
            gather(TIME,y, 2:ncol(data))
          
        }
        
      }
      
      clus <- values$df_clus
      colnames(data)[3] <- "y"
      
      df <- merge.data.frame(data, clus)
      
      centroid <- ddply(df, c("cluster","TIME"),summarize, y = mean(y))
      
      g1 <- ggplot()+
        geom_line(data = df, aes(x = as.POSIXct(TIME, format="%H:%M" , tz="Etc/GMT+12"), y = y, group = DATE), color = "gray75", size = 0.5)+
        geom_line(data = centroid  , aes(x = as.POSIXct(TIME, format="%H:%M" , tz="Etc/GMT+12") , y = y , color = as.factor(cluster), group = as.factor(cluster)), size = 1)+
        theme_bw()+
        facet_wrap(~cluster)+
        scale_x_datetime(expand = c(0,0), labels = date_format("%H:%M" , tz="Etc/GMT+12"), breaks = date_breaks("4 hour"))+
        theme(
          legend.position = "none"
        )+
        labs(x = "TIME" , y = input$y )
      
      g1
      
    }
    
  })
  
  
} # END sERVER

shinyApp(ui = ui, server = server)