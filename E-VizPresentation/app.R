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
    ),
    tabItem(tabName = "cart",
            fluidRow(
              column(4,
                     uiOutput('box4_1')
              ),
              column(8,
                     box(tableOutput("cm_cart"), width = "100%",
                         solidHeader = T, status = "primary", title = "Confusion Matrix"),
                     box(plotOutput("cp_plot", width = "800px", height = "450px"), width = "100%",
                         solidHeader = T, status = "primary", title = "CP Plot"),
                     box(plotOutput("cart_plot", width = "800px", height = "600px"), width = "100%",
                         solidHeader = T, status = "primary", title = "Tree Plot")
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
                menuItem("Inspector", tabName = "Inspector", icon = icon("th")),
                menuItem("CART", tabName = "cart", icon = icon("ambulance"))
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
  
  options(shiny.maxRequestSize=100*1024^2)
  
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
        distance3Value="Euclidean"
        if(length(input$distance3)!=0)
          distance3Value=input$distance3
        if(distance3Value == "PCC"){
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
          actionButton("merge", "Merge Selected Clusters"), width = "100%",
          background = "black"
        )
      })
    }
    
    output[["box3_6"]] <- renderUI({
      
      box(
        actionButton("reuse", "Reuse Clusters"), width = "100%",
        solidHeader = T, status = "warning", title = "Save cluster labels"
      )
      
    })
    
    output[["box3_7"]] <- renderUI({
      
      cList <- "intensive"
      norm3Value="none"
      clusValue = "ward.D2"
      if(length(input$norm_3 )!=0)
        norm3Value=input$norm_3
      if(length(input$clus)!=0)
         clusValue=input$clus
      
      if(norm3Value != "none") cList <- c(cList,"normalized")
      
      if(clusValue %in% c("average","ward.D2","single","complete")) cList <- c(cList,"dendrogram")
      
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
        labs(x = "TIME" , y = input$clusterVaraiable)#input$y )
      
      g1
      
    }
    
  })
  
  observeEvent(input$reuse, {
    temp2 <- values$df_data
    temp1 <- values$df_clus[c("DATE","cluster")]
    temp1$cluster <- as.character(temp1$cluster)
    colnames(temp1)[2] <- paste("cluster", input$clusterVaraiable, sep = "_")# input$y, sep = "_")
    temp2[which(colnames(temp2) == paste("cluster", input$clusterVaraiable, sep = "_"))] <- NULL#input$y, sep = "_"))] <- NULL
    values$df_data <- merge.data.frame(temp1, temp2, all.x = T)
    
  })
  
  observeEvent(input$merge, {
    temp1 <- values$df_clus
    temp2 <- min(input$K_merge)
    temp1$cluster[temp1$cluster %in% input$K_merge] <- temp2
    values$df_clus <- temp1
  })
  
  ### CART
  
  output[["box4_1"]] <- renderUI({
    
    data <- values$df_data
    
    num_var <- sapply(data, is.numeric)
    num_var <- names(num_var[num_var == TRUE])
    num_var <- num_var[num_var != input$clusterVaraiable]#input$y]
    
    cat_var <- colnames(data)
    cat_var <- cat_var[!cat_var %in% c(num_var,"DATE","TIME","DATE_TIME",input$clusterVaraiable)]#input$y)]
    
    box(
      h4("Select numeric variables:"),
      selectInput("num_var", "Variables:", choices = num_var, multiple = T),
      h4("Select fucntions to apply:"),
      checkboxGroupInput("fun_gr", "Functions:", choices = list("mean" = 1, "max" = 2, "min" = 3, "sd" = 4), selected = 1),
      h4("Select categorical variables:"),
      selectInput("cat_var", "Variables:", choices = cat_var, multiple = T),
      h4("Select CART max depth:"),
      sliderInput("max_depth", "Value:", min = 2, max = 30, value = 4, step = 1 ),
      h4("Select CART min bucket:"),
      sliderInput("min_buck", "Value:", min = 1, max = 50, value = 12, step = 1 ),
      actionButton("CART", "Un po' di CART qua?"),
      width = "100%",
      solidHeader = T,
      status = "warning",
      background = "black"
    )
    
  })
  
  cart_res <- eventReactive(input$CART,{
    
    if(is.null(input$num_var) & is.null(input$cat_var)) return()
    
    data <- values$df_data
    
    data <- data[c("DATE",input$cat_var,input$num_var)]
    
    M <- ncol(data[c("DATE",input$cat_var)])
    
    data_end <- unique(data[c("DATE",input$cat_var)])
    
    if(!is.null(input$num_var)){
      
      for(i in 1:length(input$fun_gr)){
        
        data_x <- ddply(data, c("DATE",input$cat_var), colwise(match.fun(f_mat(input$fun_gr[i]))))
        
        colnames(data_x)[(M+1):ncol(data_x)] <- paste(f_mat(input$fun_gr[i]), colnames(data_x)[(M+1):ncol(data_x)], sep = "_")
        
        data_end <- merge.data.frame(data_end, data_x)
      }
    }
    
    data <- data_end
    # data <- ddply(data, c("DATE",input$cat_var), colwise(mean))
    
    clus <- values$df_clus[c("DATE","cluster")]
    
    data <- merge.data.frame(data,clus)
    
    data$cluster <- as.factor(data$cluster)
    set.seed(200)
    ct <- rpart(reformulate(response = "cluster" , termlabels = names(data[!names(data) %in% c("DATE","cluster")])) ,
                data = data, control = rpart.control(cp = 0 , minsplit = 0, xval = 10, maxdepth = input$max_depth, minbucket = input$min_buck))
    cp.table <- as.data.frame(ct$cptable)
    pcp <- plotcp(ct)
    
    min_err <- cp.table[cp.table$xerror == min(cp.table$xerror) , ]
    
    min_err <- min_err[min_err$nsplit == min(min_err$nsplit) , ]
    ct_pre <- ct
    ct <- prune(ct , cp = max(cp.table$CP[cp.table$xerror < min_err$xerror + min_err$xstd]  )    )
    
    cm <- as.data.frame(as.matrix(table(data[,"cluster"],predict(ct, data, type = "class"))))
    cm <- spread(cm , Var2, Freq)
    rownames(cm) <- cm$Var1
    cm$Var1 <- NULL
    
    res <- list("model" = ct, "cm" = cm, "cp_plot" = ct_pre)
    
  })
  
  output$cm_cart <- renderTable({
    
    req(cart_res())
    
    cm <- cart_res()[["cm"]]
    
    cm
  },include.rownames=T)
  
  output$cp_plot <- renderPlot({ 
    
    req(cart_res()) ## ?req #  require that the input is available
    
    pcp <- cart_res()[["cp_plot"]]
    
    plotcp(pcp)
    
  })
  
  
  output$cart_plot <- renderPlot({ 
    
    req(cart_res()) ## ?req #  require that the input is available
    
    ct <- cart_res()[["model"]]
    
    cm <- cart_res()[["cm"]]
    
    accuracy = round(sum(diag(as.matrix(cm[1:(length(cm))]))) / sum(cm[1:(length(cm))]),3)
    
    plot(as.party(ct), main=paste("Accuracy: ", accuracy), gp=gpar(fontsize=7))
  })
  
  
  
} # END sERVER

shinyApp(ui = ui, server = server)