library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(scales)
library(dplyr)
# Load the predefined theme for carpet plot
source("theme_carpet.R")
source("Functions.R")


ui <- fluidPage(
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
    #actionButton(inputId = "update", label = "Reload input files"),
  
    #selectInput(inputId = "variables", label = "Select your measure",choices =NULL),
    
)

myData<-NULL
TimeColumnName="Date/Time"
OneDayStep <-96
InterestingVaraible<-NULL
imageContent <-NULL
server <- function(input, output, session) {
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
            varnames=colnames(myData[,6:ncol(myData)])
        }
        selectInput("Variable", "Variable",choices =varnames)# as.list(data_sets))
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

    } # END sERVER

shinyApp(ui = ui, server = server)