#
# This Shiny app was created for the Data Products course in the 
#   John Hobkins Data Science Specialization on Coursera
#
# Author    Marius Peche
# Date      8 August 2017
#
# This uses my own KNP data collected during a five-day Safari in South Africa.

library(shiny)
library(shinyjs)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Kruger National Park: Safari Overview"),
  
  #Initiate Tabs, each with a sidebar
  tabsetPanel(
    #Linechart with count of species per day
    #Select a file for a safari
    tabPanel("Safari Overview",
    #         sidebarLayout(
    #           sidebarPanel(
    #             fileInput("filSafari","Import a Safari Database",
    #                       multiple = FALSE,accept = c("*.csv"))
    #           ),
    #           mainPanel(
                 plotOutput("pltTotalcounts")
    #           )
    #         )
    ),
    
    #CheckList with unique species per day
    #Select day or check entire safari & select class
    tabPanel("CheckList",
             sidebarLayout(
               sidebarPanel(shinyjs::useShinyjs(),
                 checkboxInput("chkSafari","Entire safari",value = FALSE),
                 uiOutput("dteDay_out"),
                 uiOutput("selListClass_out"),
                 uiOutput("selListOrder_out")
               ),
               mainPanel(
                 tableOutput("tblCheckList")
               )
             )
    ),
    
    #Histogram with most common species
    #select (x) most common [Birds/mammals/reptiles/all?]
    tabPanel("Most Common Species",
             sidebarLayout(
               sidebarPanel(
                 numericInput("numbBins",
                              "Select how many creatures you would like to see:",
                              value = 10, min = 5, max = 20, step = 1),
                 #replace this with uiOutput
                 uiOutput("selCommonClass_out")
               ),
               mainPanel(
                 textOutput("debug"),
                 plotOutput("pltMostCommon")
               )
             )
    )
    
  )
  
)

server <- function(input, output) {
  #load species data. This is once-off
  species <- read.csv("./Species.csv")
  knp <- read.csv("KNP.csv")
  knp$date = as.Date(knp$Timestamp)
  knp <- merge(knp,species[,1:4],by="SpeciesUId")
  
  #load new data set: Won't use this for Coursera, but necessary for personal use
  #knp <- reactive({
  #  if (is.null(input$filSafari)) {return(NULL)}
  #  #print(input$filSafari$name)
  #  tmp <- read.csv(input$filSafari$name)
  #  tmp$date = as.Date(tmp$Timestamp)
  #    tmp <- merge(tmp,species[,1:4],by="SpeciesUId")
  #  return(tmp)
  #})
  
  
  #UI Interfaces
  observeEvent(input$chkSafari,{
    if (input$chkSafari){
      shinyjs::disable("dteDay")
    }else{
      shinyjs::enable("dteDay")
    }
  })
  output$dteDay_out <- renderUI({
    if (is.null(knp)){ 
      dateInput("dteDay","Select a day")
      shinyjs::disable("dteDay")
    }
    dateInput("dteDay","Select a day", value=min(knp$date), min=min(knp$date), max=max(knp$date))
  })
  output$selListClass_out <- renderUI({
    if (is.null(knp)){ return(NULL)}
    classOpt <- sort(unique(knp$Class))
    
    radioButtons("selListClass",
                 "Select the Species class of sightings",
                 choices = classOpt,
                 selected = classOpt[1])
  })
  output$selListOrder_out <- renderUI({
    if (is.null(input$selListClass)){ return(NULL)}
    
    tmp <- knp %>% filter(Class==input$selListClass)
    if (input$chkSafari==FALSE){
      tmp <- tmp %>% filter(date==input$dteDay)
    }
    
    orderOpt <- sort(unique(as.character(tmp$Order)))
      
    selectInput("selListOrder",
              "Select the Species Order of sightings",
              choices = append("ALL",orderOpt),
              selected = "ALL")
  })
  output$selCommonClass_out <- renderUI({
    if (is.null(knp)){ return(NULL)}
    classOpt <- sort(unique(knp$Class))
    
    checkboxGroupInput("selCommonClass",
                      "Select the class of sightings",
                      choices = classOpt,
                      selected = classOpt[1])
  })
  

  
  #Tables
  output$tblCheckList <- renderTable({
    if(is.null(knp)){
      return(data.frame(err=c(1),message=c("No Database selected")))
    }
    if(is.null(input$selListClass)){
      return(data.frame(err=c(2),message=c("No Sighting Class selected")))
    }
    if(is.null(input$selListOrder)){
      return(data.frame(err=c(2),message=c("No Sighting Order selected")))
    }
    
    library(dplyr)
    tmp <- knp %>% filter(Class==input$selListClass)
    if (input$selListOrder != "ALL"){
      tmp <- tmp %>% filter(Order==input$selListOrder)
    }
    
    if (input$chkSafari){
      tmp <- tmp  %>% select(date,Name) %>% unique()
      library(tidyr)
      tmp$mark = "#"
      tmp <- spread(tmp,date,mark)
      tmp[][is.na(tmp)] <- " "
      return(tmp)
    }else{
      tmp <- tmp %>% filter(date==input$dteDay)
      return(unique(select(tmp,Name)))
    }
    
    
  })
  
  
  #Plots
  output$pltTotalcounts <- renderPlot({
    if (is.null(knp)) {return(NULL)}
    library(plyr)
    tmp <- ddply(knp,c("date","Class"),
               function(x){ length(unique(x$SpeciesUId))})
    #head(tmp)
    ggplot(tmp, aes(x=date, y=V1, color=tmp$Class)) + geom_line()
  })
  output$pltMostCommon <- renderPlot({
    if (is.null(knp)) {return(NULL)}
    if (is.null(input$selCommonClass)) {return(NULL)}
    
    library(plyr)
    #tmp <- ddply(tmp,c("date","Name"),
    tmp <- ddply(knp[knp$Class %in% input$selCommonClass,],"Name",
                 function(x){ length(x$SpeciesUId)})
    tmp <- tmp[order(tmp$V1,decreasing = TRUE),]
    
    dsp <- knp[knp$Name %in% tmp[seq(input$numbBins),]$Name,]
    
    dsp <- within(dsp, 
               Name <- factor(Name, levels=names(sort(table(Name), 
                              decreasing=TRUE))))
    
    
    ggplot(dsp, aes(Name)) + 
        geom_histogram(stat="count") +
        xlab("Name of species") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Times seen")
  })

}


shinyApp(ui = ui, server = server)






#Use this to disable a Interface

#library(shiny)
#runApp(shinyApp(
#  ui = fluidPage(
#    shinyjs::useShinyjs(),
#    numericInput("test", "Test", 5),
#    actionButton("submit", "Choose")
#  ),
#  server = function(input, output, session) {
#    observeEvent(input$submit, {
#      shinyjs::disable("test")
#    })
#  }
#))  


##Don't think I'll need this, but keep it aside rather than delete it.
#We're not interested in the GPS data at present.
#tmp <- knp[tmp$lat!="Unable to determine GPS",]
#tmp$lat <- as.numeric(as.character(tmp$lat))
#tmp$long <- as.numeric(as.character(tmp$long))

