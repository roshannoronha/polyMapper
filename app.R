## Load essential libraries

if (!require(shiny)) {
  install.packages("shiny")
}

if (!require(ggpot2)) {
  install.packages("shiny")
}

if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}

if (!require(dplyr)) {
  install.packages("dplyr")
}

if (!require(imager)) {
  install.packages("shiny")
}

if (!require(markdown)) {
  install.packages("shiny")
}

library(shiny)
library(shinydashboard)
library(dplyr)
library(imager)
library(markdown)

#read in drug info
drugInfo <- read.csv("drugData/drugInfo.csv", stringsAsFactors = FALSE, header = TRUE)

#input options
#the user can search based on these options
drugNames <- tolower(drugInfo$drugName)
drugCID <- drugInfo$CID

ui <- fluidPage(
  
  titlePanel("PolyMapper"),
  
  navbarPage("",
             tabPanel("App",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectizeInput('drugname1', label = "Drug 1", choices = drugNames, options = list(create = TRUE)),
                          selectizeInput('drugname2', label = "Drug 2", choices = drugNames, options = list(create = TRUE)),
                          selectizeInput('drugname3', label = "Drug 3", choices = drugNames, options = list(create = TRUE)),
                          selectizeInput('drugname4', label = "Drug 4", choices = drugNames, options = list(create = TRUE)),
                          selectizeInput('drugname5', label = "Drug 5", choices = drugNames, options = list(create = TRUE)),
                          actionButton("drugCalc", label = "Calculate")
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Drug 1", htmlOutput("drug1info"), imageOutput("drug1image")),
                            tabPanel("Drug 2", htmlOutput("drug2info"),imageOutput("drug2image")),
                            tabPanel("Drug 3", htmlOutput("drug3info"), imageOutput("drug3image")),
                            tabPanel("Drug 4", htmlOutput("drug4info"), imageOutput("drug4image")),
                            tabPanel("Drug 5", htmlOutput("drug5info"), imageOutput("drug5image"))
                          ),
                          
                          fluidRow(
                            box(title = "Shared Pathways", status = "warning", side = "left", textOutput("pathways")),
                            box(title = "Common Side Effects", status = "warning", side = "right", textOutput("effects"))
                          )
                        )
                      )),
             tabPanel("About",
                      includeMarkdown("polyMapperInfo/about.md"))
             )
)

server <- function(input, output) {
  
  observeEvent(input$drugCalc, {
    drugOneIndex <- which(drugNames == input$drugname1)
    drugTwoIndex <- which(drugNames == input$drugname2)
    drugThreeIndex <- which(drugNames == input$drugname3)
    drugFourIndex <- which(drugNames == input$drugname4)
    drugFiveIndex <- which(drugNames == input$drugname5)
    
    output$drug1info <- renderText(paste("Name: ", drugInfo[drugOneIndex, 1], "<br>CID: ", drugInfo[drugOneIndex, 2], "<br>IUPAC: ", drugInfo[drugOneIndex, 3], "<br>SMILE: ", drugInfo[drugOneIndex, 4], "<br>Molecular Formula: ", drugInfo[drugOneIndex, 5], "<br>Synonyms: ", drugInfo[drugOneIndex, 6]))
    output$drug1image <- renderPlot({
      plot(load.image("drugImages/aspirin.png"), axes = FALSE)
    })
    
    output$drug2info <- renderText(paste("Name: ", drugInfo[drugTwoIndex, 1], "<br>CID: ", drugInfo[drugTwoIndex, 2], "<br>IUPAC: ", drugInfo[drugTwoIndex, 3], "<br>SMILE: ", drugInfo[drugTwoIndex, 4], "<br>Molecular Formula: ", drugInfo[drugTwoIndex, 5], "<br>Synonyms: ", drugInfo[drugTwoIndex, 6]))
    output$drug2image <- renderPlot({
      plot(load.image("drugImages/mdma.png"), axes = FALSE)
    })
    
    output$drug3info <- renderText(paste("Name: ", drugInfo[drugThreeIndex, 1], "<br>CID: ", drugInfo[drugThreeIndex, 2], "<br>IUPAC: ", drugInfo[drugThreeIndex, 3], "<br>SMILE: ", drugInfo[drugThreeIndex, 4], "<br>Molecular Formula: ", drugInfo[drugThreeIndex, 5], "<br>Synonyms: ", drugInfo[drugThreeIndex, 6]))
    output$drug3image <- renderPlot({
      plot(load.image("drugImages/drug3.png"), axes = FALSE)
    })
    
    output$drug4info <- renderText(paste("Name: ", drugInfo[drugFourIndex, 1], "<br>CID: ", drugInfo[drugFourIndex, 2], "<br>IUPAC: ", drugInfo[drugFourIndex, 3], "<br>SMILE: ", drugInfo[drugFourIndex, 4], "<br>Molecular Formula: ", drugInfo[drugFourIndex, 5], "<br>Synonyms: ", drugInfo[drugFourIndex, 6]))
    output$drug4image <- renderPlot({
      plot(load.image("drugImages/drug3.png"), axes = FALSE)
    })
    
    output$drug5info <- renderText(paste("Name: ", drugInfo[drugFiveIndex, 1], "<br>CID: ", drugInfo[drugFiveIndex, 2], "<br>IUPAC: ", drugInfo[drugFiveIndex, 3], "<br>SMILE: ", drugInfo[drugFiveIndex, 4], "<br>Molecular Formula: ", drugInfo[drugFiveIndex, 5], "<br>Synonyms: ", drugInfo[drugFiveIndex, 6]))
    output$drug5image <- renderPlot({
      plot(load.image("drugImages/drug3.png"), axes = FALSE)
    })
    
    output$pathways <- renderText(commonPathways(drugOneIndex, drugTwoIndex, drugThreeIndex, drugFourIndex, drugFiveIndex))
    output$effects <- renderText(c("heartburn, nausea,", "indigestion,", "upset stomach"))
  })
}

commonPathways <- function(one, two, three, four, five) {
  
  cp <- intersect(intersect(intersect(intersect(drugInfo[one, 7], drugInfo[two, 7]), drugInfo[three, 7]), drugInfo[four, 7]), drugInfo[five, 7])
  
  if (length(cp) < 1) {
    return ("No common pathways")
  } else if (cp == "") {
    return ("No common pathways")
  } else {
    return (cp)
  }
}


shinyApp(ui = ui, server = server)