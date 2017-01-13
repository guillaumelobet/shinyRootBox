# Guillaume Lobet - University of Liege
# This file contains the User Interface details for the Shiny app.


library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("CRootBox"),
  
  # Sidebar with a Simulations outputs
  sidebarLayout(
    sidebarPanel(
      
      helpText("This app display the capabilities of the CRootBox model."),
      
      helpText("Daniel Leitner, Guillaume Lobet, Andrea Schnepf"),
      
      helpText("Forschungszentrum Juelich GmbH"),
      
      tags$hr(),      
      
      # h4("Select root system type"),
      selectInput("dataset", label = "Select root system dataset", choices = c("Please load datafile")), # updated with the datafile
      actionButton(inputId = "runROOTBOX", label="Unleash CRootBox", icon("rocket")),
      
      tags$hr(),   
      
      # h4("Update root parameters"),
      selectInput("roottype", label = "Select root type", choices = c("Please load datafile")), # updated with the datafile
      selectInput("parameter", label = "Select parameter to change", choices = c("Please load datafile")), # updated with the datafile
      sliderInput("value", "Parameter value:", min=10, max=20, value=10),
     
      tags$hr(),   
      
      # h4("Update plant parameters"),
      selectInput("parameter2", label = "Select plant parameter to change", choices = c("Please load datafile")), # updated with the datafile
      sliderInput("value2", "Parameter value:", min=10, max=20, value=10),
      
       
      actionButton(inputId = "updateParams", label="Update root system", icon("refresh")),
      downloadButton("downloadParams", ""),
      
      # Run the simulations
      tags$hr(), 
      strong(htmlOutput("paramTitle")),
      htmlOutput("paramText"),
      
      tags$hr(), 
      strong(htmlOutput("plantTitle")),
      htmlOutput("plantText")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(     
        tabPanel("Root system",
                 fluidRow(
                   column(5,
                          h6("Root system representation"),
                          plotOutput("rootPlot"),
                          downloadButton("downloadRSML", "RSML file")
                          ),
                   column(5,
                          h6("Density plot"),
                          plotOutput("densityPlot")
                          ),
                   width="100%",
                   height="100%"
                 ),
                 value=1
        ),
        id="tabs1"
      )
    )
  )
))