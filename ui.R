# Guillaume Lobet - University of Liege
# This file contains the User Interface details for the Shiny app.


library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("- CRootBox -")),
  
  # Sidebar with a Simulations outputs
  sidebarLayout(
    sidebarPanel(
      
      helpText("Guillaume Lobet ... ."),
      
      tags$hr(),      
      
      actionButton(inputId = "runROOTBOX", label="Unleash CRootBox"),
      tags$hr(),   
      
      selectInput("dataset", label = "Select dataset", choices = c("Please load datafile")), # updated with the datafile
      
      tags$hr(),   
      
      
      sliderInput("time_in_sim", "Time in simulation:", min=10, max=20, value=10),
      
      sliderInput("P_nbMaxPrim", "Number of primary:", min=1, max=10, value=2, step = 1),
      
      sliderInput("P_distRamif", "Interbranch distance:", min=0.2, max=2, value=1.5, step=0.1),
      
      sliderInput("P_coeffCroissRad", "Radial growth:", min=0, max=0.4, value=0, step=0.05),
      
      sliderInput("P_maxLatAge", "Lateral max age:", min=5, max=20, value=5, step=1),
      
      sliderInput("P_penteVitDiam", "Slope growth/diam:", min=20, max=60, value=20, step=2),
      
      # Run the simulations
      
      tags$hr()
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(     
        tabPanel("Root system",
                 fluidRow(
                   column(5,
                          h6("Root system representation"),
                          plotOutput("rootPlot")
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