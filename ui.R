# Guillaume Lobet - University of Liege
# This file contains the User Interface details for the Shiny app.


library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("CRootBox"),
  
  # Sidebar with a Simulations outputs
  fluidRow(
    column(3, 

      helpText("This app displays the capabilities of the CRootBox model. Choose a dataset, unleash CRootBox, then try changing the parameters."),
       
      helpText(tags$small("Daniel Leitner, Guillaume Lobet, Magdalena Landl, Mirjam Zorner, Shehan Morandage, Trung Hieu Mai, Cheng Sheng, Jan Vanderborgth, Andrea Schnepf")),
       
      helpText(tags$small("Forschungszentrum Juelich GmbH")),
      
      h3("1. Load parameter set"),
      
      wellPanel(
            
          # h4("Select root system type"),
          selectInput("dataset", label = "1. Select root system dataset", choices = c("Please load datafile")), # updated with the datafile
          htmlOutput("littTitle"),
          htmlOutput("littAuth"),
          htmlOutput("littRef"),
          htmlOutput("doi"),
          tags$hr(),   
          checkboxInput("bwfig", "Black and white root system", FALSE),
          actionButton(inputId = "runROOTBOX", label="Unleash CRootBox", icon("rocket"))
          
    )),
    column(3, 
      h3("2. Update parameters"),
      wellPanel(
          
          # h4("Update root parameters"),
          selectInput("roottype", label = "2. Select root type", choices = c("Please load datafile")), # updated with the datafile
          selectInput("parameter", label = "Select parameter to change", choices = c("Please load datafile")), # updated with the datafile
          sliderInput("value", "Parameter mean:", min=10, max=20, value=10),
          sliderInput("stdev", "Parameter deviation [%]:", min=0, max=50, value=0, step=5),
          strong(htmlOutput("paramTitle")),
          htmlOutput("paramText"),
          
          tags$hr(),   
          
          # h4("Update plant parameters"),
          selectInput("parameter2", label = "3. Select plant parameter to change", choices = c("Please load datafile")), # updated with the datafile
          sliderInput("value2", "Parameter value:", min=10, max=20, value=10),
          strong(htmlOutput("plantTitle")),
          htmlOutput("plantText"),
          
          tags$hr(),   
          
          actionButton(inputId = "updateParams", label="Update root system", icon("refresh")),
          downloadButton("downloadParams", "")
      
      
    )),
    
    # Show a plot of the generated distribution
    column(6,
         fluidRow(
           column(5,
                  h4("Root length profile"),
                  plotOutput("densityPlot"),
                  helpText("This plot show the root length profile for each root types. The bold lines represent the smooth density profile"),
                  tags$hr(),   
                  tableOutput('table_results')
           ),
           column(5,
                  h4("Root system representation"),
                  plotOutput("rootPlot"),
                  tags$hr(),   
                  #downloadButton("downloadRSML", "RSML"),
                  downloadButton("downloadCSV", "CSV"),
                  downloadButton("downloadVTP", "VTP"),
                  downloadButton("downloadPlot", "Plot")
           ),

           width="100%",
           height="100%"
         )

    )
  )
))