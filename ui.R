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
          
    ),
    img(src='logo.jpg', align = "left", width="80%")
    ),
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
           tabsetPanel(
             tabPanel("Root system representation",
                      plotOutput("rootPlot", height = "800px"),
                      tags$hr(),   
                      downloadButton("downloadRSML", "RSML"),
                      downloadButton("downloadCSV", "CSV"),
                      downloadButton("downloadVTP", "VTP"),
                      downloadButton("downloadPlot", "Plot"),
                      tags$hr(),
                      tableOutput('table_results'),
                      value=1
             ),
             tabPanel("Root length profile",
                  plotOutput("densityPlot"),
                  helpText("This plot show the root length profile for each root types. The bold lines represent the smooth density profile"),
                  tags$hr(),
                  tableOutput('table_results_1'),
                  value=2
              ),
             tabPanel("About CRootBox",
                      tags$hr(),
                      fluidRow(
                        column(6,
                               h3("What is CRootBox"),
                               helpText("The focus of CRootBox is the simulation of different types of root architecture, and to provide a generic interface for coupling with arbitrary soil/environmental models, e.g., in order to determine the impact of specific root architectures on function."),
                               h3("More about CRootBox"),
                               helpText("This web interface only display basic capabilities of CRootBox. For more, check out the official webpage."),
                               actionButton(inputId='ab1', label="CRootBox webpage", icon = icon("th"), onclick ="window.open('https://plant-root-soil-interactions-modelling.github.io/CRootBox/', '_blank')"),
                               h3("How to cite CRootBox"),
                               tags$strong("CRootBox: A structural-functional modelling framework for root systems."),
                               helpText("A Schnepf, D Leitner, M Landl, G Lobet, T Hieu Mai, S Morandage, C Sheng, M Zörner, J Vanderborght, H Vereecken"),
                               actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('https://doi.org/10.1101/139980', '_blank')")                            
                               ),
                        column(6,
                               h3("Licence"),
                               helpText("CRootBox is released under a GPL licence, which means that redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
                                        
                                        1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
                                        
                                        2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
                                        
                                        3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission."),
                               h3("Disclaimer"),
                               helpText("This software is provided by the copyright holders and contributors 'as is' and any express or implied warranties, including, but not limited to, the implied warranties of merchantability and fitness for a particular purpose are disclaimed. In no event shall the copyright holder or contributors be liable for any direct, indirect, incidental, special, exemplary, or consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data, or profits; or business interruption) however caused and on any theory of liability, whether in contract, strict liability, or tort (including negligence or otherwise) arising in any way out of the use of this software, even if advised of the possibility of such damage.")
                               )
                        ),
                      value=3
             )
           )
         )
    )
  )
)