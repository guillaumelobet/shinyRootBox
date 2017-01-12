# Guillaume Lobet - University of Liege

shinyServer(
  
  function(input, output, clientData, session) {  
    
    
    observe({
      ls <- list.files("www/modelparameter/")
      ls <- ls[grepl("rparam", ls)]
      ls <- gsub(".rparam", "", ls)
      updateSelectInput(session, "dataset", choices = ls)  
    })    
    
    
    #------------------------------------------------------
    # PROCESS THE DATA
    
    
    Results <- reactive({
      
      
      
      if(input$runROOTBOX == 0){return("NOTINHG HERE")}
    
        file.copy(from=paste0("www/modelparameter/", input$dataset, ".rparam"), to="www/modelparameter/param.rparam", overwrite = T)
        file.copy(from=paste0("www/modelparameter/", input$dataset, ".pparam"), to="www/modelparameter/param.pparam", overwrite = T)
      
        setwd("www/")
        system("./a.out")  
        rs <- read.table("rootsystem.txt", header = T)
        setwd("../")
        return(rs)
    })
    
    
    
    output$caption1 <- renderUI( {
      if(input$runROOTBOX == 0){return("Nothing here yet")}
      "Simulation done"
    })
    
    
    # ----------------------------------------------------------------
    # PLOT THE ROOT SYSTEM
    # ----------------------------------------------------------------
    
    rootPlot <- function(){
      
      plot <- ggplot() +  theme_classic()
      if(input$runROOTBOX == 0){return(plot)}
      
      rs <- Results()
      rs$width <- rs$radius * 10
      
      
      plot <- plot + 
        geom_segment(data = rs, aes(x = x1, y = z1, xend = x2, yend = z2, 
                         colour=factor(type)), alpha=0.9) + 
        # theme(axis.line = element_blank(), 
        #       axis.text.x = element_blank(),
        #       axis.text.y = element_blank(),
        #       axis.ticks = element_blank(),
        #       axis.title.x = element_blank(),
        #       panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        #       plot.background = element_rect(fill = "transparent",colour = NA)) +
        coord_fixed() +
        ylab("Depth (cm)") #+
        # geom_hline(yintercept=0, colour="grey") + 
        # scale_colour_manual(values=c("red", "green")) +
        #theme(legend.position="none")
      
      # ggplotly(plot)
      plot
    }
    
    output$rootPlot <- renderPlot({
      print(rootPlot())
    })
    
    
    
    # ----------------------------------------------------------------
    # PLOT THE DENSITY
    # ----------------------------------------------------------------
    
    
    densityPlot <- function(){
      
      plot <- ggplot() +  theme_classic()
      if(input$runROOTBOX == 0){return(plot)}
      
      rs <- Results()
      rs$length = sqrt((rs$x1-rs$x2)^2 + (rs$y1-rs$y2)^2 + (rs$z1-rs$z2)^2 )
      rs$z1 <- round(rs$z1, 0)
      
      dens <- ddply(rs, .(z1, type), summarise, root = sum(length))
      
      
      plot <- ggplot(data = dens, aes(x = z1, y = root, colour=factor(type))) +  
        theme_classic() + 
        geom_line(data = dens, aes(x = z1, y = root, colour=factor(type)), alpha=0.5) + 
        stat_smooth(se=F) + 
        coord_flip() +
        xlab("depth (cm)") +
        ylab("total root length (cm)")
      plot
    }
    
    output$densityPlot <- renderPlot({
      print(densityPlot())
    })    
    
    
    
    
  }
)