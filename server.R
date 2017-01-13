# Guillaume Lobet - University of Liege

shinyServer(
  
  function(input, output, clientData, session) {  
    
    rs <- reactiveValues(rootsystem = NULL, dataset = NULL, plant = NULL, params = NULL)
    
    observe({
      ls <- list.files("www/modelparameter/")
      ls <- ls[grepl("rparam", ls)]
      ls <- gsub(".rparam", "", ls)
      updateSelectInput(session, "dataset", choices = ls)  
    })  


    
    # For the root paramerers
    observe({
      if(is.null(rs$dataset)){return()}
        dataset <- rs$dataset
        sel1 <- input$roottype
        sel2 <- input$parameter
        updateSelectInput(session, "roottype", choices = unique(dataset$name), selected = sel1)
        updateSelectInput(session, "parameter", choices = unique(dataset$param[dataset$name == input$roottype]), selected = sel2)
        current <- as.numeric(dataset$val1[dataset$name == input$roottype & dataset$param == input$parameter])
        if(length(current) == 0) current <- 0
        newMin <- 0
        newMax <- current * 2
        if(newMax == 0) newMax <- 1
        updateSliderInput(session, "value", value = current, min = newMin, max = newMax, step = current/10)
    })
    
    

    output$paramTitle <- renderUI( {
      if(is.null(rs$params)){return()}
      params <- rs$params
      params$name[params$name == input$parameter]
    })     
    output$paramText <- renderUI( {
      if(is.null(rs$params)){return()}
      params <- rs$params
      params$text[params$name == input$parameter]
    })  
    output$plantText <- renderUI( {
      if(is.null(rs$params)){return()}
      params <- rs$params
      params$text[params$name == input$parameter2]
    })  
    output$plantTitle <- renderUI( {
      if(is.null(rs$params)){return()}
      params <- rs$params
      params$name[params$name == input$parameter2]
    })      
    
    # For the plant paramerers
    observe({
      if(is.null(rs$plant)){return()}
      dataset <- rs$plant
      sel <- input$parameter2
      updateSelectInput(session, "parameter2", choices = unique(dataset$param), selected = sel)
      current <- as.numeric(dataset$val1[dataset$param == input$parameter2])
      if(length(current) == 0) current <- 0
      newMin <- 0
      newMax <- current * 2
      if(newMax == 0) newMax <- 1
      updateSliderInput(session, "value2", value = current, min = newMin, max = newMax, step = current/10)
    })    

    
    #------------------------------------------------------
    # PROCESS THE DATA
    
    
    observeEvent(input$runROOTBOX, {
      
      file.copy(from=paste0("www/modelparameter/", input$dataset, ".rparam"), to="www/modelparameter/param.rparam", overwrite = T)
      file.copy(from=paste0("www/modelparameter/", input$dataset, ".pparam"), to="www/modelparameter/param.pparam", overwrite = T)
      
      
      ## READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
      fileName <- 'www/modelparameter/param.rparam'
      param <- read_file(fileName)
      
      param <- strsplit(param, "#")
      dataset <- NULL
      for(k in c(2:length(param[[1]]))){
        spl <- strsplit(param[[1]][k], "\n")
        type <- ""
        name <- ""
        for(i in c(1:length(spl[[1]]))){
          temp <- spl[[1]][i]
          if(nchar(temp) > 0){
            temp <- strsplit(temp, "\t")
            temp2 <- data.frame("type" = character(0), "name" = character(0), 
                                "param" = character(0), "val1" = numeric(0), 
                                "val2" = numeric(0), "val3" = numeric(0), stringsAsFactors = F)
            
            if(temp[[1]][1] == "type"){ type <- temp[[1]][2]
            } else if(temp[[1]][1] == "name"){ name <- temp[[1]][2]
            } else if(grepl("Parameter", temp[[1]][1])){
            } else {
              for(j in c(1:length(temp[[1]]))){
                temp2[[1,j+2]] <- temp[[1]][j]
                temp2$type <- type
                temp2$name <- name
              }
              dataset <- rbind(dataset, temp2)
            }
          }
        }
      }        
      

      ## READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
      fileName <- 'www/modelparameter/param.pparam'
      plant <- read.table(fileName)
      colnames(plant) <- c("param", "val1")      
      
      fileName <- 'www/params.txt'
      params <- read.table(fileName, sep="\t", stringsAsFactors = F)
      colnames(params) <- c("name", "text")            

      setwd("www/")
      system("./a.out")  
      rootsystem <- read.table("rootsystem.txt", header = T)
      setwd("../")
      
      rs$rootsystem <- rootsystem
      rs$dataset <- dataset
      rs$plant <- plant
      rs$params <- params
      
    })
    
    
    
        
    observeEvent(input$updateParams, {
        
        if(is.null(rs$dataset)) return()
      
        ## READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
        dataset <- rs$dataset
        
        ## WRITE BACK THE PARAMETERSET
        dataset$val1[dataset$name == input$roottype & dataset$param == input$parameter] <- input$value
        types <- unique(dataset$type)
        text <- NULL
        for(t in types){
          if(is.null(text)){text <- "# Parameter set for type"
          }else{
            text <- paste(text, "# Parameter set for type", sep="\n")
          }
          temp <- dataset[dataset$type == t,]
          
          str <- paste("type", temp$type[1], sep="\t")
          text <- paste(text, str, sep="\n")
          
          str <- paste("name", temp$name[1], sep="\t")
          text <- paste(text, str, sep="\n")
          
          for(i in c(1:nrow(temp))){
            str <- paste(temp[i, 3], temp[i, 4], temp[i, 5], temp[i, 6], sep="\t")
            text <- paste(text, str, sep="\n")
          }
          
        }
        text <- gsub("\tNA", "", text)
        cat(text, file="www/modelparameter/param.rparam")
        
        
        ## READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
        plant <- rs$plant
        # WRITE THE NEW PARAMETER FILE
        plant$val1[plant$param == input$parameter2] <- input$value2
        text <- NULL
        for(i in c(1:nrow(plant))){
          str <- paste(plant[i, 1], plant[i, 2], sep="\t")
          text <- paste(text, str, sep="\n")
        }
        
        text <- gsub("\tNA", "", text)
        message(text)
        
        cat(text, file="www/modelparameter/param.pparam")
        
        
        
        
        
        setwd("www/")
        system("./a.out")  
        rootsystem <- read.table("rootsystem.txt", header = T)
        setwd("../")
        

        rs$rootsystem <- rootsystem
        rs$dataset <- dataset
        rs$plant <- plant
    })
    
    
    

    
    
    # ----------------------------------------------------------------
    # PLOT THE ROOT SYSTEM
    # ----------------------------------------------------------------
    
    rootPlot <- function(){
      
      plot <- ggplot() +  theme_classic()
      if(is.null(rs$rootsystem)){return(plot)}
      
      mydata <- rs$rootsystem

      plot <- plot + 
        geom_segment(data = mydata, aes(x = x1, y = z1, xend = x2, yend = z2, colour=factor(type)), alpha=0.9) + 
        coord_fixed() +
        ylab("Depth (cm)")
      
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
      if(is.null(rs$rootsystem)){return(plot)}
      
      mydata <- rs$rootsystem
      mydata$length = sqrt((mydata$x1-mydata$x2)^2 + (mydata$y1-mydata$y2)^2 + (mydata$z1-mydata$z2)^2 )
      mydata$z1 <- round(mydata$z1, 0)
      
      dens <- ddply(mydata, .(z1, type), summarise, root = sum(length))
      
      
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
    
    
    
    # ----------------------------------------------------------------
    # Downlaod the parameters
    # ----------------------------------------------------------------
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadParams <- downloadHandler(
      
      # if(is.null(rs$dataset)){return()}
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        # paste(input$dataset, input$filetype, sep = ".")
        "param.rparams"
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {

        dataset <- rs$dataset
        
        ## WRITE BACK THE PARAMETERSET
        
        dataset$val1[dataset$name == input$roottype & dataset$param == input$parameter] <- input$value
        
        types <- unique(dataset$type)
        
        text <- NULL
        for(t in types){
          if(is.null(text)){text <- "# Parameter set for type"
          }else{
            text <- paste(text, "# Parameter set for type", sep="\n")
          }
          temp <- dataset[dataset$type == t,]
          
          str <- paste("type", temp$type[1], sep="\t")
          text <- paste(text, str, sep="\n")
          
          str <- paste("name", temp$name[1], sep="\t")
          text <- paste(text, str, sep="\n")
          
          for(i in c(1:nrow(temp))){
            str <- paste(temp[i, 3], temp[i, 4], temp[i, 5], temp[i, 6], sep="\t")
            text <- paste(text, str, sep="\n")
          }
          
        }
        text <- gsub("\tNA", "", text)
        cat(text, file=file)        
        
      }
    )
    
    
    # ----------------------------------------------------------------
    # Downlaod the data
    # ----------------------------------------------------------------
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadRSML <- downloadHandler(
      
      # if(is.null(rs$dataset)){return()}
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        # paste(input$dataset, input$filetype, sep = ".")
        paste0(input$dataset, ".rsml")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        
        rsml <- read_file("www/rootsystem.txt")
        cat(rsml, file=file)
        
      }
    )    
    
    
    
  }
)