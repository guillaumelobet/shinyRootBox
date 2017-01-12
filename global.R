##################################################################
###### Load libraries
##################################################################

packages <- c("ggplot2", "plyr", "plotly")
for(p in packages){
  if (!require(p,character.only = TRUE)){
    install.packages(p, dep=TRUE)
    if(!require(p,character.only = TRUE)) stop("Package not found")
  }
}
