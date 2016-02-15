library(shiny)
#rsconnect::deployApp
#rsconnec::showlog(streaming=TRUE)
#options(shiny.trace=TRUE)
#options(fullstacktrce=TRUE)
#options(shiny.error = browser)
#options(shiny.reactlog=TRUE)
#library(profvis)




  rm(list=ls(all=TRUE))
  base_dir <- paste0(gsub("\\\\","/",Sys.getenv("dropbox_path")),"/BEAMA/Rmodules")
  shiny_dir <- paste0(base_dir,"/shiny/bts")
  setwd(shiny_dir)
  #profvis({
    runApp(shiny_dir,launch.browser=TRUE) #,display.mode = "showcase"
  #})

#devtools::install_github("rstudio/shiny")

# devtools::install_github('hadley/ggplot2')
# devtools::install_github('thomasp85/ggforce')
# devtools::install_github('thomasp85/ggraph')