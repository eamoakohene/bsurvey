##LIBRARIES
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(networkD3)
library(quantmod)
library(pipeR)
library(scales)
library(shinyjs)


# BEAMA colors
beama2_colour_corporate_blue     <- '#3a6f8f'
beama2_colour_grayblue         <- '#6d92a8'
beama2_colour_lime  <- '#d5c40a'
beama2_colour_pink    <- '#b63e97'
beama2_colour_limegreen <- '#a9a613'
beama2_colour_gray      <- '#73808b'
beama2_colour_blue      <- '#30a4dc'
beama2_colour_darkyellow<- '#dd7c1f'
beama2_colour_yellow    <- '#ffcd34'
beama2_colour_green    <- '#649e35'

BOOTSTRAP_BLUE <- '#337AB7'

beama_header <- c("#78A22F", beama2_colour_pink  ,"#6A737B", "#80A1B6")

beama2s_colours <- c(
  beama2_colour_corporate_blue ,
  beama2_colour_grayblue,
  beama2_colour_lime,
  beama2_colour_pink ,
  beama2_colour_limegreen,
  beama2_colour_gray ,
  beama2_colour_blue ,
  beama2_colour_darkyellow,
  beama2_colour_yellow ,
  beama2_colour_green
)

beama2_colours <- c(
  BOOTSTRAP_BLUE,

  beama2_colour_blue ,
  beama2_colour_corporate_blue ,
  beama2_colour_grayblue,
  beama2_colour_lime,
  beama2_colour_pink ,
  beama2_colour_limegreen,
  beama2_colour_gray ,
  beama2_colour_blue ,
  beama2_colour_darkyellow,
  beama2_colour_yellow ,
  beama2_colour_green
)
BEAMA_BAR_COLOURS <-  beama2_colours[-c(1)]

BEAMA_LINE_COLOURS <- c(
  beama2_colour_corporate_blue,
  beama2_colour_limegreen,
  beama2_colour_pink,
  beama2_colour_blue,
  beama2_colour_darkyellow,

  beama2_colour_grayblue,

  beama2_colour_lime,
  beama2_colour_green,
  beama2_colour_gray

)

LINECOLOUR <- BOOTSTRAP_BLUE
SMOOTHCOLOUR <- beama2_colour_pink
PLUSMINUS<- c("#377EB8","#E41A1C")

# Plot dimensions
PLOT_HEIGHT  <- "500px"
PLOT_HEIGHT_XL  <- "650px"
PLOT_HEIGHT_SM  <- "250px"
PLOT_FONT_SIZE_GEOM_TEXT <- 5

# Database name
SQLITE_DB  <- "bts.sqlite"


### SURVEY SETTINGS ####

survey_title <- "BEAMA Trends Survey - 2015 Q4"

cur_period <- lite_run_sql("select yr,mth from bts2 order by yr desc,mth desc limit 1")
survey_mth <- cur_period$mth
survey_yr <-  cur_period$yr



slider_range_yr <- c(2011,2016)
slider_range_qtr <- c(1,4)
slider_select_yr <- c(2015,2016)
slider_select_qtr <- c(3,4)

survey_que_first <- 1
survey_que_last <- 54

survey_que_current <- survey_que_first

menu_bar_css <- '
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '

### SURVEY SETTINGS ENDS ####


#extending textareInput to enable update from shiny server
textareaInput <- function(id, label, value=NULL, rows=4, cols=35, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

updateTextareaInput <- function (session, inputId, label = NULL, value = NULL){
    message <- dropNulls(list(label = label, value = value))
    session$sendInputMessage(inputId, message)

}

#pinched from shiny source
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

# reverse engineered shiny function NS() to allow for post-fix as oppose to pre-fix
# SN - space name
SN <- function (id, namespace = NULL){
  if (missing(namespace)) {
    function(namespace) {
      paste(c(namespace,id), collapse = ns.sep)
    }
  }
  else {
    paste(c(namespace,id), collapse = ns.sep)
  }
}
