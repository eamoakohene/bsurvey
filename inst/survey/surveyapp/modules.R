##### experimental #####
# survey_tabUI <- function(
#   id,
#   qn=42,
#   yr_range=c(2011,2016),yr_select=c(2015,2016),
#   qtr_range=c(1,4),qtr_select=c(1,4),
#   trend_name = "Optimism",
#   is_pc = FALSE
# ){
#
#   ns <- NS(id)
#
#   tabItem(
#     tabName=ns("menu")
#     ,fluidPage(
#
#       #header
#       shinyjs::hidden( div(id = ns("hidden_qn"), numericInput(inputId=ns("qn"), label="qn", value=qn)   )),
#       h1( btsq$new(qn=qn)$select_row()$head)
#       ,div(
#         ifelse(is_pc
#                ,survey_trends_get_desc(qn=qn,mth=survey_mth,yr=survey_yr,trend_name = trend_name,is_auto = TRUE)
#                ,survey_trends_get_general(qn=qn,mth=survey_mth,yr=survey_yr,trend_name = trend_name)
#         )
#       )
#
#       #chart
#       ,fluidRow(
#         column(6,  wellPanel(plotOutput( ns("vert")))),
#         column(6,  wellPanel(plotOutput(ns("horiz"))))
#       )
#       ,fluidRow(
#         column(12,  wellPanel(plotOutput(ns("line"))))
#       )
#
#       #chart-controls
#
#       ,fluidRow(
#         column(6,wellPanel( sliderInput(ns("yr"),   label = h4("Years"), min = yr_range[1], max = yr_range[2], value =yr_select,sep=""))),
#         column(6,wellPanel( sliderInput(ns("mth"),  label = h4("Quarter"), min =qtr_range[1], max = qtr_range[2], value =qtr_select,sep="")))
#       )
#
#     ) #fluidPage
#   )#tab-optmism
#
# }
#
# survey_tab <- function(input,ouput,session){
#
#   output$vert <- renderPlot({
#     survey_plot_bar_grid(
#       qn= as.character(input$qn), qsn='', x_axis = 'period' ,ytitle="",
#       y1=input$yr[1], y2=input$yr[2],
#       m1=input$qtr[1]*3, m2=input$qtr[2]*3,
#       show_title = FALSE, show_legend = FALSE
#     )
#   })
#
#   output$horiz <- renderPlot({
#     survey_plot_bar_grid(
#       qn=as.character(input$qn), qsn='', x_axis = 'opts' ,ytitle="",
#       y1=input$yr[1], y2=input$yr[2],
#       m1=input$qtr[1]*3, m2=input$qtr[2]*3,
#       show_title = FALSE, show_legend = FALSE
#     )
#   })
#
#
#   output$line <- renderPlot({
#     survey_plot_line_grid(
#       qn=as.character(input$qn), qsn='', ytitle="",
#       y1=input$yr[1], y2=input$yr[2],
#       m1=input$qtr[1]*3, m2=input$qtr[2]*3,
#       show_title = FALSE, show_legend = FALSE
#     )
#   })
# }
