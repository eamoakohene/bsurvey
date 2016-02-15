get_ui_tab_admin <- function(id){


  tabItem(
    tabName = "admin-menu", icon= icon("usd")
    ,fluidPage(
      column(12,
      box(
        title="Comments", width = 6
         ,column(3, actionLink("btn_first","<<first"))
         ,column(3, actionLink("btn_back","back",icon = icon("arrow-left")))

         ,column(3, actionLink("btn_next","next",icon = icon("arrow-right")))
         ,column(3, actionLink("btn_last","last>>"))

        ,column(12, shinyjs::disabled(textInput("cmt_qn","Number")))
        ,column(12, textInput("cmt_q","Que"))
        ,column(12, textInput("cmt_head","Headline"))
        ,column(12, textareaInput("cmt_comment","Comment"))
        ,column(3, actionButton("btn_new","New", icon = icon("file")))
        ,column(3, actionButton("btn_save","Save"),icon = icon("save"))
        ,column(3, shinyjs::disabled(actionButton("btn_delete","Delete"),icon = icon("cut")))
        ,column(3, shinyjs::disabled(actionButton("btn_cancel","Cancel"),icon = icon("rotate-left")))
        ,column(12, textOutput("sqltxt"))

      )
      ,column(6,
            tabsetPanel(
              tabPanel("Period", plotOutput("admin_plot_vert")),
              tabPanel("Opts",  plotOutput("admin_plot_horiz"))
            )
        )


      )#col 12
      ,fluidRow(
        column(12,plotOutput("admin_plot_line"))
      )
      ,fluidRow(

          column(6,wellPanel( sliderInput( "admin_yr",  label = h4("Years"), min = slider_range_yr[1], max = slider_range_yr[2], value =slider_select_yr,sep=""))),
          column(6,wellPanel( sliderInput( "admin_qtr",  label = h4("Quarter"), min =slider_range_qtr[1], max = slider_range_qtr[2], value = slider_select_qtr,sep="")))
        )
      )
    )

}


set_ui_server_admin <- function(input,output,session,sqn){

  ##admin_plots - vertical
  output$admin_plot_vert <- renderPlot({

    survey_plot_bar_grid(
      qn= sqn$qn, qsn='', x_axis = 'period' ,ytitle="",
      y1=input$admin_yr[1], y2=input$admin_yr[2],
      m1=input$admin_qtr[1]*3, m2=input$admin_qtr[2]*3,
      show_title = FALSE, show_legend = FALSE
    )
  })

  output$admin_plot_horiz <- renderPlot({

    survey_plot_bar_grid(
      qn= sqn$qn, qsn='', x_axis = 'opts' ,ytitle="",
      y1=input$admin_yr[1], y2=input$admin_yr[2],
      m1=input$admin_qtr[1]*3, m2=input$admin_qtr[2]*3,
      show_title = FALSE, show_legend = FALSE
    )
  })

  output$admin_plot_line <- renderPlot({

    survey_plot_line_grid(
      qn=sqn$qn, qsn='', ytitle="",
      y1=input$admin_yr[1], y2=input$admin_yr[2],
      m1=input$admin_qtr[1]*3, m2=input$admin_qtr[2]*3,
      show_title = FALSE, show_legend = FALSE
    )
  })

  #######
  observeEvent(input$btn_first,{
    sqn$pqn <- sqn$qn
    sqn$qn <- survey_que_first
  })

  observeEvent(input$btn_last,{
    sqn$pqn <- sqn$qn
    sqn$qn <- survey_que_last
  })

  observeEvent( input$btn_back,{
    sqn$pqn <- sqn$qn
    my_qn <- as.numeric(input$cmt_qn)
    my_qn <- my_qn - 1

    if( my_qn < 1){
      my_qn <- survey_que_first
    }

    sqn$qn <- my_qn

  })

  observeEvent( input$btn_next,{
    sqn$pqn <- sqn$qn
    my_qn <- as.numeric(input$cmt_qn)
    my_qn <- my_qn + 1

    if( my_qn > survey_que_last){
      my_qn <- survey_que_last
    }

    sqn$qn <- my_qn

  })



  df <- reactive({
    btsq$new(qn= sqn$qn)$select_row()
  })

  observe({

    updateTextInput(session, "cmt_qn", value = df()$qn)
    updateTextInput(session, "cmt_q", value = df()$q)
    updateTextInput(session, "cmt_head", value = df()$head)
    updateTextInput(session, "cmt_comment", value = df()$comment)
  })


  observeEvent( input$btn_save, {
    #req(input$cmt_qn,input$cmt_q)

    abc <- btsq$new(
      qn= as.numeric(input$cmt_qn),
      qtxt = input$cmt_q,
      qhead = input$cmt_head,
      qcomment = input$cmt_comment
    )$update_row()
  })

  observeEvent(input$cancel,{
    sqn$qn <- sqn$pqn
  })

  #     output$sqltxt <- renderText({
  #       abc$get_update_sql()
  #     })


  ######
}