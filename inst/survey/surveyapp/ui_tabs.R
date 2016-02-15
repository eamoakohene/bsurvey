get_ui_tabs<-function(){

    tabItems(

             get_ui_tab_dashboard()
            ,get_ui_tab_optimism()

            ,get_ui_tab_sales_volume_prev_qtr()
            ,get_ui_tab_sales_volume_next_qtr()
            ,get_ui_tab_sales_volume_prev_yr()
            ,get_ui_tab_sales_volume_next_yr()

            ,get_ui_tab_sales_margins_uk_prev_qtr()
            ,get_ui_tab_sales_margins_uk_prev_yr()
            ,get_ui_tab_sales_margins_exports_prev_qtr()
            ,get_ui_tab_sales_margins_exports_prev_yr()

            ,get_ui_tab_exports_prev_qtr()
            ,get_ui_tab_exports_prev_yr()
            ,get_ui_tab_exports_next_qtr()
            ,get_ui_tab_exports_next_yr()
            ,get_ui_tab_exports_key_market()
            ,get_ui_tab_exports_key_factor()
            ,get_ui_tab_exports_proportion()

            ,get_ui_tab_imports_prev_qtr()
            ,get_ui_tab_imports_next_qtr()

            ,get_ui_tab_constraint()

            ,get_ui_tab_unit_cost_prev_qtr()
            ,get_ui_tab_unit_cost_prev_yr()
            ,get_ui_tab_unit_cost_next_qtr()
            ,get_ui_tab_unit_cost_next_yr()

            ,get_ui_tab_impact_cost_energy()
            ,get_ui_tab_impact_cost_fuel()
            ,get_ui_tab_impact_cost_rates()
            ,get_ui_tab_impact_cost_materials()
            ,get_ui_tab_impact_cost_taxes()
            ,get_ui_tab_impact_cost_wages()

            ,get_ui_tab_labour_cost_prev_yr()
            ,get_ui_tab_labour_cost_next_yr()
            ,get_ui_tab_labour_force_prev_yr()
            ,get_ui_tab_labour_force_next_yr()

            ,get_ui_tab_capacity_prev_qtr()
            ,get_ui_tab_capacity_prev_yr()
            ,get_ui_tab_capacity_next_qtr()
            ,get_ui_tab_capacity_next_yr()

            ,get_ui_tab_capex_customer_prev_yr()
            ,get_ui_tab_capex_ebusiness_prev_yr()
            ,get_ui_tab_capex_product_prev_yr()
            ,get_ui_tab_capex_export_prev_yr()
            ,get_ui_tab_capex_rd_prev_yr()
            ,get_ui_tab_capex_structures_prev_yr()
            ,get_ui_tab_capex_plant_prev_yr()

            ,get_ui_tab_capex_customer_next_yr()
            ,get_ui_tab_capex_ebusiness_next_yr()
            ,get_ui_tab_capex_product_next_yr()
            ,get_ui_tab_capex_export_next_yr()
            ,get_ui_tab_capex_rd_next_yr()
            ,get_ui_tab_capex_structures_next_yr()
            ,get_ui_tab_capex_plant_next_yr()

            ,get_ui_tab_company_labour()
            ,get_ui_tab_company_sector()
            ,get_ui_tab_company_turnover()

            ,get_ui_tab_admin()

  )#tabItems

}



get_ui_tab_template <- function(
  tab="optimism",qn=42,
  yr_range=c(2011,2016),yr_select=c(2015,2016),
  qtr_range=c(1,4),qtr_select=c(1,4),
  trend_name = "Optimism",
  is_pc = FALSE
){

  ns <- NS(tab)

  tabItem(
    tabName=ns("menu")
    ,fluidPage(

      #header
       shinyjs::hidden( div(id = ns("hidden-id"), numericInput( inputId=ns("qn"), label="qn", value=qn)   )),
       h1( btsq$new(qn=qn)$select_row()$head )
      ,div(
        ifelse(is_pc
               ,survey_trends_get_desc(qn=qn,mth=survey_mth,yr=survey_yr,trend_name = trend_name,is_auto = TRUE)
               ,survey_trends_get_general(qn=qn,mth=survey_mth,yr=survey_yr,trend_name = trend_name)
        )
       )

      #chart
      ,fluidRow(
        column(6,  wellPanel(plotOutput( ns("vert")  ))),
        column(6,  wellPanel(plotOutput( ns("horiz") )))
      )
      ,fluidRow(
        column(12,  wellPanel(plotOutput( ns("line") )))
      )

      #chart-controls

      ,fluidRow(
        column(6,wellPanel( sliderInput( ns("yr"),  label = h4("Years"), min = yr_range[1], max = yr_range[2], value =yr_select,sep=""))),
        column(6,wellPanel( sliderInput( ns("qtr"),  label = h4("Quarter"), min =qtr_range[1], max = qtr_range[2], value =qtr_select,sep="")))
      )

    ) #fluidPage
  )#tab-optmism

}

set_ui_server <- function(input,output,session, tab="optimism"){

  ns <- NS(tab)


  id_qn <- ns("qn")
  id_yr <- ns("yr")
  id_qtr <- ns("qtr")

  id_vert <- ns("vert")
  id_horiz <- ns("horiz")
  id_line <- ns("line")


  output[[id_vert]] <- renderPlot({
    survey_plot_bar_grid(
      qn= as.character(input[[id_qn]]), qsn='', x_axis = 'period' ,ytitle="",
      y1=input[[id_yr]][1], y2=input[[id_yr]][2],
      m1=input[[id_qtr]][1]*3, m2=input[[id_qtr]][2]*3,
      show_title = FALSE, show_legend = FALSE
    )
  })

  output[[id_horiz]] <- renderPlot({
    survey_plot_bar_grid(
      qn=as.character(input[[id_qn]]), qsn='', x_axis = 'opts' ,ytitle="",
      y1=input[[id_yr]][1], y2=input[[id_yr]][2],
      m1=input[[id_qtr]][1]*3, m2=input[[id_qtr]][2]*3,
      show_title = FALSE, show_legend = FALSE
    )
  })


  output[[id_line]] <- renderPlot({
    survey_plot_line_grid(
      qn=as.character(input[[id_qn]]), qsn='', ytitle="",
      y1=input[[id_yr]][1], y2=input[[id_yr]][2],
      m1=input[[id_qtr]][1]*3, m2=input[[id_qtr]][2]*3,
      show_title = FALSE, show_legend = FALSE
    )
  })


}


