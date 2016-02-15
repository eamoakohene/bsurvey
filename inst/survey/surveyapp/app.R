#file sources

files_src <- c(
   'db_lite.R','db_lite_trends.R','lite_stored_proc.R', 'bts.R'
  ,'global.R'
  ,'ui_menu.R','ui_tabs.R'
  ,'ui_tabs_optimism.R','ui_tabs_sales_volumes.R','ui_tabs_sales_margins.R'
  ,'ui_tabs_exports.R','ui_tabs_imports.R','ui_tabs_constraint.R','ui_tabs_unit_cost.R'
  ,'ui_tabs_impact_cost.R','ui_tabs_labour.R','ui_tabs_capacity.R','ui_tabs_capital_investment.R'
  ,'ui_tabs_company_information.R','ui_tabs_dashboard.R','ui_tabs_admin.R'
)

sapply(files_src,FUN = source)


 ui <- dashboardPage(
   skin="blue",

   dashboardHeader(
      title= survey_title
     ,titleWidth = 350
   ),

  dashboardSidebar(get_ui_menu()),

  dashboardBody(

     tags$head(tags$style(HTML( menu_bar_css ))),

     shinyjs::useShinyjs()
     ,get_ui_tabs()
    ),

   title= survey_title

 )


server <- function(input, output, session) {

  ## Each survey question has a ui tab. Each ui tab has code is call from set_ui_server (a function in ui_tabs.R)

  #OPTIMISM
  set_ui_server(input=input,output=output,session=session, tab="optimism") #### 1

  #SALES VOLUME
  set_ui_server(input=input,output=output,session=session, tab="sales_prvq")#### 2  - previous quarter
  set_ui_server(input=input,output=output,session=session, tab="sales_nxtq")#### 2 SALES VOLUME - next quarter
  set_ui_server(input=input,output=output,session=session, tab="sales_prvy")#### 2 SALES VOLUME - previous year
  set_ui_server(input=input,output=output,session=session, tab="sales_nxty")#### 2 SALES VOLUME - next year

  #SALES MARGINS
  set_ui_server(input=input,output=output,session=session, tab="margins_uk_prvq")
  set_ui_server(input=input,output=output,session=session, tab="margins_uk_prvy")
  set_ui_server(input=input,output=output,session=session, tab="margins_exp_prvq")
  set_ui_server(input=input,output=output,session=session, tab="margins_exp_prvy")


  #EXPORTS
  set_ui_server(input=input,output=output,session=session, tab="exports_prvq")
  set_ui_server(input=input,output=output,session=session, tab="exports_nxtq")
  set_ui_server(input=input,output=output,session=session, tab="exports_prvy")
  set_ui_server(input=input,output=output,session=session, tab="exports_nxty")
  set_ui_server(input=input,output=output,session=session, tab="exports_keym") # key market
  set_ui_server(input=input,output=output,session=session, tab="exports_keyf") # key factor
  set_ui_server(input=input,output=output,session=session, tab="exports_prop") # proportion of sales

  #IMPORTS
  set_ui_server(input=input,output=output,session=session, tab="imports_prvq")
  set_ui_server(input=input,output=output,session=session, tab="imports_nxtq")

  #CONSTRAINTS
  set_ui_server(input=input,output=output,session=session, tab="constraint")

  #UNIT COST
  set_ui_server(input=input,output=output,session=session, tab="ucost_prvq")
  set_ui_server(input=input,output=output,session=session, tab="ucost_nxtq")
  set_ui_server(input=input,output=output,session=session, tab="ucost_prvy")
  set_ui_server(input=input,output=output,session=session, tab="ucost_nxty")

  #COST IMPACT
  set_ui_server(input=input,output=output,session=session, tab="cost_energy")
  set_ui_server(input=input,output=output,session=session, tab="cost_rates")
  set_ui_server(input=input,output=output,session=session, tab="cost_fuel")
  set_ui_server(input=input,output=output,session=session, tab="cost_materials")
  set_ui_server(input=input,output=output,session=session, tab="cost_taxes")
  set_ui_server(input=input,output=output,session=session, tab="cost_wages")
  set_ui_server(input=input,output=output,session=session, tab="cost_rates")

 #LABOUR
  set_ui_server(input=input,output=output,session=session, tab="labour_cost_prvy")
  set_ui_server(input=input,output=output,session=session, tab="labour_cost_nxty")
  set_ui_server(input=input,output=output,session=session, tab="labour_force_prvy")
  set_ui_server(input=input,output=output,session=session, tab="labour_force_nxty")


 #CAPACITY
  set_ui_server(input=input,output=output,session=session, tab="capacity_prvq")
  set_ui_server(input=input,output=output,session=session, tab="capacity_nxtq")
  set_ui_server(input=input,output=output,session=session, tab="capacity_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capacity_nxty")

 #CAPEX
  set_ui_server(input=input,output=output,session=session, tab="capex_customer_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_customer_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_export_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_export_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_ebusiness_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_ebusiness_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_rates_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_rates_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_product_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_product_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_plant_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_plant_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_rd_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_rd_nxty")

  set_ui_server(input=input,output=output,session=session, tab="capex_structures_prvy")
  set_ui_server(input=input,output=output,session=session, tab="capex_structures_nxty")

 #COMPANY INFORMATION
  set_ui_server(input=input,output=output,session=session, tab="company_labour")
  set_ui_server(input=input,output=output,session=session, tab="company_sector")
  set_ui_server(input=input,output=output,session=session, tab="company_turnover")
  ##----------


 ##ADMIN tab is different and has its own unique server codes.

  #reactive values for plots
  sqn <- reactiveValues( qn = survey_que_first ,pqn = survey_que_first )

  #admin server codes function
  set_ui_server_admin(input=input,output=output,session=session,sqn = sqn)



## hidden menu
 observe({

   query <- parseQueryString(session$clientData$url_search)

   if (length(query) == 0) {
     shinyjs::hide('adm_menu_update')
   }else{
     is_admin_zero <- (length(query$is_admin) == 0)
     if (!is_admin_zero && (query$is_admin == 'badd')) {
       shinyjs::show('adm_menu_update')

     }else{
       shinyjs::hide('adm_menu_update')
     }
   }

 }) #observe



}#server-ends


shinyApp(ui, server)
