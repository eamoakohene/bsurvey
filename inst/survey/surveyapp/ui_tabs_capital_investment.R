#### previous year
get_ui_tab_capex_customer_prev_yr<- function(){

  get_ui_tab_template(
    tab="capex_customer_prvy",qn=12,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "customer"
  )


}

get_ui_tab_capex_export_prev_yr <- function(){

  get_ui_tab_template(
    tab="capex_export_prvy",qn=13,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "export development"
  )


}

get_ui_tab_capex_ebusiness_prev_yr <- function(){

  get_ui_tab_template(
    tab="capex_ebusiness_prvy",qn=14,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "e-business"
  )


}

get_ui_tab_capex_product_prev_yr <- function(){

  get_ui_tab_template(
    tab="capex_product_prvy",qn=16,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "product development"
  )


}

get_ui_tab_capex_plant_prev_yr <- function(){

  get_ui_tab_template(
    tab="capex_plant_prvy",qn=15,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "plant & machinery"
  )


}

get_ui_tab_capex_rd_prev_yr <- function(){

  get_ui_tab_template(
    tab="capex_rd_prvy",qn=17,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "research & development"
  )


}

get_ui_tab_capex_structures_prev_yr <- function(){

  get_ui_tab_template(
    tab="capex_structures_prvy",qn=11,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "structures"
  )


}


##### end previous year


#### next year
get_ui_tab_capex_customer_next_yr<- function(){

  get_ui_tab_template(
    tab="capex_customer_nxty",qn=5,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr ,
    is_pc = TRUE, trend_name = "customer"
  )


}

get_ui_tab_capex_export_next_yr <- function(){

  get_ui_tab_template(
    tab="capex_export_nxty",qn=6,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "export development"
  )


}

get_ui_tab_capex_ebusiness_next_yr <- function(){

  get_ui_tab_template(
    tab="capex_ebusiness_nxty",qn=7,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "e-business"
  )


}

get_ui_tab_capex_product_next_yr <- function(){

  get_ui_tab_template(
    tab="capex_product_nxty",qn=9,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "product development"
  )


}

get_ui_tab_capex_plant_next_yr <- function(){

  get_ui_tab_template(
    tab="capex_plant_nxty",qn=8,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "plant & machinery"
  )


}

get_ui_tab_capex_rd_next_yr <- function(){

  get_ui_tab_template(
    tab="capex_rd_nxty",qn=10,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "research & development"
  )


}

get_ui_tab_capex_structures_next_yr <- function(){

  get_ui_tab_template(
    tab="capex_structures_nxty",qn=11,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "structures"
  )


}
#### end next year