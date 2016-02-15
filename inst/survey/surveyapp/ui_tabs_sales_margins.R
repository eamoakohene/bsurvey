
get_ui_tab_sales_margins_uk_prev_qtr <- function(){

  get_ui_tab_template(
    tab="margins_uk_prvq",qn=48,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Margins of UK sales"
  )


}

get_ui_tab_sales_margins_uk_prev_yr <- function(){

  get_ui_tab_template(
    tab="margins_uk_prvy",qn=49,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Margins on UK sales"
  )


}

get_ui_tab_sales_margins_exports_prev_qtr <- function(){

  get_ui_tab_template(
    tab="margins_exp_prvq",qn=46,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Margins on export sales"
  )


}

get_ui_tab_sales_margins_exports_prev_yr <- function(){

  get_ui_tab_template(
    tab="margins_exp_prvy",qn=47,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Margins on export sales"
  )


}