
get_ui_tab_sales_volume_prev_qtr <- function(){

  get_ui_tab_template(
    tab="sales_prvq",qn=50,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Sales Volume"
  )


}

get_ui_tab_sales_volume_next_qtr <- function(){

  get_ui_tab_template(
    tab="sales_nxtq",qn=44,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected Sales Volume"
  )


}

get_ui_tab_sales_volume_prev_yr <- function(){

  get_ui_tab_template(
    tab="sales_prvy",qn=51,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Sales Volume"
  )


}

get_ui_tab_sales_volume_next_yr <- function(){

  get_ui_tab_template(
    tab="sales_nxty",qn=45,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected Sales Volume"
  )


}