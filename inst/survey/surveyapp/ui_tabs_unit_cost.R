
get_ui_tab_unit_cost_prev_qtr <- function(){

  get_ui_tab_template(
    tab="ucost_prvq",qn=53,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "unit cost"
  )


}

get_ui_tab_unit_cost_next_qtr <- function(){

  get_ui_tab_template(
    tab="ucost_nxtq",qn=54,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected unit cost"
  )


}

get_ui_tab_unit_cost_prev_yr <- function(){

  get_ui_tab_template(
    tab="ucost_prvy",qn=52,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "unit cost"
  )


}

get_ui_tab_unit_cost_next_yr <- function(){

  get_ui_tab_template(
    tab="ucost_nxty",qn=55,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected unit cost"
  )


}