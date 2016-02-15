
get_ui_tab_capacity_prev_qtr <- function(){

  get_ui_tab_template(
    tab="capacity_prvq",qn=3,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "capacity"
  )


}

get_ui_tab_capacity_next_qtr <- function(){

  get_ui_tab_template(
    tab="capacity_nxtq",qn=1,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected capacity"
  )


}

get_ui_tab_capacity_prev_yr <- function(){

  get_ui_tab_template(
    tab="capacity_prvy",qn=4,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "capacity"
  )


}

get_ui_tab_capacity_next_yr <- function(){

  get_ui_tab_template(
    tab="capacity_nxty",qn=2,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected capacity"
  )


}