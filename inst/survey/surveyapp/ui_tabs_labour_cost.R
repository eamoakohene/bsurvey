
get_ui_tab_labour_cost_prev_qtr <- function(){

  get_ui_tab_template(
    tab="labour_prvq",qn=50,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "labour cost"
  )


}

get_ui_tab_labour_cost_next_qtr <- function(){

  get_ui_tab_template(
    tab="labour_nxtq",qn=44,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected labour cost"
  )


}

get_ui_tab_labour_cost_prev_yr <- function(){

  get_ui_tab_template(
    tab="labour_prvy",qn=51,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "labour cost"
  )


}

get_ui_tab_labour_cost_next_yr <- function(){

  get_ui_tab_template(
    tab="labour_nxty",qn=45,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Expected labour cost"
  )


}