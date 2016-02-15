
get_ui_tab_labour_cost_prev_yr <- function(){

  get_ui_tab_template(
    tab="labour_cost_prvy",qn=39,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "labour cost"
  )


}

get_ui_tab_labour_cost_next_yr <- function(){

  get_ui_tab_template(
    tab="labour_cost_nxty",qn=38,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "expected labour cost"
  )


}

get_ui_tab_labour_force_prev_yr <- function(){

  get_ui_tab_template(
    tab="labour_force_prvy",qn=41,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "labour force"
  )


}

get_ui_tab_labour_force_next_yr <- function(){

  get_ui_tab_template(
    tab="labour_force_nxty",qn=40,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "expected labour force"
  )


}