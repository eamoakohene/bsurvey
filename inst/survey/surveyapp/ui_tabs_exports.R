
get_ui_tab_exports_prev_qtr <- function(){

  get_ui_tab_template(
    tab="exports_prvq",qn=24,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Exports"
  )


}

get_ui_tab_exports_prev_yr <- function(){

  get_ui_tab_template(
    tab="exports_prvy",qn=25,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "Exports"
  )


}

get_ui_tab_exports_next_qtr <- function(){

  get_ui_tab_template(
    tab="exports_nxtq",qn=22,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "Exports"
  )


}

get_ui_tab_exports_next_yr <- function(){

  get_ui_tab_template(
    tab="exports_nxty",qn=23,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "Exports"
  )


}

get_ui_tab_exports_key_market <- function(){

  get_ui_tab_template(
    tab="exports_keym",qn=26,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "exports key market"
  )


}

get_ui_tab_exports_key_factor <- function(){

  get_ui_tab_template(
    tab="exports_keyf",qn=27,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "exports key factor"
  )


}

get_ui_tab_exports_proportion <- function(){

  get_ui_tab_template(
    tab="exports_prop",qn=28,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select = slider_select_qtr,
    is_pc = TRUE, trend_name = "exports key factor"
  )


}