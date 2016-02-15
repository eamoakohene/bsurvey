
get_ui_tab_imports_prev_qtr <- function(){

  get_ui_tab_template(
    tab="imports_prvq",qn=37,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Imports"
  )


}

get_ui_tab_imports_next_qtr <- function(){

  get_ui_tab_template(
    tab="imports_nxtq",qn=36,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "Imports"
  )


}

