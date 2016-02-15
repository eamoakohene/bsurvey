
get_ui_tab_constraint <- function(){

  get_ui_tab_template(
    tab="constraint",qn=29,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select=slider_select_qtr,
    is_pc = TRUE, trend_name = "Constraint"
  )


}



