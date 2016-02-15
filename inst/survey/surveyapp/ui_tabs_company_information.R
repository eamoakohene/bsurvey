
get_ui_tab_company_labour <- function(){

  get_ui_tab_template(
    tab="company_labour",qn=19,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "company labour"
  )


}

get_ui_tab_company_sector <- function(){

  get_ui_tab_template(
    tab="company_sector",qn=20,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "company sector"
  )


}

get_ui_tab_company_turnover <- function(){

  get_ui_tab_template(
    tab="company_turnover",qn=21,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "company_turnover"
  )


}

