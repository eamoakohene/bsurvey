
get_ui_tab_impact_cost_energy <- function(){

  get_ui_tab_template(
    tab="cost_energy",qn=30,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "energy cost"
  )


}

get_ui_tab_impact_cost_rates <- function(){

  get_ui_tab_template(
    tab="cost_rates",qn=31,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "exchange rates"
  )


}

get_ui_tab_impact_cost_fuel <- function(){

  get_ui_tab_template(
    tab="cost_fuel",qn=32,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "fuel"
  )


}

get_ui_tab_impact_cost_materials <- function(){

  get_ui_tab_template(
    tab="cost_materials",qn=33,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "raw materials"
  )


}

get_ui_tab_impact_cost_taxes <- function(){

  get_ui_tab_template(
    tab="cost_taxes",qn=34,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "taxes"
  )


}

get_ui_tab_impact_cost_wages <- function(){

  get_ui_tab_template(
    tab="cost_wages",qn=35,
    yr_range= slider_range_yr,yr_select=slider_select_yr,
    qtr_range=slider_range_qtr, qtr_select= slider_select_qtr,
    is_pc = TRUE, trend_name = "wages & salaries"
  )


}