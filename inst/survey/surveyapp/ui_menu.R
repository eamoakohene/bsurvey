get_ui_menu <- function(){

  sn <- SN("menu")

  sidebarMenu(

    menuItem("Dashboard", tabName = sn("dashboard"), icon = icon("dashboard"))
   ,menuItem("Optimism",  tabName = sn("optimism"), icon = icon("smile-o"))
   ,menuItem("Sales Volumes", tabName= sn("sales"), icon= icon("bar-chart-o"),
             menuSubItem("Past Quarter",tabName = sn("sales_prvq") ),
             menuSubItem("Next Quarter",tabName = sn("sales_nxtq") ),
             menuSubItem("Past Year",tabName = sn("sales_prvy") ),
             menuSubItem("Next Year",tabName = sn("sales_nxty") )

    )
   ,menuItem("Margins on Sales", tabName=sn("margins"), icon= icon("area-chart"),
             menuSubItem("UK Sales - Past Quarter",tabName = sn("margins_uk_prvq")),
             menuSubItem("UK Sales - Past Year",tabName = sn("margins_uk_prvy")),
             menuSubItem("Exports - Past Quarter",tabName = sn("margins_exp_prvq")),
             menuSubItem("Exports - Past Year",tabName = sn("margins_exp_prvy"))

   )
   ,menuItem("Exports", tabName=sn("exports"), icon= icon("pie-chart"),
             menuSubItem("Past Quarter",tabName = sn("exports_prvq")),
             menuSubItem("Next Quarter",tabName = sn("exports_nxtq")),
             menuSubItem("Past Year",tabName = sn("exports_prvy")),
             menuSubItem("Next Year",tabName = sn("exports_nxty")),
             menuSubItem("Key Market",tabName = sn("exports_keym")),
             menuSubItem("Key Factor",tabName = sn("exports_keyf")),
             menuSubItem("Proportion of Sales",tabName = sn("exports_prop"))


   )
   ,menuItem("Imports", tabName=sn("imports"), icon= icon("bar-chart-o"),
             menuSubItem("Past Quarter",tabName = sn("imports_prvq")),
             menuSubItem("Next Quarter",tabName = sn("imports_nxtq"))
   )
   ,menuItem("Output Constraint", tabName = sn("constraint"), icon = icon("street-view"))
   ,menuItem("Unit Cost", tabName=sn("ucost"), icon= icon("usd"),
             menuSubItem("Past Quarter",tabName = sn("ucost_prvq")),
             menuSubItem("Next Quarter",tabName = sn("ucost_nxtq")),
             menuSubItem("Past Year",tabName = sn("ucost_prvy")),
             menuSubItem("Next Year",tabName = sn("ucost_nxty"))
   )

   ,menuItem("Impact on Unit Cost", tabName=sn("cost"), icon= icon("sort-amount-desc"),
             menuSubItem("Energy",tabName = sn("cost_energy")),
             menuSubItem("Exchange rates",tabName = sn("cost_rates")),
             menuSubItem("Fuel Costs",tabName = sn("cost_fuel")),
             menuSubItem("Raw Materials",tabName = sn("cost_materials")),
             menuSubItem("Taxes",tabName = sn("cost_taxes")),
             menuSubItem("Wages & Salaries",tabName = sn("cost_wages"))
   )
   ,menuItem("Labour ", tabName=sn("labour"), icon= icon("users"),
             menuSubItem("Cost past year",tabName = sn("labour_cost_prvy")),
             menuSubItem("Cost next year",tabName = sn("labour_cost_nxty")),
             menuSubItem("Force past year",tabName = sn("labour_force_prvy")),
             menuSubItem("Force next year",tabName = sn("labour_force_nxty"))
   )
   ,menuItem("Capacity Utlisation", tabName=sn("capacity"), icon= icon("battery-half"),
             menuSubItem("Past Quarter",tabName = sn("capacity_prvq")),
             menuSubItem("Next Quarter",tabName = sn("capacity_nxtq")),
             menuSubItem("Past Year",tabName = sn("capacity_prvy")),
             menuSubItem("Next Year",tabName = sn("capacity_nxty"))
   )
   ,menuItem("Capital Investment", tabName=sn("capex"), icon= icon("industry"),
             menuItem("Past Year",tabName = sn("capex_prvy"),
                      menuSubItem("Customer Research",tabName = sn("capex_customer_prvy")),
                      menuSubItem("Developing Export Markets",tabName = sn("capex_export_prvy")),
                      menuSubItem("E-Busines",tabName = sn("capex_ebusiness_prvy")),
                      menuSubItem("Product Improvement",tabName = sn("capex_product_prvy")),
                      menuSubItem("Plant & Equipment",tabName = sn("capex_plant_prvy")),
                      menuSubItem("R&D",tabName = sn("capex_rd_prvy")),
                      menuSubItem("Structures",tabName = sn("capex_structures_prvy"))
             ),
             menuItem("Next Year",tabName = sn("capex_nxty"),
                      menuSubItem("Customer",tabName = sn("capex_customer_nxty")),
                      menuSubItem("Developing Export",tabName = sn("capex_export_nxty")),
                      menuSubItem("E-Busines",tabName = sn("capex_ebusiness_nxty")),
                      menuSubItem("Product Improvement",tabName = sn("capex_product_nxty")),
                      menuSubItem("Plant & Equipment",tabName = sn("capex_plant_nxty")),
                      menuSubItem("R&D",tabName = sn("capex_rd_nxty")),
                      menuSubItem("Structures",tabName = sn("capex_structures_nxty"))
             )

   )
   ,menuItem("Company Information", tabName=sn("company"), icon= icon("building"),
               menuSubItem("Labour Force",tabName = sn("company_labour")),
               menuSubItem("Most Important Sector",tabName = sn("company_sector")),
               menuSubItem("Turnover",tabName = sn("company_turnover"))
   )

   ,shinyjs::hidden(
             div(id="adm_menu_update",
                  menuItem("Admin",tabName=sn("admin"),icon=icon("user"))
             )
    )

  )


}