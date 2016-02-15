get_ui_tab_dashboard <- function(){
    tabItem(
      tabName="dashboard-menu",
            fluidPage(

              fluidRow(
                infoBox("Responses",    h1(85),  icon=shiny::icon("users"),color = "blue"),
                infoBox("Average Labour Force per firm",h1(200),icon=shiny::icon("male") ,color = "blue"),
                infoBox("Average Turnover per firm",h1("71 GBP million"),icon=shiny::icon("gbp") ,color = "blue")
              ),
              h1("Headlines:"),
              tags$ul(
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",42) )$head)),
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",50) )$head)),
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",44) )$head)),
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",31) )$head)),
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",22) )$head)),
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",35) )$head)),
                tags$li(h2(lite_run_sql( sprintf("select head from bts_q where qn=%i",4) )$head))
              )

            )#fluidpage
    )#tabitem
}