dashboardPage(
    dashboardHeader(title = "Personal finance monitoring"),
    dashboardSidebar(),
    dashboardBody(
        fluidPage(DT::dataTableOutput("raw_spending_table"))
    )
)
