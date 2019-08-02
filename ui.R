dashboardPage(
    dashboardHeader(title = "Personal finance monitoring"),
    dashboardSidebar(
        checkboxGroupInput(
            "person_selector", label = "Select Person",
            choices = c("NikiCica", "TomiMaci"), selected = c("NikiCica", "TomiMaci")
        ),
        sidebarMenu(
            menuItem("Raw Spending Data", tabName = "raw_spending_data",       icon = icon("th")),
            menuItem("Spending Summary",  tabName = "spending_summary_charts", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("raw_spending_data", fluidPage(DT::dataTableOutput("raw_spending_table"))),
            tabItem("spending_summary_charts", fluidPage(
                box(plotOutput("total_spending_plot"))
            ))
        )
    )
)
