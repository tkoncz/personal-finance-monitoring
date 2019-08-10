dashboardPage(
    dashboardHeader(title = "Personal finance monitoring"),
    dashboardSidebar(
        checkboxGroupInput(
            "person_selector", label = "Select Person",
            choices  = c("NikiCica", "TomiMaci"),
            selected = c("NikiCica", "TomiMaci")
        ),
        sidebarMenu(
            menuItem(
                "Raw Spending Data",
                tabName = "raw_spending_data",
                icon = icon("th")
            ),
            menuItem(
                "Spending Summary",
                tabName = "spending_summary_charts",
                icon = icon("chart-line")
            ),
            menuItem(
                "Net debt",
                tabName = "net_debt",
                icon = icon("balance-scale")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                "raw_spending_data",
                fluidRow(
                    box(DT::dataTableOutput("raw_spending_table") %>% withSpinner(color = "#6984D1"), width = 12)
                )
            ),
            tabItem(
                "spending_summary_charts",
                fluidRow(
                    box(plotOutput("total_spending_plot") %>% withSpinner(color = "#6984D1"))
                )
            ),
            tabItem(
                "net_debt",
                fluidRow(
                    box(DT::dataTableOutput("net_debt_table") %>% withSpinner(color = "#6984D1"), width = 8)
                )
            )
        )
    )
)
