header <- dashboardHeader(title = "Personal finance monitoring")

sidebar <- dashboardSidebar(
    checkboxGroupInput(
        "person_selector", label = "Select Person",
        choices  = c("NikiCica", "TomiMaci"),
        selected = c("NikiCica", "TomiMaci")
    )
)

body <- dashboardBody(
    tabsetPanel(type = "tabs",
        tabPanel(
            title = "Spending Summary", icon = icon("chart-line"),
            fluidRow(
                box(plotOutput("total_spending_plot") %>% withSpinner(color = "#6984D1"))
            )
        ),
        tabPanel(
            title = "Net debt", icon = icon("balance-scale"),
            fluidRow(box(
                DT::dataTableOutput("net_debt_table") %>% withSpinner(color = "#6984D1"),
                width = 8
            ))
        ),
        tabPanel(
            title = "Raw Spending Data", icon = icon("th"),
            fluidRow(box(
                DT::dataTableOutput("raw_spending_table") %>% withSpinner(color = "#6984D1"),
                width = 12
            ))
        )
    )
)


dashboardPage(header, sidebar, body)
