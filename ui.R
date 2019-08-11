header <- dashboardHeader(title = "Personal finance monitoring")

sidebar <- dashboardSidebar(
    sidebarMenu(
        uiOutput("date_interval_filter"),
        uiOutput("category_filter"),
        uiOutput("subcategory_filter"),
        uiOutput("currency_filter"),
        uiOutput("person_filter")
    )
)

body <- dashboardBody(
    tabsetPanel(type = "tabs",
        tabPanel(
            title = "Spending Summary", icon = icon("chart-line"),
            fluidRow(
                box(plotlyOutput("total_spending_plot") %>% withSpinner(color = "#6984D1"))
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
