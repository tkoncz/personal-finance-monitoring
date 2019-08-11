# HEADER ----
header <- dashboardHeader(title = "Personal finance monitoring")

# SIDEBAR ----
sidebar <- dashboardSidebar(
    sidebarMenu(
        uiOutput("date_interval_filter"),
        uiOutput("category_filter"),
        uiOutput("subcategory_filter"),
        uiOutput("currency_filter"),
        uiOutput("person_filter")
    )
)

# BODY ----
body <- dashboardBody(
    tabsetPanel(type = "tabs",
        tabPanel(
            title = "Spending Summary", icon = icon("chart-line"),
            fluidRow(
                box(plotlyOutput("total_spending_over_time_plot") %>% withSpinner(color = "#6984D1")),
                box(plotlyOutput("total_spending_by_category_plot") %>% withSpinner(color = "#6984D1"))
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

# PUTTING IT TOGETHER ----
dashboardPage(header, sidebar, body)
