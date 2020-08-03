spendingSummaryUI <- function() {
    fluidPage(
        tags$head(
            tags$style(
                HTML("
                     .thresholdsetter > .shiny-input-container {
                     width : 100%;
                     padding-left: 60px;
                     }
                     ")
            )
        ),
        sidebarLayout(
            sidebarPanel(width = 3,
                uiOutput("date_interval_filter"),
                selectInput(
                    "date_aggregation_level", "Select Date Aggregation:",
                    choices = c("month", "week", "day"), selected = "month"
                ),
                uiOutput("category_filter"),
                uiOutput("subcategory_filter"),
                uiOutput("currency_filter"),
                uiOutput("person_filter")
            ),
            mainPanel(width = 9,
                fluidRow(
                    box(
                        plotlyOutput("total_spending_over_time_plot") %>% withSpinner(color = "#6984D1"),
                        width = 12
                    )
                ),
                fluidRow(
                    box(plotlyOutput("total_spending_by_category_plot") %>% withSpinner(color = "#6984D1")),
                    box(plotlyOutput("total_spending_by_person_plot") %>% withSpinner(color = "#6984D1"))
                )
            )
        )
    )
}
