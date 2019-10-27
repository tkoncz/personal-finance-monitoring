rawSpendingUI <- function() {
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
                p("placeholder")
            ),
            mainPanel(width = 9,
                downloadLink("download_raw_spending", "Export to .csv file"),
                fluidRow(box(
                    withSpinner(DT::dataTableOutput("raw_spending_table"), color = "#6984D1"),
                    width = 12
                ))
            )
        )
    )
}
