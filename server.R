server <- function(input, output) {
    spending_google_sheet <- loadGoogleSheet(google_sheet_name = "Add new spending (Responses)")

    output$raw_spending_table <- DT::renderDataTable(
        spending_google_sheet %>% selectRelevantColumns(),
        caption = "Raw Data from 'Add new spending (Responses)' Google Sheet",
        options = list(pageLength = 50)
    )

    getPlotInputDataReactive <- reactive({
        req(input$person_selector)
        selected_person <- input$person_selector

        if (length(selected_person) > 1) {
            dt_to_plot <- spending_google_sheet %>%
                .[, .(
                    Date,
                    Spending = `NikiCica Paid for Share` + `TomiMaci Paid for Share`
                )]
        } else {
            dt_to_plot <- spending_google_sheet %>%
                .[,
                    c("Date", paste(selected_person, "Paid for Share")),
                    with = FALSE
                ] %>%
                setnames(paste(selected_person, "Paid for Share"), "Spending")
        }

        dt_to_plot
    })

    output$total_spending_plot <- shiny::renderPlot(
        getPlotInputDataReactive() %>%
            plotTotalSpending()
    )

    output$net_debt_table <- DT::renderDataTable(
        prepareNetDebtTable(spending_google_sheet),
        caption = "Net Debt by Currency",
        options = list(pageLength = 50)
    )
}
