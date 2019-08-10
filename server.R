server <- function(input, output) {
    # Input Data ----
    spending_google_sheet <- loadGoogleSheet(
        google_sheet_name = "Add new spending (Responses)"
    )

    spending_paid_for_both_adjusted <- adjustForPaidForByBoth(spending_google_sheet)

    filterSpendingSheetReactive <- reactive({
        req(input$person_selector)
        selected_persons <- input$person_selector

        spending_paid_for_both_adjusted %>%
            .[`Paid for` %in% selected_persons]
    })

    # UI - Plots & Tables ----
    output$total_spending_plot <- shiny::renderPlot(
        plotTotalSpending(filterSpendingSheetReactive())
    )

    output$net_debt_table <- DT::renderDataTable(
        prepareNetDebtTable(spending_paid_for_both_adjusted),
        caption = "Net Debt by Currency",
        options = list(pageLength = 25)
    )

    output$raw_spending_table <- DT::renderDataTable(
        prepareTableForRawData(spending_google_sheet),
        caption = "Raw Data from 'Add new spending (Responses)' Google Sheet",
        options = list(pageLength = 25)
    )
}
