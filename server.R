server <- function(input, output) {
    # Input Data ----
    spending_google_sheet <- loadGoogleSheet(
        google_sheet_name = "Add new spending (Responses)"
    )

    spending_paid_for_both_adjusted <- adjustForPaidForByBoth(spending_google_sheet)

    filterSpendingSheetReactive <- reactive({
        req(input$person_selector)
        req(input$date_interval)

        spending_paid_for_both_adjusted %>%
            .[`Paid for` %in% input$person_selector] %>%
            .[Date %between% input$date_interval]
    })

    # UI - Filters ----
    output$date_interval_filter <- renderUI({
        min_date <- spending_google_sheet[, min(Date)]
        max_date <- spending_google_sheet[, max(Date)]

        sliderInput(
            "date_interval", "Select Date Interval:",
            min = min_date, max = max_date, value = c(min_date, max_date),
            step = 1
        )
    })

    # UI - Plots & Tables ----
    output$total_spending_plot <- shiny::renderPlot(
        plotTotalSpendingOverTime(filterSpendingSheetReactive())
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
