server <- function(input, output) {
    loadNewSpendingGoogleSheetReactive <- reactive({
        loadGoogleSheet(google_sheet_name = "Add new spending (Responses)")
    })

    output$raw_spending_table <- DT::renderDataTable(
        loadNewSpendingGoogleSheetReactive() %>% selectRelevantColumns(),
        caption = "Data from 'Add new spending (Responses)' Google Sheet",
        options = list(pageLength = 50)
    )

    getPlotInputDataReactive <- reactive({
        req(input$person_selector)
        selected_person <- input$person_selector

        if (length(selected_person) > 1) {
            dt_to_plot <- loadNewSpendingGoogleSheetReactive() %>%
                .[, .(
                    Date,
                    Spending = `NikiCica Paid for Share` + `TomiMaci Paid for Share`
                )]
        } else {
            dt_to_plot <- loadNewSpendingGoogleSheetReactive() %>%
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
}
