server <- function(input, output) {
    loadNewSpendingGoogleSheetReactive <- reactive({
        loadGoogleSheet(google_sheet_name = "Add new spending (Responses)")
    })

    output$raw_spending_table <- DT::renderDataTable(
        loadNewSpendingGoogleSheetReactive(),
        caption = "Data from 'Add new spending (Responses)' Google Sheet",
        options = list(pageLength = 50)
    )

    getPlotInputDataReactive <- reactive({
        req(input$person_selector)
        selected_person <- input$person_selector

        if (length(selected_person) > 1) {
            loadNewSpendingGoogleSheetReactive()
        } else {
            loadNewSpendingGoogleSheetReactive() %>%
                .[`Paid by` %like% selected_person]
        }
    })

    output$total_spending_plot <- shiny::renderPlot(
        getPlotInputDataReactive() %>% plotTotalSpending()
    )
}
