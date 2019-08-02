server <- function(input, output) {
    output$raw_spending_table <- DT::renderDataTable(
        loadGoogleSheet(google_sheet_name = "Add new spending (Responses)"),
        caption = "Data from 'Add new spending (Responses)' Google Sheet"
    )
}
