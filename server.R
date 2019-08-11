server <- function(input, output) {
    # Input Data ----
    spending_google_sheet <- loadGoogleSheet(
        google_sheet_name = "Add new spending (Responses)"
    )

    spending_paid_for_both_adjusted <- adjustForPaidForByBoth(spending_google_sheet)

    filterSpendingSheetReactive <- reactive({
        req(input$person)
        req(input$date_interval)
        req(input$category)
        req(input$subcategory)

        spending_paid_for_both_adjusted %>%
            .[`Paid for` %in% input$person] %>%
            .[Date %between% input$date_interval] %>%
            .[Category %in% input$category] %>%
            .[Subcategory %in% input$subcategory]
    })

    # UI - Filters ----
    output$person_filter <- renderUI({
        persons <- c("NikiCica", "TomiMaci")

        pickerInput(
            inputId = "person", label = "Select Person:",
            choices = persons, selected = persons,
            options = list(`actions-box` = TRUE, size = 10),
            multiple = TRUE
        )
    })

    output$currency_filter <- renderUI({
        currencies <- spending_google_sheet[, unique(`Currency`)]

        pickerInput(
            "currency", "Select Currency:",
            choices = currencies, selected = "HUF"
        )
    })

    output$date_interval_filter <- renderUI({
        min_date <- spending_google_sheet[, min(Date)]
        max_date <- spending_google_sheet[, max(Date)]

        sliderInput(
            "date_interval", "Select Date Interval:",
            min = min_date, max = max_date, value = c(min_date, max_date),
            step = 1
        )
    })

    output$category_filter <- renderUI({
        categories <- spending_google_sheet[, unique(`Category`)]

        pickerInput(
            inputId = "category", label = "Select Category:",
            choices = categories, selected = categories,
            options = list(
                `actions-box` = TRUE, size = 10,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
        )
    })

    output$subcategory_filter <- renderUI({
        subcategory_map <- spending_google_sheet[, .(`Category`, `Subcategory`)] %>%
            unique()

        subcategories <- subcategory_map %>%
            split(by = "Category") %>%
            map(~.x[["Subcategory"]])

        subcategories_preselected <- spending_google_sheet[, unique(`Subcategory`)]
        # note: this a work-around, as somehow selected = subcategories doesn't work
        # (probably have to do with cases when there is 1 subcategory / category)

        pickerInput(
            inputId = "subcategory", label = "Select Subcategory:",
            choices = subcategories,
            selected = subcategories_preselected,
            options = list(
                `actions-box` = TRUE, size = 10,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
        )
    })

    # UI - Plots & Tables ----
    output$total_spending_plot <- renderPlotly(
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
