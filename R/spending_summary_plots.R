plotTotalSpendingOverTime <- function(google_sheet, date_range) {
    cumulative_spending <- prepareDataForTotalSpendingOverTimePlot(
        google_sheet, date_range
    )

    p <- ggplot(
        cumulative_spending,
        aes(
            x = as.character(Date), y = spending_cumsum, ymin = 0, ymax = spending_cumsum, group = 1,
            text = paste0(
                "Total Spending: ", scales::comma(spending_cumsum), "<br>",
                "By NikiCica: ", scales::comma(nikicica_spending_cumsum), "<br>",
                "By TomiMaci: ", scales::comma(tomimaci_spending_cumsum), "<br>",
                "Spending on date: ", scales::comma(spending), "<br>",
                "Spending Until: ", Date, "<br>",
                "Spending Since: ", date_range[1], "<br>"
            )
        )
    ) +
        geom_line(color = getColorScale(1)) +
        geom_ribbon(fill = getColorScale(3)[3], alpha = 0.5) +
        geom_point(color = getColorScale(1), size = 2) +
        scale_y_continuous(
            breaks = scales::pretty_breaks(), labels = scales::comma
        ) +
        labs(
            title = "Cumulative Spending Over Time",
            x = "", y = ""
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = c("text"))
}

prepareDataForTotalSpendingOverTimePlot <- function(google_sheet, date_range) {
    cumulative_spending <- google_sheet %>%
        .[, .(
                spending = sum(Amount),
                nikicica_spending = sum(Amount[`Paid for` == "NikiCica"]),
                tomimaci_spending = sum(Amount[`Paid for` == "TomiMaci"])
            ),
            by = .(Date)
        ] %>%
        .[order(Date)] %>%
        .[, `:=`(
            spending_cumsum = cumsum(spending),
            nikicica_spending_cumsum = cumsum(nikicica_spending),
            tomimaci_spending_cumsum = cumsum(tomimaci_spending)
        )]

    all_dates_in_interval <- data.table(
        Date = seq.Date(date_range[1], date_range[2], by = "day")
    )

    cumulative_spending <- merge(
        all_dates_in_interval, cumulative_spending,
        by = "Date", all.x = TRUE
    ) %>%
        .[order(Date)] %>%
        .[, `:=`(
            spending_cumsum          = zoo::na.locf(spending_cumsum, na.rm = FALSE),
            tomimaci_spending_cumsum = zoo::na.locf(tomimaci_spending_cumsum, na.rm = FALSE),
            nikicica_spending_cumsum = zoo::na.locf(nikicica_spending_cumsum, na.rm = FALSE)
        )]
}

plotTotalSpendingByCategory <- function(google_sheet) {
    spending_by_category <- prepareDataForTotalSpendingByCategoryPlot(google_sheet)

    p <- ggplot(
        spending_by_category,
        aes(
            x = Category, y = spending, fill = Category,
            text = paste0(
                "Category: ", Category, "<br>",
                "Total Spending: ", scales::comma(spending), "<br>",
                "By NikiCica: ", scales::comma(nikicica_spending), "<br>",
                "By TomiMaci: ", scales::comma(tomimaci_spending)
            )
        )
    ) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = rev(unname(getColorScale(6)))) +
        scale_y_continuous(
            breaks = scales::pretty_breaks(), labels = scales::comma
        ) +
        labs(
            title = "Spending By Category",
            x = "", y = ""
        ) +
        coord_flip() +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none"
        )

    ggplotly(p, tooltip = c("text"))
}

prepareDataForTotalSpendingByCategoryPlot <- function(google_sheet) {
    spending_by_category <- google_sheet %>%
        .[, .(
                spending = sum(Amount),
                nikicica_spending = sum(Amount[`Paid for` == "NikiCica"]),
                tomimaci_spending = sum(Amount[`Paid for` == "TomiMaci"])
            ),
            by = .(Category)
        ] %>%
        .[, category_rank := frank(-spending)] %>%
        .[, Category := ifelse(category_rank < 6, Category, "Other Categories")] %>%
        .[, .(
                spending = sum(spending),
                nikicica_spending = sum(nikicica_spending),
                tomimaci_spending = sum(tomimaci_spending)
            ),
            by = .(Category)
        ]

    categories_in_order <- spending_by_category[Category != "Other Categories"][order(spending), Category]
    categories_in_order <- c("Other Categories", categories_in_order)
    spending_by_category[, Category := factor(Category, levels = categories_in_order)]

    spending_by_category
}

plotTotalSpendingByPerson <- function(google_sheet) {
    spending_by_person <- google_sheet %>%
        .[, .(spending = sum(Amount)), by = .(person = `Paid for`)]

    p <- ggplot(
        spending_by_person,
        aes(
            x = person, y = spending, fill = person,
            text = paste0(
                "Person: ", person, "<br>",
                "Total Spending: ", scales::comma(spending)
            )
        )
    ) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = rev(unname(getColorScale(2)))) +
        scale_y_continuous(
            breaks = scales::pretty_breaks(), labels = scales::comma
        ) +
        labs(
            title = "Spending By Person",
            x = "", y = ""
        ) +
        coord_flip() +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none"
        )

    ggplotly(p, tooltip = c("text"))
}
