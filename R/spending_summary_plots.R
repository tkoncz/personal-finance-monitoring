plotTotalSpendingOverTime <- function(google_sheet) {
    cumulative_spending <- google_sheet %>%
        .[, .(
                spending = sum(Amount),
                nikicica_spending = sum(Amount[`Paid for` == "NikiCica"]),
                tomimaci_spending = sum(Amount[`Paid for` == "TomiMaci"])
            ),
            by = .(Date)
        ] %>%
        .[order(Date), `:=`(
            spending_cumsum = cumsum(spending),
            nikicica_spending_cumsum = cumsum(nikicica_spending),
            tomimaci_spending_cumsum = cumsum(tomimaci_spending)
        )]

    since_date <- cumulative_spending[, min(Date)]

    p <- ggplot(
        cumulative_spending, aes(
            x = as.character(Date), y = spending_cumsum,
            text = paste0(
                "Total Spending: ", scales::comma(spending_cumsum), "<br>",
                "By NikiCica: ", scales::comma(nikicica_spending_cumsum), "<br>",
                "By TomiMaci: ", scales::comma(tomimaci_spending_cumsum), "<br>",
                "Spending Until: ", Date, "<br>",
                "Spending Since: ", since_date, "<br>"
            )
        )
    ) +
        geom_bar(stat = "identity", fill = "lightblue") +
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
