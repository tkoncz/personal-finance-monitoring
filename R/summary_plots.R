plotTotalSpending <- function(google_sheet) {
    cumulative_spending <- google_sheet %>%
        .[, .(Amount = sum(Amount)), by = .(Date)] %>%
        .[order(Date), amount_cumsum := cumsum(Amount)] %>%
        .[]

    ggplot(cumulative_spending, aes(x = Date, y = amount_cumsum)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        scale_y_continuous(
            breaks = scales::pretty_breaks(),
            labels = scales::comma
        ) +
        labs(
            title = "Cumulative Spending Over Time",
            x = "", y = ""
        ) +
        theme_minimal()
}
