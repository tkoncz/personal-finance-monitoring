source("global.R")

shinyUI(navbarPage("Personal finance monitoring",
    tabPanel(
        "Spending Summary",
        spendingSummaryUI(),
        icon = icon("chart-line")
    ),
    tabPanel(
        "Net debt",
        netDebtUI(),
        icon = icon("balance-scale")
    ),
    tabPanel(
        "Raw Spending Data",
        rawSpendingUI(),
        icon = icon("th")
    )
))
