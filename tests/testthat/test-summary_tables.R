# ----
context("Net debt calculation")

test_that("Net debt by currency is calculated correctly", {
    spending_table <- data.table(
        Currency   = c("HUF",      "HUF",      "EUR"),
        Amount     = c(10,         10,         10),
        `Paid for` = c("TomiMaci", "TomiMaci", "NikiCica"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica")
    )

    expected_net_debt_table <- data.table(
        Currency                  = c("HUF", "EUR"),
        `NikiCica Spending Share` = c(0,     10),
        `TomiMaci Spending Share` = c(20,    0),
        `NikiCica Paid by Share`  = c(10,    10),
        `TomiMaci Paid by Share`  = c(10,    0),
        `NikiCica Net Balance`    = c(10,    0),
        `TomiMaci Net Balance`    = c(-10,   0)
    )

    expect_equal(
        prepareNetDebtTable(spending_table),
        expected_net_debt_table
    )
})
