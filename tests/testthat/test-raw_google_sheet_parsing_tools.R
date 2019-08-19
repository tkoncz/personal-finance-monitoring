context("Adjust records corresponding multiple people")

test_that("Simple case when everyone pays for themselves works", {
    spending_table <- data.table(
        Date       = rep("2019-01-01", 3),
        Amount     = rep(10,           3),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica")
    )

    expect_equal(
        adjustForPaidForByBoth(spending_table),
        spending_table
    )
})

test_that("If someone paid for two people, it is parsed correctly", {
    spending_table <- data.table(
        Date       = rep("2019-01-01", 3),
        Amount     = rep(10,           3),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica, TomiMaci"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica")
    )

    expected_adjusted_spending_table <- data.table(
        Date       = rep("2019-01-01", 4),
        Amount     = c(10,         10,         5,          5),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica", "TomiMaci"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica", "NikiCica")
    )

    expect_equal(
        adjustForPaidForByBoth(spending_table),
        expected_adjusted_spending_table
    )
})

test_that("If something is paid by two people, it is parsed correctly", {
    spending_table <- data.table(
        Date       = rep("2019-01-01", 3),
        Amount     = rep(10,           3),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica, TomiMaci")
    )

    expected_adjusted_spending_table <- data.table(
        Date       = rep("2019-01-01", 4),
        Amount     = c(10,         10,         5,          5),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica", "NikiCica"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica", "TomiMaci")
    )

    expect_equal(
        adjustForPaidForByBoth(spending_table),
        expected_adjusted_spending_table
    )
})

test_that("If something is paid by and for two people, it is parsed correctly", {
    spending_table <- data.table(
        Date       = rep("2019-01-01", 3),
        Amount     = rep(10,           3),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica, TomiMaci"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica, TomiMaci")
    )

    expected_adjusted_spending_table <- data.table(
        Date       = rep("2019-01-01", 6),
        Amount     = c(10,         10,         2.5,        2.5,        2.5,        2.5),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica", "TomiMaci", "NikiCica", "TomiMaci"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica", "NikiCica", "TomiMaci", "TomiMaci")
    )

    expect_equal(
        adjustForPaidForByBoth(spending_table),
        expected_adjusted_spending_table
    )
})
