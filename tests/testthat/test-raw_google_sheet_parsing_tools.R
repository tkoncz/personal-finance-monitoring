# ----
context("Unique ID is created for all records")

test_that("Different records are given different IDs", ~{
    test_table <- data.table(
        Timestamp   = c("7/29/2019 13:07:10", "7/29/2019 14:00:01"),
        Date        = c("7/29/2019",          "7/29/2019"),
        Category    = c("foo",                "qux"),
        Subcategory = c("bar",                "baz"),
        `Paid for`  = c("baz",                "bar"),
        `Paid by`   = c("qux",                "foo")
    )

    addSpendingUniqueID_(test_table)

    expect_true(!is.null(test_table[["spending_unique_id"]]))
    expect_true(length(test_table[["spending_unique_id"]]) == 2)
    expect_true(
        test_table[1, spending_unique_id] != test_table[2, spending_unique_id]
    )
})

test_that("Same record is given the same ID if run twice", ~{
    test_table <- data.table(
        Timestamp   = c("7/29/2019 13:07:10"),
        Date        = c("7/29/2019"),
        Category    = c("foo"),
        Subcategory = c("bar"),
        `Paid for`  = c("baz"),
        `Paid by`   = c("qux")
    )

    addSpendingUniqueID_(test_table)
    spending_unique_id_1 <- test_table[, spending_unique_id]

    test_table[, spending_unique_id := NULL]

    addSpendingUniqueID_(test_table)
    spending_unique_id_2 <- test_table[, spending_unique_id]

    expect_true(spending_unique_id_1 == spending_unique_id_2)
})

# ----
context("Fixing field formats")

test_that("Date & Timestamp columns are converted to correct format", {
    test_table <- data.table(
        Timestamp = "7/29/2019 13:07:10",
        Date = "7/29/2019"
    )

    fixDateFormat_(test_table)

    expect_equal(
        lapply(test_table, class) %>% names(),
        c("Timestamp", "Date")
    )

    expect_equal(
        test_table,
        data.table(
            Timestamp = as.POSIXct("2019-07-29 13:07:10", tz = "CET"),
            Date      = as.Date("2019-07-29")
        )
    )
})

test_that("Subcategory column is created with the correct contents", {
    test_table <- data.table(
        Category = c("foo",        "foo",          "bar"),
        foo      = c("baz",        "baz",          NA_character_),
        bar      = c(NA_character_, NA_character_, "foobar")
    )

    addSubCategory_(test_table)

    expect_true(!is.null(test_table[["Subcategory"]]))

    expect_equal(
        test_table[, Subcategory],
        c("baz", "baz", "foobar")
    )

})

test_that("Currency is correctly updated for 'Other'", {
    test_table <- data.table(
        Currency                  = c("HUF",         "HUF",         "EUR",         "Other"),
        `Specify other currency:` = c(NA_character_, NA_character_, NA_character_, "JPY")
    )

    fixOtherCurrency_(test_table)

    expect_equal(
        test_table[, Currency],
        c("HUF", "HUF", "EUR", "JPY")
    )
})

# ----
context("Share amount & Net balance calculation")

test_that("Share amounts are calculated correctly", {
    spending_table <- data.table(
        Amount     = rep(10,           3),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica, TomiMaci"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica")
    )

    addShareAmounts_(spending_table)

    expected_adjusted_spending_table <- data.table(
        Amount                    = c(10,         10,         10),
        `Paid for`                = c("TomiMaci", "NikiCica", "NikiCica, TomiMaci"),
        `Paid by`                 = c("TomiMaci", "NikiCica", "NikiCica"),
        `TomiMaci Paid for Share` = c(10,         0,           5),
        `TomiMaci Paid by Share`  = c(10,         0,           0),
        `NikiCica Paid for Share` = c(0,          10,          5),
        `NikiCica Paid by Share`  = c(0,          10,          10)
    )

    expect_equal(spending_table, expected_adjusted_spending_table)
})

test_that("Net balances are calculated correctly", {
    spending_table <- data.table(
        `TomiMaci Paid for Share` = c(10, 0,  5),
        `TomiMaci Paid by Share`  = c(10, 0,  0),
        `NikiCica Paid for Share` = c(0,  10, 5),
        `NikiCica Paid by Share`  = c(0,  10, 10)
    )

    addNetBalances_(spending_table)

    expected_adjusted_spending_table <- data.table(
        `TomiMaci Paid for Share` = c(10, 0,  5),
        `TomiMaci Paid by Share`  = c(10, 0,  0),
        `NikiCica Paid for Share` = c(0,  10, 5),
        `NikiCica Paid by Share`  = c(0,  10, 10),
        `NikiCica Net Balance`    = c(0,  0,  5),
        `TomiMaci Net Balance`    = c(0,  0,  -5)
    )

    expect_equal(spending_table, expected_adjusted_spending_table)
})

# ----
context("Adjust records corresponding multiple people")

test_that("Simple case when everyone pays for themselves works", {
    spending_table <- data.table(
        Date       = rep("2019-01-01", 3),
        Amount     = rep(10,           3),
        `Paid for` = c("TomiMaci", "NikiCica", "NikiCica"),
        `Paid by`  = c("TomiMaci", "NikiCica", "NikiCica")
    )

    expect_equal(
        adjustForPaidForOrPaidByMultiplePeople(spending_table),
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
        adjustForPaidForOrPaidByMultiplePeople(spending_table),
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
        adjustForPaidForOrPaidByMultiplePeople(spending_table),
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
        adjustForPaidForOrPaidByMultiplePeople(spending_table),
        expected_adjusted_spending_table
    )
})
