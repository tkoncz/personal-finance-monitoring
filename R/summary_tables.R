prepareNetDebtTable <- function(google_sheet, by = "Currency") {
    by_cols <- by
    google_sheet[,
        .(
            `NikiCica Spending Share`  = sum(Amount[`Paid for` == "NikiCica"]),
            `TomiMaci Spending Share`  = sum(Amount[`Paid for` == "TomiMaci"]),
            `NikiCica Paid by Share`   = sum(Amount[`Paid by`  == "NikiCica"]),
            `TomiMaci Paid by Share`   = sum(Amount[`Paid by`  == "TomiMaci"])
        ),
        by = by_cols
    ] %>%
        .[, `:=`(
            `NikiCica Net Balance` = `NikiCica Paid by Share` - `NikiCica Spending Share`,
            `TomiMaci Net Balance` = `TomiMaci Paid by Share` - `TomiMaci Spending Share`
        )]
}
