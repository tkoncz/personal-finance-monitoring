prepareNetDebtTable <- function(google_sheet) {
    google_sheet[,
        .(
            `NikiCica Spending Share`  = sum(Amount[`Paid for` == "NikiCica"]),
            `TomiMaci Spending Share`  = sum(Amount[`Paid for` == "TomiMaci"]),
            `NikiCica Paid by Share`   = sum(Amount[`Paid by`  == "NikiCica"]),
            `TomiMaci Paid by Share`   = sum(Amount[`Paid by`  == "TomiMaci"])
        ),
        by = .(Currency)
    ] %>%
        .[, `:=`(
            `NikiCica Net Balance` = sum(`NikiCica Paid by Share` - `NikiCica Spending Share`),
            `TomiMaci Net Balance` = sum(`TomiMaci Paid by Share` - `TomiMaci Spending Share`)
        )]
}
