prepareNetDebtTable <- function(google_sheet) {
    google_sheet[,
        .(
            `NikiCica Net Balance`    = sum(`NikiCica Net Balance`),
            `TomiMaci Net Balance`    = sum(`TomiMaci Net Balance`),
            `NikiCica Paid by Share`  = sum(`NikiCica Paid by Share`),
            `TomiMaci Paid by Share`  = sum(`TomiMaci Paid by Share`),
            `NikiCica Paid for Share` = sum(`NikiCica Paid for Share`),
            `TomiMaci Paid for Share` = sum(`TomiMaci Paid for Share`)
        ),
        by = .(Currency)
    ]
}
