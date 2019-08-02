loadGoogleSheet <- function(google_sheet_name) {
    loadRawGoogleSheet(google_sheet_name) %>%
        fixDateFormat_() %>%
        addSubCategory_() %>%
        fixOtherCurrency_() %>%
        addShareAmounts_() %>%
        addNetBalances_()
}

loadRawGoogleSheet <- function(google_sheet_name) {
    google_sheet_name %>%
        googlesheets::gs_title() %>%
        googlesheets::gs_read() %>%
        data.table::as.data.table()
}

fixDateFormat_ <- function(raw_google_sheet) {
    raw_google_sheet %>%
        .[, `:=`(
            Timestamp = as.POSIXct(Timestamp, tz = "CET", format = "%m/%d/%Y %H:%M:%S"),
            Date      = as.Date(Date, format = "%m/%d/%Y")
        )]
}

addSubCategory_ <- function(google_sheet) {
    categories <- unique(google_sheet[["Category"]])
    purrr::walk(categories, ~{
        google_sheet[Category == .x, Subcategory := get(.x)]
    })

    google_sheet
}

fixOtherCurrency_ <- function(google_sheet) {
    google_sheet[
        Currency == "Other" & !is.na(`Specify other currency:`),
        Currency := `Specify other currency:`
    ]
}

addShareAmounts_ <- function(google_sheet) {
    purrr::walk(list("TomiMaci", "NikiCica"), ~{
        person <- .x
        purrr::walk(list("Paid for", "Paid by"), ~{
            which_share <- .x

            addShareAmount_(google_sheet, person = person, which_share = which_share)
        })
    })

    google_sheet
}

addShareAmount_ <- function(google_sheet, person, which_share) {
    other_person <- setdiff(c("TomiMaci", "NikiCica"), person)
    column_to_add <- paste(person, which_share, "Share")
    google_sheet[, (column_to_add) := dplyr::case_when(
            grepl(person, get(which_share)) & grepl(other_person, get(which_share)) ~ Amount / 2,
            grepl(person, get(which_share))                                         ~ Amount,
            TRUE                                                                    ~ 0
        )
    ]
}

addNetBalances_ <- function(google_sheet) {
    google_sheet[,  `:=`(
        `NikiCica Net Balance` = `NikiCica Paid by Share` - `NikiCica Paid for Share`,
        `TomiMaci Net Balance` = `TomiMaci Paid by Share` - `TomiMaci Paid for Share`
    )]
}

selectRelevantColumns <- function(google_sheet) {
    relevant_cols <- c(
        "Date", "Category", "Subcategory", "Currency", "Amount",
        "Paid for", "Paid by", "NikiCica Net Balance", "TomiMaci Net Balance"
    )
    google_sheet[, relevant_cols, with = FALSE]
}
