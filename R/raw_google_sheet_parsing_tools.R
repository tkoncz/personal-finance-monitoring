loadGoogleSheet <- function(google_sheet_name) {
    loadRawGoogleSheet(google_sheet_name) %>%
        fixDateFormat_() %>%
        addSubCategory_() %>%
        fixOtherCurrency_()
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

prepareTableForRawData <- function(google_sheet) {
    copy(google_sheet) %>%
        addShareAmounts_() %>%
        addNetBalances_() %>%
        selectRelevantColumns()
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

adjustForPaidForByBoth <- function(google_sheet) {
    row_count <- google_sheet[, .N]
    map(1:row_count, ~{
        current_row <- google_sheet[.x]
        if (current_row[, `Paid for`] == "NikiCica, TomiMaci") {
            current_row_corrected_paid_for <- rbind(
                copy(current_row)[, `:=`(`Paid for` = "NikiCica", Amount = Amount / 2)],
                copy(current_row)[, `:=`(`Paid for` = "TomiMaci", Amount = Amount / 2)]
            )
        } else {
            current_row_corrected_paid_for <- current_row
        }

        if (unique(current_row_corrected_paid_for[, `Paid by`]) == "NikiCica, TomiMaci") {
            current_row_corrected_paid_for_paid_by <- rbind(
                copy(current_row_corrected_paid_for)[,
                    `:=`(`Paid by` = "NikiCica", Amount = Amount / 2)
                ],
                copy(current_row_corrected_paid_for)[,
                    `:=`(`Paid by` = "TomiMaci", Amount = Amount / 2)
                ]
            )
        } else {
            current_row_corrected_paid_for_paid_by <- current_row_corrected_paid_for
        }

        current_row_corrected_paid_for_paid_by
    }) %>% rbindlist()
}
