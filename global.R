source('libraries.R')
invisible(sapply(list.files("R", full.names = TRUE), source))

# googlesheets::gs_auth(new_user = TRUE)
googlesheets::gs_auth(token = Sys.getenv("GS_TOKEN"))
