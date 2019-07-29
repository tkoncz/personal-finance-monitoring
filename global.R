source('libraries.R')
invisible(sapply(list.files("R", full.names = TRUE), source))

gs_auth(token = Sys.getenv("GS_TOKEN"))