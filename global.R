invisible(suppressPackageStartupMessages(lapply(
    trimws(readLines("packages.txt")),
    function(x) library(x, character.only = TRUE)
)))

invisible(sapply(list.files("R", full.names = TRUE), source))
