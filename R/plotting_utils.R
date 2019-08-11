getColorScale <- function(n = 5) {
    colors <- c(
        "blue1" = "#4575b4",
        "red" = "#d73027",
        "blue2" = "#74add1",
        "orange1" = "#f46d43",
        "lightblue1" = "#abd9e9",
        "orange2" = "#fdae61",
        "lightblue2" = "#e0f3f8",
        "yellow1" = "#fee090",
        "yellow2" = "#ffffbf"
    )

    colors[1:n]
}
