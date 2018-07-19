#' @export
load_raw <- function(filename) {
    pathname <- paste("D:\\R\\packages\\Mreport\\data-raw\\", filename, ".Rdata", sep = "")
    load(pathname, .GlobalEnv)
}

#' @export
load_base <- function() {
    load_raw("station_simple")
    load_raw("station_useful")
    load_raw("station_plot")
    load_raw("station_line")
    load_raw("station_use")
    load_raw("line")
    load_raw("roadlevel")
    load_raw("province_level")
}

#' @export
load_sample_base <- function() {
    load_raw("sample_base")
}
