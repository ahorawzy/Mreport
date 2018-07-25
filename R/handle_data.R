#' Split attributes from station_atts
#'
#' This function helps split attributes from station_atts.Rdata
#'
#' File "station_atts.Rdata" is handled by strsplit function from the big-table with all station-attributes.
#' "station_atts" is a big list. Each element is a station. This function receives attname and startpoint of "("
#' to extract the content of attributes.
#'
#' @param stationatts The list: station_atts.
#' @param attname The attribute you want to split, like character "Cheng Shi Qun".
#' @param startpoint The startpoint of the first str behind "(".
#' @return A dataframe contains 2 columns, "V1" save attributes content, "index" save station_index.
#'
#' @export
handle_splitatts <- function(stationatts, attname, startpoint) {
    x <- lapply(station_atts, stringr::str_subset, attname)
    x <- lapply(x, stringr::str_sub, startpoint, -2)
    x <- as.matrix(x)
    x <- as.data.frame(x)
    x$V1 <- as.character(x$V1)
    x$index <- rownames(x)
    x <- x[x$V1 != "character(0)", ]
    return(x)
}

#' Merge samplebase information and stationplot data.
#'
#' This function can merge one attribute from "samplebase" list with "station_plot" dataframe.
#'
#' This function aims to quickly plot points samplebase. It can be used before geo_pointplot function.
#' You can x <- handle_mergeplot(samplebase$citygroup2,station_plot), and then geo_pointplot(x,na.rm=T,type=T).
#' The attribute information contains in "type" column.
#'
#' @param samplebase One element of list sample_base, like "sample_base$citygroup2".
#' @param stationplot The dataframe station_plot.
#' @return A dataframe contains station_plot information and attribute information, which can be directly used
#' by geo_pointplot().
#'
#' @export
handle_mergeplot <- function(samplebase, stationplot) {
    x <- merge(samplebase, stationplot, by.x = "index", by.y = "popup", all.x = T)
    names(x)[1:2] <- c("popup", "type")
    return(x)
}

#' @export
handle_mergeline <- function(jd, stationline) {
    df <- merge(jd, stationline, by = "index")
    return(df)
}

#' @export
handle_mergesample <- function(jd, samplebase) {
    samplenames <- names(samplebase)
    for (i in samplenames) {
        jd <- merge(jd, sample_base[[i]], by = "index", all.x = TRUE)
        names(jd)[length(jd)] <- i
    }
    return(jd)
}

#' @export
handle_gather <- function(jd){
  jd <- caculate_equivalent(jd)
  jd <- select_atts(jd)
  jd <- handle_mergeline(jd,station_line)
  jd <- handle_mergesample(jd,sample_base)
  jd <- merge(jd,roadlevel,by="index",all.x = T)
  jd <- subset(jd,index %in% station_use)
}
