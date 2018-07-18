#' @export
caculate_developratio <- function(nowdf, thendf) {
    nlen <- ncol(nowdf)
    attname <- names(nowdf)[1]
    x <- merge(nowdf, thendf, by = attname, all.x = TRUE)
    out <- x[1:nlen]
    for (i in 2:nlen) {
        out[i] <- x[i]/x[i + nlen - 1]
    }
    return(out)
}

#' @export
caculate_increaseratio <- function(nowdf, thendf) {
    nlen <- ncol(nowdf)
    attname <- names(nowdf)[1]
    x <- merge(nowdf, thendf, by = attname, all.x = TRUE)
    out <- x[1:nlen]
    for (i in 2:nlen) {
        out[i] <- ((x[i] - x[i + nlen - 1])/x[i + nlen - 1]) * 100
        out[i] <- round(out[i], 1)
        if (nlen == 2) {
            out <- out[order(out[2]), ]
        }
        out[i] <- apply(out[i], 1, as.character)
        out[i] <- apply(out[i], 1, paste, "%", sep = "")
    }
    return(out)
}
