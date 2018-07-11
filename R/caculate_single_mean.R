#' @export
caculate_carsmean <- function(jd, attsname, na.rm = T) {
    x_wmean <- plyr::ddply(jd, attsname, plyr::summarise, Wmean = weighted.mean(cars, w = mileage))
    x_wmean <- x_wmean[order(x_wmean$Wmean), ]
    if (na.rm == T) {
        x_wmean <- na.omit(x_wmean)
    }
    rownames(x_wmean) <- 1:nrow(x_wmean)
    return(x_wmean)
}

#' @export
caculate_passcarsmean <- function(jd, attsname, na.rm = T) {
    x_wmean <- plyr::ddply(jd, attsname, plyr::summarise, Wmean = weighted.mean(passenger_cars, w = mileage))
    x_wmean <- x_wmean[order(x_wmean$Wmean), ]
    if (na.rm == T) {
        x_wmean <- na.omit(x_wmean)
    }
    rownames(x_wmean) <- 1:nrow(x_wmean)
    return(x_wmean)
}

#' @export
caculate_frecarsmean <- function(jd, attsname, na.rm = T) {
    x_wmean <- plyr::ddply(jd, attsname, plyr::summarise, Wmean = weighted.mean(freight_cars, w = mileage))
    x_wmean <- x_wmean[order(x_wmean$Wmean), ]
    if (na.rm == T) {
        x_wmean <- na.omit(x_wmean)
    }
    rownames(x_wmean) <- 1:nrow(x_wmean)
    return(x_wmean)
}
