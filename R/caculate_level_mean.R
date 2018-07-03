#' @export
caculate_level_carsmean <- function(jd,attsname,na.rm = T){
  x_wmean <- plyr::ddply(jd,c(attsname,"level"),plyr::summarise,
                         Wmean = weighted.mean(cars,w=mileage))
  if (na.rm == T) {
    x_wmean <- na.omit(x_wmean)
  }
  x_wmean <- reshape2::dcast(x_wmean,x_wmean[[attsname]]~x_wmean[["level"]])
  names(x_wmean)[1] <- attsname
  return(x_wmean)
}

#' @export
caculate_level_passcarsmean <- function(jd,attsname,na.rm = T){
  x_wmean <- plyr::ddply(jd,c(attsname,"level"),plyr::summarise,
                         Wmean = weighted.mean(passenger_cars,w=mileage))
  if (na.rm == T) {
    x_wmean <- na.omit(x_wmean)
  }
  x_wmean <- reshape2::dcast(x_wmean,x_wmean[[attsname]]~x_wmean[["level"]])
  names(x_wmean)[1] <- attsname
  return(x_wmean)
}

#' @export
caculate_level_frecarsmean <- function(jd,attsname,na.rm = T){
  x_wmean <- plyr::ddply(jd,c(attsname,"level"),plyr::summarise,
                         Wmean = weighted.mean(freight_cars,w=mileage))
  if (na.rm == T) {
    x_wmean <- na.omit(x_wmean)
  }
  x_wmean <- reshape2::dcast(x_wmean,x_wmean[[attsname]]~x_wmean[["level"]])
  names(x_wmean)[1] <- attsname
  return(x_wmean)
}
