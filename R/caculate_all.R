#' @export
caculate_all_cars <- function(jd){
  return(weighted.mean(jd$cars,jd$mileage))
}

#' @export
caculate_all_passcars <- function(jd){
  return(weighted.mean(jd$passenger_cars,jd$mileage))
}

#' @export
caculate_all_frecars <- function(jd){
  return(weighted.mean(jd$freight_cars,jd$mileage))
}
