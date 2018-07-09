#' Select atts and direction
#'
#' This function not only select attributes and rename them, but also select
#' records whose direction is "DuanMian". So the nrow will shrink.

select_atts <- function(df){
  df <- df[df[["行驶方向"]]=="断面",c("观测站编号","观测站名称","观测里程",
                                "经度","纬度","省级名称","地市名称","区县名称",
                                "行驶方向","客车当量","货车当量","机动车当量",
                                "机动车速度","拥挤度")]
  names(df) <- c("index","label","mileage","lng","lat","province","city","county",
                 "direction","passenger_cars","freight_cars","cars","speed","crowd")
  return(df)
}
