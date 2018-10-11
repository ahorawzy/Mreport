#' Select atts and direction for month-data
#'
#' This function not only select attributes and rename them, but also select
#' records whose direction is "DuanMian". So the nrow will shrink.
#'
select_atts <- function(df){
  df <- df[df[["行驶方向"]]=="断面",c("观测站编号","观测站名称","观测里程",
                                "经度","纬度","省级名称","地市名称","区县名称",
                                "行驶方向","客车当量","货车当量","机动车当量",
                                "机动车速度","拥挤度")]
  names(df) <- c("index","label","mileage","lng","lat","province","city","county",
                 "direction","passenger_cars","freight_cars","cars","speed","crowd")
  return(df)
}

#' Select atts and direction for day-data
select_atts_forday <- function(df){
  df <- df[df[["行驶方向"]]=="断面",c("观测站编号","观测站名称","观测里程",
                                "经度","纬度","省级名称","地市名称","区县名称",
                                "行驶方向","客车当量","货车当量","机动车当量",
                                "机动车速度","拥挤度","日")]
  names(df) <- c("index","label","mileage","lng","lat","province","city","county",
                 "direction","passenger_cars","freight_cars","cars","speed","crowd",
                 "day")
  return(df)
}

select_atts_forym <- function(df){
  df <- df[df[["行驶方向"]]=="断面",c("观测站编号","观测站名称","观测里程",
                                "经度","纬度","省级名称","地市名称","区县名称",
                                "行驶方向","客车当量","货车当量","机动车当量",
                                "机动车速度","拥挤度","观测年月")]
  names(df) <- c("index","label","mileage","lng","lat","province","city","county",
                 "direction","passenger_cars","freight_cars","cars","speed","crowd",
                 "ym")
  return(df)
}

select_atts_formd <- function(df){
  df <- df[df[["行驶方向"]]=="断面",c("观测站编号","观测站名称","观测里程",
                                "经度","纬度","省级名称","地市名称","区县名称",
                                "行驶方向","客车当量","货车当量","机动车当量",
                                "机动车速度","拥挤度","月日")]
  names(df) <- c("index","label","mileage","lng","lat","province","city","county",
                 "direction","passenger_cars","freight_cars","cars","speed","crowd",
                 "md")
  return(df)
}

select_atts_formd_line <- function(df){
  df <- df[df[["行驶方向"]]=="断面",c("观测站编号","观测站名称","观测里程",
                                "经度","纬度","省级名称","地市名称","区县名称",
                                "行驶方向","客车当量","货车当量","机动车当量",
                                "机动车速度","拥挤度","月日","路线简称","路线业务编号")]
  names(df) <- c("index","label","mileage","lng","lat","province","city","county",
                 "direction","passenger_cars","freight_cars","cars","speed","crowd",
                 "md","linename","lineindex")
  return(df)
}
