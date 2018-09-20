split_day <- function(df){
  df$日 <- lubridate::day(df$观测日期)
  return(df)
}

split_ym <- function(df){
  df$观测年月 <- paste(df$年份,df$月份,sep = "-")
  return(df)
}
