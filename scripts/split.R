split_day <- function(df){
  df$日 <- lubridate::day(df$观测日期)
  return(df)
}

split_ym <- function(df){
  df$观测年月 <- paste(df$年份,df$月份,sep = "-")
  return(df)
}

split_md <- function(df){
  df$月 <- lubridate::month(df$观测日期)
  df$日 <- lubridate::day(df$观测日期)
  df$月日 <- paste(df$月,df$日,sep="-")
  return(df)
}
