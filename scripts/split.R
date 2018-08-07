split_day <- function(df){
  df$日 <- lubridate::day(df$观测日期)
  return(df)
}
