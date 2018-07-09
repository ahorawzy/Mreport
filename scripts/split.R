split_day <- function(df){
  x <- strsplit(df$观测日期,split = "-")
  y <- sapply(x,'[',3)
  df$日 <- y
  return(df)
}
