caculate_equivalent <- function(df){
  df[["客车当量"]] <- 1.0*df[["中小客流量"]] + 1.5*df[["大客车流量"]]
  df[["货车当量"]] <- 1.0*df[["小货车流量"]] + 1.5*df[["中货车流量"]] +
    3.0*df[["大货车流量"]] + 4.0*df[["特大货流量"]] + 4.0*df[["集装箱流量"]]
  return(df)
}
