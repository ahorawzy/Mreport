caculate_yearratio <- function(nowdf,thendf){
  nlen <- ncol(nowdf)
  attname <- names(nowdf)[1]
  x <- merge(nowdf,thendf,by=attname,all.x=TRUE)
  out <- x[1:nlen]
  for (i in 2:nlen) {
    out[i] <- x[i]/x[i+nlen-1]
  }
  return(out)
}
