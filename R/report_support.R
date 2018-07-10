#' @export
merge_outcome <- function(now,previous,last,bywhat){
  x <- merge(now,previous,by=bywhat)
  x <- merge(x,last,by=bywhat)
  names(x)[2:4] <- c("now","previous","last")
  return(x)
}
