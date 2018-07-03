caculate_yearratio <- function(nowdf,thendf){
  x <- nowdf
  for (i in 2:length(x)) {
    x[[i]] <- nowdf[[i]]/thendf[[i]]
  }
  return(x)
}
