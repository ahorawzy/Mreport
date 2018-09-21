#' @export
caculate_travel_volume <- function(section_volume,mileage,atts){
  l <- split(section_volume,section_volume[[atts]])
  m <- split(mileage,mileage[[atts]])
  l2 <- lapply(l, `[`, 3:6)
  m2 <- lapply(m, `[`, 2:5)
  k2 <- list(length = length(l2))
  for (i in 1:length(l2)) {
    k2[[i]] <- l2[[i]] * matrix(as.numeric(m2[[names(l2[1])]]),nrow = nrow(l2[[i]]),ncol = 4,byrow=T)
  }
  names(k2) <- names(l2)
  k3 <- lapply(k2, rowSums, na.rm = T)
  for(i in 1:length(k3)){
    names(k3[[i]]) <- l[[1]][[1]]
  }
  return(data.frame(k3))
}
