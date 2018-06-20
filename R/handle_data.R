#' @export
handle_extractindex <- function(inputpath){
  df <- read.csv(inputpath)
  df_index <- df[,2]
  return(df_index)
}

#' @export
handle_selectindex <- function(database,samplename){
  newdf <- database[database[,1] %in% sample_base[[samplename]],]
  return(newdf)
}

#' @export
handle_splitatts <- function(dfatts,attname,startpoint){
  x <- lapply(station_atts, stringr::str_subset,attname)
  x <- lapply(x, stringr::str_sub,startpoint,-2)
  x <- as.matrix(x)
  x <- as.data.frame(x)
  x$V1 <- as.character(x$V1)
  x$index <- rownames(x)
  x <- x[x$V1!="character(0)",]
  return(x)
}
