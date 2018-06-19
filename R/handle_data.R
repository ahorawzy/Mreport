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
