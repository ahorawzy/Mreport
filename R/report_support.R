#' Merge 3 columns of outcome
#'
#' This function can merge 3 column-like dataframes into 1 dataframe with 3 columns.
#'
#' During report writing, merging now-value, previous-value, last-value into 1 dataframe is quite
#' useful and concise. This function receive 3 column-like dataframes. Each column-like dataframes
#' has two columns, one is "theme" like "citygroup" or "portroad". These 3 dataframes has the same theme.
#' The other is Wmean. It's probably caculated by function caculate_carsmean or caculate_increaseratio.
#'
#' @param now The report month, like 2018/06.
#' @param previous One year ago of report month, like 2017/06. Or caculated by function caculate_increaseratio.
#' @param last One month ago of report month, like 2018/05. Or caculated by function caculate_increaseratio.
#' @param bywhat The theme name, like "citygroup" or "portroad".
#'
#' @export
merge_outcome <- function(now,previous,last,bywhat){
  x <- merge(now,previous,by=bywhat)
  x <- merge(x,last,by=bywhat)
  names(x)[2:4] <- c("now","previous","last")
  return(x)
}
