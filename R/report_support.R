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
merge_outcome <- function(now,previous=FALSE,last=FALSE,bywhat){
  if(is.logical(previous) == TRUE){
    x <- merge(now,last,by=bywhat)
    names(x)[2:3] <- c("now","last")
  } else if(is.logical(last) == TRUE){
    x <- merge(now,previous,by=bywhat)
    names(x)[2:3] <- c("now","previous")
  } else{
    x <- merge(now,previous,by=bywhat)
    x <- merge(x,last,by=bywhat)
    names(x)[2:4] <- c("now","previous","last")
  }
  return(x)
}

#' @export
typicalroute_horizon <- function(jdnews, jdprevious, routename) {
  x <- subset(jdnews,horizon10 == routename)
  xnow <- caculate_carsmean(x,"province")
  y <- subset(jdprevious,horizon10 == routename)
  ylast <- caculate_carsmean(y,"province")
  yratio <- caculate_increaseratio(xnow,ylast)
  t <- merge_outcome(xnow,previous=yratio,bywhat = "province")
  return(t)
}

#' @export
typicalroute_vertical <- function(jdnews, jdprevious, routename) {
  x <- subset(jdnews,vertical10 == routename)
  xnow <- caculate_carsmean(x,"province")
  y <- subset(jdprevious,vertical10 == routename)
  ylast <- caculate_carsmean(y,"province")
  yratio <- caculate_increaseratio(xnow,ylast)
  t <- merge_outcome(xnow,previous=yratio,bywhat = "province")
  return(t)
}

#' @export
data_use <- function(jd){
  t <- table(jd[["province"]],jd[["level"]])
  t <- data.frame(t)
  t <- dcast(t,Var1~Var2)
  t$Var1 <- factor(t$Var1,ordered = T,levels = province_level)
  rownames(t) <- t$Var1
  t <- t[order(t$Var1),-1]
  s <- rbind(t,colSums(t))
  p <- colSums(t)/sum(colSums(t))
  return(list(s,p))
}
