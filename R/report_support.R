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
merge_outcome <- function(now,previous=NULL,last=NULL,bywhat){
  if(is.null(previous) == TRUE){
    x <- merge(now,last,by=bywhat)
    names(x)[2:3] <- c("now","last")
  } else if(is.null(last) == TRUE){
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

#'
#' Caculate now_wmean previous_increaseratio and last_increaseratio
#'
#' This function can caculate 3 components in one time by given attsname and carstype.
#'
#' This function serves for report wirting. It's very common that combines now_wmean,
#' previous_increaseratio and last_increaseratio in one dataframe. This function
#' is a combination machine which combines most important functions into one.
#'
#' It's the most useful function in this package.
#'
#' @param newsjd The jd dataframe which used to be jdnews.
#' @param previoussjd The jd dataframe which used to be jdpreviouss.
#' @param lastsjd The jd dataframe which used to be jdlasts.
#' @param attsname A string that you want to analysis, for example "citygroup2".
#' @param carstype A string must be one from "cars", "passcars", "frecars".
#' @return A dataframe consists of 3 columns, including now_wmean, previous_increaseratio, last_increaseratio.
#'
#' @export
result_present3 <- function(newsjd,previoussjd,lastsjd,attsname,carstype){
  if(carstype == "cars"){
    newcars <- caculate_carsmean(newsjd,attsname)
    previouscars <- caculate_carsmean(previoussjd,attsname)
    x <- caculate_increaseratio(newcars,previouscars)
    lastcars <- caculate_carsmean(lastsjd,attsname)
    y <- caculate_increaseratio(newcars,lastcars)
    z <- merge_outcome(newcars,x,y,bywhat=attsname)
  } else if (carstype == "passcars"){
    newcars <- caculate_passcarsmean(newsjd,attsname)
    previouscars <- caculate_passcarsmean(previoussjd,attsname)
    x <- caculate_increaseratio(newcars,previouscars)
    lastcars <- caculate_passcarsmean(lastsjd,attsname)
    y <- caculate_increaseratio(newcars,lastcars)
    z <- merge_outcome(newcars,x,y,bywhat=attsname)
  } else if (carstype == "frecars"){
    newcars <- caculate_frecarsmean(newsjd,attsname)
    previouscars <- caculate_frecarsmean(previoussjd,attsname)
    x <- caculate_increaseratio(newcars,previouscars)
    lastcars <- caculate_frecarsmean(lastsjd,attsname)
    y <- caculate_increaseratio(newcars,lastcars)
    z <- merge_outcome(newcars,x,y,bywhat=attsname)
  } else{
    stop("Wrong carstype!")
  }
  return(z)
}

#' @export
result_present2 <- function(newsjd,previoussjd,attsname,carstype){
  if(carstype == "cars"){
    x <- caculate_carsmean(newsjd,attsname)
    y <- caculate_carsmean(previoussjd,attsname)
    z <- caculate_increaseratio(x,y)
    w <- merge(x,z,by=attsname)
  } else if(carstype == "passcars"){
    x <- caculate_passcarsmean(newsjd,attsname)
    y <- caculate_passcarsmean(previoussjd,attsname)
    z <- caculate_increaseratio(x,y)
    w <- merge(x,z,by=attsname)
  } else if(carstype == "frecars"){
    x <- caculate_frecarsmean(newsjd,attsname)
    y <- caculate_frecarsmean(previoussjd,attsname)
    z <- caculate_increaseratio(x,y)
    w <- merge(x,z,by=attsname)
  } else{
    stop("Wrong carstype!")
  }
  names(w) <- c(attsname,"now","ratio")
  return(w)
}
