#' @export
load_raw <- function(filename){
  pathname <- paste("D:\\R\\packages\\Mreport\\data-raw\\",
                    filename,".Rdata",sep="")
  load(pathname,.GlobalEnv)
}

#' @export
load_base <- function(){
  load_raw("station_simple")
  load_raw("station_useful")
  load_raw("station_plot")
  load_raw("line")
}

#' @export
load_sample_base <- function(){
  load_raw("sample_base")
}
