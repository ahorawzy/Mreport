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
  load_raw("line")
}
