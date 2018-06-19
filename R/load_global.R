load_data <- function(filename){
  pathname <- paste("D:\\R\\packages\\Mreport\\data-raw\\",
                    filename,".Rdata",sep="")
  load(pathname,.GlobalEnv)
}

load_base <- function(){
  load_data("station_simple")
  load_data("station_useful")
  load_data("line")
}
