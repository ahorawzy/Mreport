#' @export
save_raw <- function(filename){
  pathname <- paste("D:\\R\\packages\\Mreport\\data-raw\\",
                    substitute(filename),".Rdata",sep="")
  save(filename,file=pathname)
}
