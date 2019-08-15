#'Read a single file of data
#'
#'Mostly for internal use
#'@param location The path of data to use (relative or absolute)
#'@name ss_read
#'@export

ss_read <- function(location){
  #require(magrittr)
  #require(tidyverse)
  suppressMessages(dat <- tryCatch({
    readr::read_delim(location,delim="\t",col_names = c("Time","Raw_Voltage","Ref","DeltaA"))
  },error=function(e){
    readr::read_delim(location,delim = "\t",col_names = F)
  }))
  return(dplyr::mutate_all(dat,as.numeric))
}
